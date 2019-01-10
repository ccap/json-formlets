package gov.wicourts.json.formlet

import scalaz._
import scalaz.Liskov._

import scalaz.std.option._
import scalaz.syntax.monad._
import scalaz.syntax.monoid._
import scalaz.syntax.validation._

import scala.language.higherKinds

import Predef.identity

case class Formlet[M[_], I, V, E, A](run: I => M[(Validation[E, A], V)]) {
  def eval(i: I)(implicit M: Functor[M]): M[Validation[E, A]] =
    M.map(run(i))(_._1)

  def view(i: I)(implicit M: Functor[M]): M[V] =
    M.map(run(i))(_._2)

  def bimap[B, C](f: E => B, g: A => C)(implicit M: Functor[M]): Formlet[M, I, V, B, C] =
    mapResult((result, view) => (result bimap (f, g), view))

  def leftMap[B](f: E => B)(implicit M: Functor[M]): Formlet[M, I, V, B, A] =
    bimap(f, identity)

  def map[B](f: A => B)(implicit M: Functor[M]): Formlet[M, I, V, E, B] =
    bimap(identity, f)

  def mapK[G[_], VV, EE, AA](
    f: M[(Validation[E, A], V)] => G[(Validation[EE, AA], VV)]
  ): Formlet[G, I, VV, EE, AA] =
    Formlet(c => f(run(c)))

  def mapK_[G[_], B](
    f: M[(Validation[E, A], V)] => G[(Validation[E, B], V)]
  ): Formlet[G, I, V, E, B] =
    mapK(f)

  def ap[B](
    f: => Formlet[M, I, V, E, A => B]
  )(
    implicit E: Semigroup[E], V: Monoid[V], M: Applicative[M]
  ): Formlet[M, I, V, E, B] =
    Formlet(c =>
      M.apply2(this.run(c), f.run(c)) { case ((a, v1), (ff, v2)) =>
        (a <*> ff, v1 |+| v2)
      }
    )

  def mapView[U](f: V => U)(implicit M: Functor[M]): Formlet[M, I, U, E, A] =
    mapResult((result, view) => (result, f(view)))

  def mapResult[EE, AA, W](
    f: (Validation[E, A], V) => (Validation[EE, AA], W)
  )(
    implicit M: Functor[M]
  ): Formlet[M, I, W, EE, AA] =
    Formlet(c => M.map(run(c))(f.tupled))

  def mapValidation[B](
    f: A => Validation[E, B]
  )(
    implicit M: Functor[M]
  ): Formlet[M, I, V, E, B] =
    mapResult((a, v) => ((a.disjunction.flatMap(f(_).disjunction)).validation, v))

  def validate[B](
    f: A => Validation[E, B]
  )(
    implicit M: Functor[M]
  ): Formlet[M, I, V, E, B] =
    mapValidation(f)

  def wizard(implicit M: Functor[M]): Wizard[M, I, V, E, A] =
    Wizard(i => M.map(run(i)) { case (r, v) => (r.disjunction, v) })

  /**
    * This has the same signature has .flatMap, but is named differently as
    * Formlet is not a Monad.
    */
  def using[B](
    f: A => Formlet[M, I, V, E, B]
  )(
    implicit M: Monad[M], V: Semigroup[V]
  ): Formlet[M, I, V, E, B] =
    wizard.flatMap(f(_).wizard).formlet

  def mapResultM[EE, AA, W](
    f: (Validation[E, A], V) => M[(Validation[EE, AA], W)]
  )(
    implicit M: Bind[M]
  ): Formlet[M, I, W, EE, AA] =
    Formlet(c => M.bind(run(c))(f.tupled))

  def mapValidationM[B](
    f: A => M[Validation[E, B]]
  )(
    implicit M: Monad[M]
  ): Formlet[M, I, V, E, B] = {
    mapResultM((a, v) =>
      M.map(
        EitherT(M.point(a.disjunction))
          .flatMapF(x => M.map(f(x))(_.disjunction))
          .run
      )(
        r => (r.validation, v)
      )
    )
  }

  def validateM[B](
    f: A => M[Validation[E, B]]
  )(
    implicit M: Monad[M]
  ): Formlet[M, I, V, E, B] =
    mapValidationM(f)

  def value(implicit M: Functor[M]): I => M[Option[A]] = i =>
    M.map(this.eval(i))(_.toOption)

  def valueOpt[B](implicit M: Functor[M], ev: A <~< Option[B]): I => M[Option[B]] = i =>
    M.map(this.eval(i))(v => Monad[Option].join(v.toOption.map(ev(_))))

  def validateVM[B, C](
    other: I => M[B]
  )(
    f: (B, A) => M[Validation[E, C]]
  )(
    implicit M: Monad[M]
  ): Formlet[M, I, V, E, C] =
    Formlet(c =>
      M.bind(other(c))(b => validateM(f(b, _)).run(c))
    )

  def local[X](f: X => I): Formlet[M, X, V, E, A] = Formlet(run compose f)

  def contramap[X](f: X => I): Formlet[M, X, V, E, A] = local(f)

  def orElse(
    x: => Formlet[M, I, V, E, A]
  )(
    implicit M: Monad[M]
  ): Formlet[M, I, V, E, A] =
    Formlet(i =>
      this.run(i).flatMap { case (result, view) =>
        result.fold(
          _ => x.run(i),
          j => M.point((j.success[E], view))
        )
      }
    )
}

object Formlet {
  implicit def formletContravariant[M[_], V, E, A]: Contravariant[Formlet[M, ?, V, E, A]] =
    new Contravariant[Formlet[M, ?, V, E, A]] {
      def contramap[X, XX](fa: Formlet[M, X, V, E, A])(f: XX => X): Formlet[M, XX, V, E, A] =
        fa contramap f
    }

  implicit def formletBifunctor[M[_] : Functor, I, V]: Bifunctor[Formlet[M, I, V, ?, ?]] =
    new Bifunctor[Formlet[M, I, V, ?, ?]] {
      def bimap[A, B, C, D](fab: Formlet[M, I, V, A, B])(f: A => C, g: B => D): Formlet[M, I, V, C, D] =
        fab.bimap(f, g)
    }

  implicit def formletApplicative[
    M[_] : Applicative,
    I,
    V : Monoid,
    E : Semigroup
  ]: Applicative[Formlet[M, I, V, E, ?]] =
    new Applicative[Formlet[M, I, V, E, ?]] {
      override def map[A, B](a: Formlet[M, I, V, E, A])(f: A => B): Formlet[M, I, V, E, B] =
        a map f

      override def point[A](a: => A): Formlet[M, I, V, E, A] =
        Formlet[M, I, V, E, A](_ => Applicative[M].point((a.success, Monoid[V].zero)))

      override def ap[A, B](
        fa: => Formlet[M, I, V, E, A]
      )(
        f: => Formlet[M, I, V, E, A => B]
      ): Formlet[M, I, V, E, B] =
        fa ap f
    }

  def ask[M[_] : Applicative, I, V : Monoid, E]: Formlet[M, I, V, E, I] =
    Formlet(i => Applicative[M].point((i.success, Monoid[V].zero)))

  def ifM[M[_], I, V, E, A](
    cond: Formlet[M, I, V, E, Boolean],
    first: Formlet[M, I, V, E, A],
    second: Formlet[M, I, V, E, A]
  )(implicit M: Monad[M]): Formlet[M, I, V, E, A] =
    Formlet(i =>
      cond.run(i).flatMap { case (result, view) =>
        result.fold(
          e => M.point((e.failure[A], view)),
          s => if (s) first.run(i) else second.run(i)
        )
      }
    )

  def point[M[_] : Applicative, I, V : Monoid, E, A](a: => A): Formlet[M, I, V, E, A] =
    Formlet(_ => Applicative[M].point((a.success[E], Monoid[V].zero)))

  def liftM[M[_]: Applicative, I, V: Monoid, E, A](
    m: M[Validation[E, A]]
  ): Formlet[M, I, V, E, A] =
    Formlet(_ => m.map((_, Monoid[V].zero)))
}
