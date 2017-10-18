package gov.wicourts.json.formlet

import scalaz._
import scalaz.Liskov._
import scalaz.Id.Id

import scalaz.NonEmptyList.nel

import scalaz.std.function._
import scalaz.std.option._
import scalaz.syntax.applicative._
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
    mapResult((result, view) => (result map f, view))

  def mapK[G[_], VV, EE, AA](
    f: M[(Validation[E, A], V)] => G[(Validation[EE, AA], VV)]
  ): Formlet[G, I, VV, EE, AA] = Formlet(c =>
    f(run(c))
  )

  def mapK_[G[_], B](
    f: M[(Validation[E, A], V)] => G[(Validation[E, B], V)]
  ): Formlet[G, I, V, E, B] = mapK(f)

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
  ): Formlet[M, I, V, E, B] = {
    mapResult((a, v) => ((a.disjunction.flatMap(f(_).disjunction)).validation, v))
  }

  def validate[B](
    h: A => Validation[E, B],
    t: (A => Validation[E, B])*
  )(
    implicit E: Semigroup[E], M: Applicative[M]
  ): Formlet[M, I, V, E, B] = {
    val X = Applicative[A => ?].compose[Validation[E, ?]]
    val f = X.sequence(nel(h, IList(t: _*)))
    mapValidation(f).map(_.head)
  }

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
    h: A => M[Validation[E, B]],
    t: (A => M[Validation[E, B]])*
  )(
    implicit E: Semigroup[E], M: Monad[M]
  ): Formlet[M, I, V, E, B] = {
    val X = Applicative[A => ?].compose[M].compose[Validation[E, ?]]
    val f = X.sequence(nel(h, IList(t: _*)))
    mapValidationM(f).map(_.head)
  }

  def value(implicit M: Functor[M]): I => M[Option[A]] = i =>
    M.map(this.eval(i))(_.toOption)

  def valueOpt[B](implicit M: Functor[M], ev: A <~< Option[B]): I => M[Option[B]] = i =>
    M.map(this.eval(i))(v => Monad[Option].join(v.toOption.map(ev(_))))

  def validateVM[B, C](
    other: I => M[B]
  )(
    h: (B, A) => M[Validation[E, C]],
    t: ((B, A) => M[Validation[E, C]])*
  )(
    implicit E: Semigroup[E], M: Monad[M]
  ): Formlet[M, I, V, E, C] = Formlet { c =>
    M.bind(other(c)) { b =>
      val h1 = h(b, _: A)
      val t1 = t.map(f => f(b, _: A))
      validateM(h1, t1: _*).run(c)
    }
  }

  def validateV[B, C](
    other: I => B
  )(
    h: (B, A) => Validation[E, C],
    t: ((B, A) => Validation[E, C])*
  )(
    implicit E: Semigroup[E], M: Applicative[M]
  ): Formlet[M, I, V, E, C] = Formlet { c =>
    val b = other(c)
    val h1 = h(b, _: A)
    val t1 = t.map(f => f(b, _: A))
    validate(h1, t1: _*).run(c)
  }

  def lift[L[_] : Applicative]: Formlet[Lambda[a => L[M[a]]], I, V, E, A] =
    Formlet[Lambda[a => L[M[a]]], I, V, E, A](c => Applicative[L].point(run(c)))

  def liftId[L[_] : Applicative](
    implicit ev: this.type <~< Formlet[Id, I, V, E, A]
  ): Formlet[L, I, V, E, A] =
    Formlet[L, I, V, E, A](c => Applicative[L].point(ev(this).run(c)))

  def local[X](f: X => I): Formlet[M, X, V, E, A] = Formlet(run compose f)

  def contramap[X](f: X => I): Formlet[M, X, V, E, A] = local(f)
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
    E : Semigroup,
    V : Monoid
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
}
