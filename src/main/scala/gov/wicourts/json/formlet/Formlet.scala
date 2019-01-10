package gov.wicourts.json.formlet

import scalaz._
import scalaz.Liskov._

import scalaz.std.option._
import scalaz.syntax.bifunctor._
import scalaz.syntax.either._
import scalaz.syntax.monad._
import scalaz.syntax.monoid._
import scalaz.syntax.validation._

import scala.language.higherKinds

import Predef.identity

case class Formlet[M[_], N[_, _], I, V, E, A](run: I => M[(N[E, A], V)]) {
  def eval(i: I)(implicit M: Functor[M]): M[N[E, A]] =
    M.map(run(i))(_._1)

  def view(i: I)(implicit M: Functor[M]): M[V] =
    M.map(run(i))(_._2)

  def bimap[B, C](f: E => B, g: A => C)(implicit M: Functor[M], N: Bifunctor[N]): Formlet[M, N, I, V, B, C] =
    mapResult((result, view) => (result bimap (f, g), view))

  def leftMap[B](f: E => B)(implicit M: Functor[M], N: Bifunctor[N]): Formlet[M, N, I, V, B, A] =
    bimap(f, identity)

  def map[B](f: A => B)(implicit M: Functor[M], N: Bifunctor[N]): Formlet[M, N, I, V, E, B] =
    bimap(identity, f)

  def mapK[G[_], VV, EE, AA](
    f: M[(N[E, A], V)] => G[(N[EE, AA], VV)]
  ): Formlet[G, N, I, VV, EE, AA] =
    Formlet(c => f(run(c)))

  def mapK_[G[_], B](
    f: M[(N[E, A], V)] => G[(N[E, B], V)]
  ): Formlet[G, N, I, V, E, B] =
    mapK(f)

  def ap[B](
    f: => Formlet[M, N, I, V, E, A => B]
  )(
    implicit V: Monoid[V], M: Applicative[M], N: Applicative[N[E, ?]]
  ): Formlet[M, N, I, V, E, B] =
    Formlet(c =>
      M.apply2(this.run(c), f.run(c)) { case ((a, v1), (ff, v2)) =>
        (a <*> ff, v1 |+| v2)
      }
    )

  def mapView[U](f: V => U)(implicit M: Functor[M]): Formlet[M, N, I, U, E, A] =
    mapResult((result, view) => (result, f(view)))

  def mapResult[EE, AA, W](
    f: (N[E, A], V) => (N[EE, AA], W)
  )(
    implicit M: Functor[M]
  ): Formlet[M, N, I, W, EE, AA] =
    Formlet(c => M.map(run(c))(f.tupled))

  def mapResultM[EE, AA, W](
    f: (N[E, A], V) => M[(N[EE, AA], W)]
  )(
    implicit M: Bind[M]
  ): Formlet[M, N, I, W, EE, AA] =
    Formlet(c => M.bind(run(c))(f.tupled))

  def local[X](f: X => I): Formlet[M, N, X, V, E, A] = Formlet(run compose f)

  def contramap[X](f: X => I): Formlet[M, N, X, V, E, A] = local(f)
}

object Formlet {
  implicit class FormletValidation[M[_], I, V, E, A](val self: Formlet[M, Validation, I, V, E, A]) extends AnyVal {
    def mapValidation[B](
      f: A => Validation[E, B]
    )(
      implicit M: Functor[M]
    ): Formlet[M, Validation, I, V, E, B] =
      self.mapResult((a, v) => ((a.disjunction.flatMap(f(_).disjunction)).validation, v))

    def validate[B](
      f: A => Validation[E, B]
    )(
      implicit M: Functor[M]
    ): Formlet[M, Validation, I, V, E, B] =
      mapValidation(f)

    def wizard(implicit M: Functor[M]): Formlet[M, \/, I, V, E, A] =
      Formlet(i => M.map(self.run(i)) { case (r, v) => (r.disjunction, v) })

    /**
      * This has the same signature has .flatMap, but is named differently as
      * Formlet is not a Monad.
      */
    def using[B](
      f: A => Formlet[M, Validation, I, V, E, B]
    )(
      implicit M: Monad[M], V: Semigroup[V]
    ): Formlet[M, Validation, I, V, E, B] =
      wizard.flatMap(f(_).wizard).formlet

    def mapValidationM[B](
      f: A => M[Validation[E, B]]
    )(
      implicit M: Monad[M]
    ): Formlet[M, Validation, I, V, E, B] = {
      self.mapResultM((a, v) =>
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
    ): Formlet[M, Validation, I, V, E, B] =
      self.mapValidationM(f)

    def value(implicit M: Functor[M]): I => M[Option[A]] = i =>
      M.map(self.eval(i))(_.toOption)

    def valueOpt[B](implicit M: Functor[M], ev: A <~< Option[B]): I => M[Option[B]] = i =>
      M.map(self.eval(i))(v => Monad[Option].join(v.toOption.map(ev(_))))

    def validateVM[B, C](
      other: I => M[B]
    )(
      f: (B, A) => M[Validation[E, C]]
    )(
      implicit M: Monad[M]
    ): Formlet[M, Validation, I, V, E, C] =
      Formlet(c =>
        M.bind(other(c))(b => validateM(f(b, _)).run(c))
      )

    def orElse(
      x: => Formlet[M, Validation, I, V, E, A]
    )(
      implicit M: Monad[M]
    ): Formlet[M, Validation, I, V, E, A] =
      Formlet(i =>
        self.run(i).flatMap { case (result, view) =>
          result.fold(
            _ => x.run(i),
            j => M.point((j.success[E], view))
          )
        }
      )
  }

  implicit class FormletDisjunction[M[_], I, V, E, A](val self: Formlet[M, \/, I, V, E, A]) extends AnyVal {
    def flatMap[B](
      f: A => Formlet[M, \/, I, V, E, B]
    )(
      implicit M: Monad[M], V: Semigroup[V]
    ): Formlet[M, \/, I, V, E, B] =
      Formlet(i => M.bind(self.run(i)) {
        case (ff@ -\/(_), v1) => M.point((ff, v1))
        case (\/-(a), v1) =>
          M.map(f(a).run(i)) { case (r2, v2) =>
            (r2, V.append(v1, v2))
          }
      })

    def formlet(implicit M: Functor[M]): Formlet[M, Validation, I, V, E, A] =
      Formlet(i => M.map(self.run(i)) { case (r, v) => (r.validation, v) })
  }

  implicit def formletContravariant[M[_], N[_, _], V, E, A]: Contravariant[Formlet[M, N, ?, V, E, A]] =
    new Contravariant[Formlet[M, N, ?, V, E, A]] {
      def contramap[X, XX](fa: Formlet[M, N, X, V, E, A])(f: XX => X): Formlet[M, N, XX, V, E, A] =
        fa contramap f
    }

  implicit def formletBifunctor[M[_]: Functor, N[_, _]: Bifunctor, I, V]: Bifunctor[Formlet[M, N, I, V, ?, ?]] =
    new Bifunctor[Formlet[M, N, I, V, ?, ?]] {
      def bimap[A, B, C, D](fab: Formlet[M, N, I, V, A, B])(f: A => C, g: B => D): Formlet[M, N, I, V, C, D] =
        fab.bimap(f, g)
    }

  implicit def formletApplicative[
    M[_]: Applicative,
    N[_, _]: Bifunctor,
    I,
    V: Monoid,
    E: Semigroup
  ](
    implicit NN: Applicative[N[E, ?]]
  ): Applicative[Formlet[M, N, I, V, E, ?]] =
    new Applicative[Formlet[M, N, I, V, E, ?]] {
      override def map[A, B](a: Formlet[M, N, I, V, E, A])(f: A => B): Formlet[M, N, I, V, E, B] =
        a map f

      override def point[A](a: => A): Formlet[M, N, I, V, E, A] =
        Formlet[M, N, I, V, E, A](_ => Applicative[M].point((NN.point(a), Monoid[V].zero)))

      override def ap[A, B](
        fa: => Formlet[M, N, I, V, E, A]
      )(
        f: => Formlet[M, N, I, V, E, A => B]
      ): Formlet[M, N, I, V, E, B] =
        fa ap f
    }

  implicit def formletDisjunctionMonad[
    M[_]: Monad,
    I,
    V: Monoid,
    E: Semigroup
  ]: Monad[Formlet[M, \/, I, V, E, ?]] =
    new Monad[Formlet[M, \/, I, V, E, ?]] {
      override def map[A, B](a: Formlet[M, \/, I, V, E, A])(f: A => B): Formlet[M, \/, I, V, E, B] =
        a.map(f)

      override def point[A](a: => A): Formlet[M, \/, I, V, E, A] =
        Formlet[M, \/, I, V, E, A](_ => Monad[M].point((a.right, Monoid[V].zero)))

      override def bind[A, B](
        fa: Formlet[M, \/, I, V, E, A]
      )(
        f: A => Formlet[M, \/, I, V, E, B]
      ): Formlet[M, \/, I, V, E, B] =
        fa.flatMap(f)
    }


  def ask[M[_]: Applicative, N[_, _], I, V : Monoid, E](
    implicit NN: Applicative[N[E, ?]]
  ): Formlet[M, N, I, V, E, I] =
    Formlet(i => Applicative[M].point((NN.point(i), Monoid[V].zero)))

  def ifM[M[_], I, V, E, A](
    cond: Formlet[M, Validation, I, V, E, Boolean],
    first: Formlet[M, Validation, I, V, E, A],
    second: Formlet[M, Validation, I, V, E, A]
  )(
    implicit M: Monad[M]
  ): Formlet[M, Validation, I, V, E, A] =
    Formlet(i =>
      cond.run(i).flatMap { case (result, view) =>
        result.fold(
          e => M.point((e.failure[A], view)),
          s => if (s) first.run(i) else second.run(i)
        )
      }
    )

  def point[M[_] : Applicative, N[_, _], I, V : Monoid, E, A](
    a: => A
  )(
    implicit NN: Applicative[N[E, ?]]
  ): Formlet[M, N, I, V, E, A] =
    Formlet(_ => Applicative[M].point((NN.point(a), Monoid[V].zero)))

  def liftM[M[_]: Applicative, N[_, _], I, V: Monoid, E, A](
    m: M[N[E, A]]
  ): Formlet[M, N, I, V, E, A] =
    Formlet(_ => m.map((_, Monoid[V].zero)))
}
