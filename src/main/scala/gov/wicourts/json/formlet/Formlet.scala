package gov.wicourts.json.formlet

import cats.Applicative
import cats.Bifunctor
import cats.Contravariant
import cats.FlatMap
import cats.Functor
import cats.Monad
import cats.Monoid
import cats.Semigroup
import cats.Traverse
import cats.data.EitherT
import cats.data.Validated
import cats.evidence.<~<
import cats.syntax.all._

import scala.language.higherKinds

import Predef.<:<
import Predef.identity

case class Formlet[M[_], N[_, _], I, V, E, A](run: I => M[(N[E, A], V)]) {

  def eval(i: I)(implicit M: Functor[M]): M[N[E, A]] =
    M.map(run(i))(_._1)

  def view(i: I)(implicit M: Functor[M]): M[V] =
    M.map(run(i))(_._2)

  def bimap[B, C](
    f: E => B,
    g: A => C,
  )(implicit M: Functor[M], N: Bifunctor[N]): Formlet[M, N, I, V, B, C] =
    mapResult((result, view) => (result.bimap(f, g), view))

  def leftMap[B](f: E => B)(implicit M: Functor[M], N: Bifunctor[N]): Formlet[M, N, I, V, B, A] =
    bimap(f, identity)

  def map[B](f: A => B)(implicit M: Functor[M], N: Functor[N[E, ?]]): Formlet[M, N, I, V, E, B] =
    mapResult((result, view) => (result.map(f), view))

  def mapK[G[_], VV, EE, AA](
    f: M[(N[E, A], V)] => G[(N[EE, AA], VV)],
  ): Formlet[G, N, I, VV, EE, AA] =
    Formlet(c => f(run(c)))

  def mapK_[G[_], B](
    f: M[(N[E, A], V)] => G[(N[E, B], V)],
  ): Formlet[G, N, I, V, E, B] =
    mapK(f)

  def ap[B](
    f: => Formlet[M, N, I, V, E, A => B],
  )(
    implicit V: Monoid[V],
    M: Applicative[M],
    N: Applicative[N[E, ?]],
  ): Formlet[M, N, I, V, E, B] =
    Formlet(c =>
      M.map2(this.run(c), f.run(c)) {
        case ((a, v1), (ff, v2)) =>
          (ff <*> a, v1 |+| v2)
      },
    )

  def mapView[U](f: V => U)(implicit M: Functor[M]): Formlet[M, N, I, U, E, A] =
    mapResult((result, view) => (result, f(view)))

  def mapResult[EE, AA, W](
    f: (N[E, A], V) => (N[EE, AA], W),
  )(
    implicit M: Functor[M],
  ): Formlet[M, N, I, W, EE, AA] =
    Formlet(c => M.map(run(c))(f.tupled))

  def mapResultM[EE, AA, W](
    f: (N[E, A], V) => M[(N[EE, AA], W)],
  )(
    implicit M: FlatMap[M],
  ): Formlet[M, N, I, W, EE, AA] =
    Formlet(c => M.flatMap(run(c))(f.tupled))

  def mapM[B](
    f: A => M[B],
  )(
    implicit M: Monad[M],
    N: Traverse[N[E, ?]],
  ): Formlet[M, N, I, V, E, B] =
    mapResultM { case (n, v) => n.traverse(f).map((_, v)) }

  def local[X](f: X => I): Formlet[M, N, X, V, E, A] = Formlet(run compose f)

  def contramap[X](f: X => I): Formlet[M, N, X, V, E, A] = local(f)
}

object Formlet extends FormletSyntax {

  implicit class FormletValidation[M[_], I, V, E, A](val self: Formlet[M, Validated, I, V, E, A])
    extends AnyVal {

    def mapValidation[B](
      f: A => Validated[E, B],
    )(
      implicit M: Functor[M],
    ): Formlet[M, Validated, I, V, E, B] =
      self.mapResult((a, v) => (a.toEither.flatMap(x => f(x).toEither).toValidated, v))

    def validate[B](
      f: A => Validated[E, B],
    )(
      implicit M: Functor[M],
    ): Formlet[M, Validated, I, V, E, B] =
      mapValidation(f)

    def step(implicit M: Functor[M]): Formlet[M, Either, I, V, E, A] =
      Formlet(i => M.map(self.run(i)) { case (r, v) => (r.toEither, v) })

    /**
      * This has the same signature has .flatMap, but is named differently as
      * Formlet is not a Monad.
      */
    def using[B](
      f: A => Formlet[M, Validated, I, V, E, B],
    )(
      implicit M: Monad[M],
      V: Semigroup[V],
    ): Formlet[M, Validated, I, V, E, B] =
      step.flatMap(f(_).step).formlet

    def mapValidationM[B](
      f: A => M[Validated[E, B]],
    )(
      implicit M: Monad[M],
    ): Formlet[M, Validated, I, V, E, B] = {
      self.mapResultM((a, v) =>
        M.map(
          EitherT(M.pure(a.toEither))
            .flatMapF(x => M.map(f(x))(_.toEither))
            .value,
        )(r => (r.toValidated, v)),
      )
    }

    def validateM[B](
      f: A => M[Validated[E, B]],
    )(
      implicit M: Monad[M],
    ): Formlet[M, Validated, I, V, E, B] =
      self.mapValidationM(f)

    def value(implicit M: Functor[M]): I => M[Option[A]] = i => M.map(self.eval(i))(_.toOption)

    def valueOpt[B](implicit M: Functor[M], ev: A <~< Option[B]): I => M[Option[B]] =
      i => M.map(self.eval(i))(v => Monad[Option].flatten(v.toOption.map(ev(_))))

    def validateVM[B, C](
      other: I => M[B],
    )(
      f: (B, A) => M[Validated[E, C]],
    )(
      implicit M: Monad[M],
    ): Formlet[M, Validated, I, V, E, C] =
      Formlet(c => M.flatMap(other(c))(b => validateM(f(b, _)).run(c)))

    def orElse(
      x: => Formlet[M, Validated, I, V, E, A],
    )(
      implicit M: Monad[M],
    ): Formlet[M, Validated, I, V, E, A] =
      Formlet(i =>
        self.run(i).flatMap {
          case (result, view) =>
            result.fold(
              _ => x.run(i),
              j => M.pure((j.valid[E], view)),
            )
        },
      )
  }

  implicit class FormletEither[M[_], I, V, E, A](val self: Formlet[M, Either, I, V, E, A])
    extends AnyVal {

    def flatMap[B](
      f: A => Formlet[M, Either, I, V, E, B],
    )(
      implicit M: Monad[M],
      V: Semigroup[V],
    ): Formlet[M, Either, I, V, E, B] =
      Formlet(i =>
        M.flatMap(self.run(i)) {
          case (Left(ff), v1) => M.pure((ff.asLeft, v1))
          case (Right(a), v1) =>
            M.map(f(a).run(i)) {
              case (r2, v2) => (r2, V.combine(v1, v2))
            }
        },
      )

    def formlet(implicit M: Functor[M]): Formlet[M, Validated, I, V, E, A] =
      Formlet(i => M.map(self.run(i)) { case (r, v) => (r.toValidated, v) })
  }

  implicit def formletContravariant[M[_], N[_, _], V, E, A]
    : Contravariant[Formlet[M, N, ?, V, E, A]] =
    new Contravariant[Formlet[M, N, ?, V, E, A]] {

      def contramap[X, XX](fa: Formlet[M, N, X, V, E, A])(f: XX => X): Formlet[M, N, XX, V, E, A] =
        fa.contramap(f)
    }

  implicit def formletBifunctor[M[_]: Functor, N[_, _]: Bifunctor, I, V]
    : Bifunctor[Formlet[M, N, I, V, ?, ?]] =
    new Bifunctor[Formlet[M, N, I, V, ?, ?]] {

      def bimap[A, B, C, D](
        fab: Formlet[M, N, I, V, A, B],
      )(f: A => C, g: B => D): Formlet[M, N, I, V, C, D] =
        fab.bimap(f, g)
    }

  implicit def formletApplicative[
    M[_]: Applicative,
    N[_, _]: Bifunctor,
    I,
    V: Monoid,
    E: Semigroup,
  ](
    implicit NN: Applicative[N[E, ?]],
  ): Applicative[Formlet[M, N, I, V, E, ?]] =
    new Applicative[Formlet[M, N, I, V, E, ?]] {

      override def map[A, B](a: Formlet[M, N, I, V, E, A])(f: A => B): Formlet[M, N, I, V, E, B] =
        a.map(f)

      override def pure[A](a: A): Formlet[M, N, I, V, E, A] =
        Formlet[M, N, I, V, E, A](_ => Applicative[M].pure((NN.pure(a), Monoid[V].empty)))

      override def ap[A, B](
        f: Formlet[M, N, I, V, E, A => B],
      )(
        fa: Formlet[M, N, I, V, E, A],
      ): Formlet[M, N, I, V, E, B] =
        fa.ap(f)
    }

  implicit def formletEitherMonad[
    M[_]: Monad,
    I,
    V: Monoid,
    E: Semigroup,
  ]: Monad[Formlet[M, Either, I, V, E, ?]] =
    new Monad[Formlet[M, Either, I, V, E, ?]] {

      override def map[A, B](
        a: Formlet[M, Either, I, V, E, A],
      )(f: A => B): Formlet[M, Either, I, V, E, B] =
        a.map(f)

      override def pure[A](a: A): Formlet[M, Either, I, V, E, A] =
        Formlet[M, Either, I, V, E, A](_ => Monad[M].pure((a.asRight, Monoid[V].empty)))

      override def flatMap[A, B](
        fa: Formlet[M, Either, I, V, E, A],
      )(
        f: A => Formlet[M, Either, I, V, E, B],
      ): Formlet[M, Either, I, V, E, B] =
        fa.flatMap(f)

      override def tailRecM[A, B](x: A)(
        f: A => Formlet[M, Either, I, V, E, Either[A, B]],
      ): Formlet[M, Either, I, V, E, B] = {

        Formlet { i =>
          f(x).run(i).flatMap {
            case (Left(err), v) => Monad[M].pure((err.asLeft, v))
            case (Right(Left(a)), v) =>
              Monad[M].tailRecM[(A, V), (Either[E, B], V)]((a, v)) {
                case (aa, vv) =>
                  f(aa).run(i).map {
                    case (Right(Right(r)), fv) =>
                      (r.asRight[E], Monoid[V].combine(vv, fv)).asRight[(A, V)]
                    case (Left(err), fv) => (err.asLeft, Monoid[V].combine(vv, fv)).asRight
                    case (Right(Left(r)), fv) =>
                      (r, Monoid[V].combine(vv, fv)).asLeft
                  }
              }
            case (Right(Right(b)), v) => Monad[M].pure((b.asRight, v))
          }
        }
      }
    }

  def ask[M[_]: Applicative, N[_, _], I, V: Monoid, E](
    implicit NN: Applicative[N[E, ?]],
  ): Formlet[M, N, I, V, E, I] =
    Formlet(i => Applicative[M].pure((NN.pure(i), Monoid[V].empty)))

  def ifM[M[_], I, V, E, A](
    cond: Formlet[M, Validated, I, V, E, Boolean],
    first: Formlet[M, Validated, I, V, E, A],
    second: Formlet[M, Validated, I, V, E, A],
  )(
    implicit M: Monad[M],
  ): Formlet[M, Validated, I, V, E, A] =
    Formlet(i =>
      cond.run(i).flatMap {
        case (result, view) =>
          result.fold(
            e => M.pure((e.invalid[A], view)),
            s => if (s) first.run(i) else second.run(i),
          )
      },
    )

  def pure[M[_]: Applicative, N[_, _], I, V: Monoid, E, A](
    a: => A,
  )(
    implicit NN: Applicative[N[E, ?]],
  ): Formlet[M, N, I, V, E, A] =
    Formlet(_ => Applicative[M].pure((NN.pure(a), Monoid[V].empty)))

  def liftM[M[_]: Applicative, N[_, _], I, V: Monoid, E, A](
    m: M[N[E, A]],
  ): Formlet[M, N, I, V, E, A] =
    Formlet(_ => m.map((_, Monoid[V].empty)))

}

trait FormletSyntax extends FormletSyntax0 {

  implicit class ObjectFormletOps[M[_], A](self: ObjectFormlet[M, A])(implicit M: Functor[M]) {

    def required[B](name: String)(implicit ev: A <:< Option[B]): ObjectFormlet[M, B] =
      Forms.requiredObj(name, self.map(a => a: Option[B]))

    def fromRoot: ObjectFormlet[M, A] = Forms.fromRoot(self)
    def fromParent: ObjectFormlet[M, A] = Forms.fromParent(self)
    def setRoot: ObjectFormlet[M, A] = Forms.setRoot(self)
  }

  implicit class IdFieldFormletOps[A](self: IdFieldFormlet[A]) {
    def obj: IdObjectFormlet[A] = Forms.obj(self)
    def label(s: String): IdFieldFormlet[A] = Forms.label(self, s)
    def errorName(s: String): IdFieldFormlet[A] = Forms.errorName(self, s)

    def required[B](implicit ev: A <:< Option[B]): IdFieldFormlet[B] =
      Forms.required(self.map(a => a: Option[B]))

    def fromRoot: IdFieldFormlet[A] = Forms.fromRoot(self)
    def fromParent: IdFieldFormlet[A] = Forms.fromParent(self)
    def setRoot: IdFieldFormlet[A] = Forms.setRoot(self)
  }
}

trait FormletSyntax0 {

  implicit class FieldFormletOps[M[_], A](self: FieldFormlet[M, A])(implicit M: Functor[M]) {
    def obj: ObjectFormlet[M, A] = Forms.obj[M, A](self)
    def label(s: String): FieldFormlet[M, A] = Forms.label(self, s)
    def errorName(s: String): FieldFormlet[M, A] = Forms.errorName(self, s)

    def required[B](implicit ev: A <:< Option[B]): FieldFormlet[M, B] =
      Forms.required(self.map(a => a: Option[B]))

    def fromRoot: FieldFormlet[M, A] = Forms.fromRoot(self)
    def fromParent: FieldFormlet[M, A] = Forms.fromParent(self)
    def setRoot: FieldFormlet[M, A] = Forms.setRoot(self)
  }
}
