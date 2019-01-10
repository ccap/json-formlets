package gov.wicourts.json.formlet

import Predef.identity
import scala.language.higherKinds
import scalaz.-\/
import scalaz.\/
import scalaz.\/-
import scalaz.Bifunctor
import scalaz.Functor
import scalaz.Monad
import scalaz.Monoid
import scalaz.Semigroup
import scalaz.syntax.either._

case class Wizard[M[_], I, V, E, A](run: I => M[(E \/ A, V)]) {
  def bimap[B, C](f: E => B, g: A => C)(implicit M: Functor[M]): Wizard[M, I, V, B, C] =
    mapResult((result, view) => (result bimap (f, g), view))

  def map[B](f: A => B)(implicit M: Functor[M]): Wizard[M, I, V, E, B] =
    bimap(identity, f)

  def leftMap[B](f: E => B)(implicit M: Functor[M]): Wizard[M, I, V, B, A] =
    bimap(f, identity)

  def flatMap[B](
    f: A => Wizard[M, I, V, E, B]
  )(
    implicit M: Monad[M], V: Semigroup[V]
  ): Wizard[M, I, V, E, B] =
    Wizard(i => M.bind(run(i)) {
      case (ff@ -\/(_), v1) => M.point((ff, v1))
      case (\/-(a), v1) =>
        M.map(f(a).run(i)) { case (r2, v2) =>
          (r2, V.append(v1, v2))
        }
    })

  def formlet(implicit M: Functor[M]): Formlet[M, I, V, E, A] =
    Formlet(i => M.map(run(i)) { case (r, v) => (r.validation, v) })

  def mapResult[EE, AA, W](
    f: (E \/ A, V) => (EE \/ AA, W)
  )(
    implicit M: Functor[M]
  ): Wizard[M, I, W, EE, AA] =
    Wizard(i => M.map(run(i))(f.tupled))
}

object Wizard {
  implicit def wizardBifunctor[M[_] : Functor, I, V]: Bifunctor[Wizard[M, I, V, ?, ?]] =
    new Bifunctor[Wizard[M, I, V, ?, ?]] {
      def bimap[A, B, C, D](fab: Wizard[M, I, V, A, B])(f: A => C, g: B => D): Wizard[M, I, V, C, D] =
        fab.bimap(f, g)
    }

  implicit def wizardMonad[
    M[_]: Monad,
    I,
    V: Monoid,
    E: Semigroup
  ]: Monad[Wizard[M, I, V, E, ?]] =
    new Monad[Wizard[M, I, V, E, ?]] {
      override def map[A, B](a: Wizard[M, I, V, E, A])(f: A => B): Wizard[M, I, V, E, B] =
        a.map(f)

      override def point[A](a: => A): Wizard[M, I, V, E, A] =
        Wizard[M, I, V, E, A](_ => Monad[M].point((a.right, Monoid[V].zero)))

      override def bind[A, B](
        fa: Wizard[M, I, V, E, A]
      )(
        f: A => Wizard[M, I, V, E, B]
      ): Wizard[M, I, V, E, B] =
        fa.flatMap(f)
    }
}
