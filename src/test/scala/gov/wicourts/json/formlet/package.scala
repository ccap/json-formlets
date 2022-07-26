package gov.wicourts.json.formlet

import cats.Apply
import cats.Monad
import cats.Semigroup
import cats.syntax.semigroup._
import org.scalacheck.Gen

package object test {
  object instances extends ScalaCheckInstances
}

trait ScalaCheckInstances {

  implicit val genMonad: Monad[Gen] = new Monad[Gen] {

    override def pure[A](a: A): Gen[A] = Gen.const(a)

    def flatMap[A, B](fa: Gen[A])(f: A => Gen[B]): Gen[B] = fa.flatMap(f)

    def tailRecM[A, B](a: A)(f: (A) ⇒ Gen[Either[A, B]]): Gen[B] =
      f(a).flatMap {
        case Right(b) => pure(b)
        case Left(aa) => tailRecM(aa)(f)
      }
  }

  implicit val genApply: Apply[Gen] = new Apply[Gen] {
    //override def pure[A](a: A): Gen[A] = Gen.const(a)
    override def ap[A, B](ff: Gen[A => B])(fa: Gen[A]): Gen[B] = ff.flatMap(fa.map(_))

    override def map[A, B](fa: Gen[A])(f: A => B): Gen[B] = fa.map(f)
  }

  implicit def genSemigroup[T: Semigroup]: Semigroup[Gen[T]] = new Semigroup[Gen[T]] {
    def combine(g1: Gen[T], g2: Gen[T]): Gen[T] = for { t1 <- g1; t2 <- g2 } yield t1 |+| t2
  }
}
