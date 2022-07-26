package gov.wicourts.json.formlet

import argonaut.Cursor
import cats.Applicative
import cats.data.Validated
import cats.~>
import scala.language.higherKinds

object ObjectFormlet {

  def apply[M[_], A](
    run: Option[Cursor] => M[(Validated[ValidationErrors, A], JsonObjectBuilder)],
  ): ObjectFormlet[M, A] =
    Formlet(run)

  def transformK[F[_], G[_], A](
    transform: F ~> G,
  )(
    f: ObjectFormlet[F, A],
  ): ObjectFormlet[G, A] =
    apply(cursor => transform(f.run(cursor)))

  def liftM[M[_]: Applicative, A](
    m: M[Validated[ValidationErrors, A]],
  ): ObjectFormlet[M, A] =
    Formlet.liftM(m)

  def pure[M[_]: Applicative, A](
    a: A,
  ): ObjectFormlet[M, A] =
    Formlet.pure(a)
}
