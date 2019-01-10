package gov.wicourts.json.formlet

import argonaut.Cursor
import scala.language.higherKinds
import scalaz.Applicative
import scalaz.Validation
import scalaz.~>

object ObjectFormlet {

  def apply[M[_], A](
    run: Option[Cursor] => M[(Validation[ValidationErrors, A], JsonObjectBuilder)]
  ): ObjectFormlet[M, A] =
    Formlet(run)

  def transformK[F[_], G[_], A](
    transform: F ~> G
  )(
    f: ObjectFormlet[F, A]
  ): ObjectFormlet[G, A] =
    apply(cursor => transform(f.run(cursor)))

  def liftM[M[_]: Applicative, A](
    m: M[Validation[ValidationErrors, A]]
  ): ObjectFormlet[M, A] =
    Formlet.liftM(m)

  def point[M[_]: Applicative, A](
    a: A
  ): ObjectFormlet[M, A] =
    Formlet.point(a)
}
