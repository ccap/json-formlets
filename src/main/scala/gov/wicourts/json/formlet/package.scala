package gov.wicourts.json

import argonaut.Cursor
import scala.language.higherKinds
import scalaz.Applicative
import scalaz.Id.Id
import scalaz.NonEmptyList
import scalaz.Validation

package object formlet {

  type JsonFormlet[M[_], V, E, A] = Formlet[M, Option[Cursor], V, E, A]

  type FieldFormlet[M[_], A] = JsonFormlet[M, FieldView, NonEmptyList[String], A]
  type ObjectFormlet[M[_], A] = JsonFormlet[M, JsonObjectBuilder, ValidationErrors, A]

  type IdFieldFormlet[A] = FieldFormlet[Id, A]
  type IdObjectFormlet[A] = ObjectFormlet[Id, A]

  object ObjectFormlet {
    def validationM[M[_]: Applicative, A](
      m: M[Validation[ValidationErrors, A]]
    ): ObjectFormlet[M, A] =
      Formlet.validationM[M, Option[Cursor], JsonObjectBuilder, ValidationErrors, A](m)
  }

  object syntax extends ToFieldFormletOps with ToObjectFormletOps
}
