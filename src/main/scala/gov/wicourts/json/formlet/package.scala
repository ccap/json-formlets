package gov.wicourts.json

import argonaut.Cursor
import scala.language.higherKinds
import scalaz.Id.Id
import scalaz.NonEmptyList
import scalaz.Validation

package object formlet {
  type JsonFormlet[M[_], V, E, A] = Formlet[M, Validation, Option[Cursor], V, E, A]

  type FieldFormlet[M[_], A] = JsonFormlet[M, FieldView, NonEmptyList[String], A]
  type ObjectFormlet[M[_], A] = JsonFormlet[M, JsonObjectBuilder, ValidationErrors, A]

  type IdFieldFormlet[A] = FieldFormlet[Id, A]
  type IdObjectFormlet[A] = ObjectFormlet[Id, A]

  object syntax extends ToFieldFormletOps with ToObjectFormletOps
}
