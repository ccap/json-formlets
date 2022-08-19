package gov.wicourts.json

import argonaut.Cursor
import cats.Id
import cats.data.NonEmptyList
import cats.data.Validated
import scala.language.higherKinds

package object formlet {
  type JsonFormlet[M[_], V, E, A] = Formlet[M, Validated, Option[Cursor], V, E, A]

  type FieldFormlet[M[_], A] = JsonFormlet[M, FieldView, NonEmptyList[String], A]
  type ObjectFormlet[M[_], A] = JsonFormlet[M, JsonObjectBuilder, ValidationErrors, A]

  type IdFieldFormlet[A] = FieldFormlet[Id, A]
  type IdObjectFormlet[A] = ObjectFormlet[Id, A]
}
