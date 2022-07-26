package gov.wicourts.json.formlet

import argonaut.Argonaut.jString
import argonaut.Json

import monocle.Lens

import cats.syntax.all._
import Predef.ArrowAssoc

case class FieldView(name: String, value: Option[Json], label: Option[String], errorName: String) {

  def obj: JsonObjectBuilder = {
    val metadataItems =
      List(
        label.map(l => "label" -> jString(l)),
      ).unite

    val metadata = metadataItems.headOption
      .as(("metadata", Json.obj(metadataItems: _*)))
      .toList

    val valueItem = value.map(("value", _)).toList

    val all = valueItem ++ metadata

    new JsonObjectBuilder(
      if (all.isEmpty) Nil
      else List(name -> Json.obj(all: _*)),
    )
  }

  def toJson: Json = obj.toJson
}

object FieldView {

  val label: Lens[FieldView, Option[String]] =
    Lens[FieldView, Option[String]](_.label)(lbl => _.copy(label = lbl))

  val errorName: Lens[FieldView, String] =
    Lens[FieldView, String](_.errorName)(err => _.copy(errorName = err))
}
