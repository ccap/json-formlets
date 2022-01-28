package gov.wicourts.json.formlet

import argonaut.Argonaut.jString
import argonaut.Json

case class FieldView(name: String, value: Option[Json], label: Option[String], errorName: String) {

  def obj: JsonObjectBuilder = {
    val metadataItems =
      List(
        label.map(l => ("label", jString(l))),
      ).flatten

    val metadata =
      metadataItems.headOption.map(_ => ("metadata", Json.obj(metadataItems: _*))).toList

    val valueItem = value.map(("value", _)).toList

    val all = valueItem ++ metadata

    new JsonObjectBuilder(
      if (all.isEmpty) Nil
      else List((name, Json.obj(all: _*))),
    )
  }

  def toJson: Json = obj.toJson
}
