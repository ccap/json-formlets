package gov.wicourts.json.formlet

import argonaut.ArgonautCats._
import argonaut.Json
import cats.Eq
import cats.Monoid

sealed trait JsonBuilder {
  def toJson: Json
}

class JsonArrayBuilder private[formlet] (private val items: List[Json]) extends JsonBuilder {
  def toJson: Json = Json.array(items: _*)
}

object JsonArrayBuilder {

  implicit val jsonArrayBuilderMonoid: Monoid[JsonArrayBuilder] = new Monoid[JsonArrayBuilder] {
    override def empty: JsonArrayBuilder = new JsonArrayBuilder(Nil)

    override def combine(f1: JsonArrayBuilder, f2: JsonArrayBuilder): JsonArrayBuilder =
      new JsonArrayBuilder(f1.items ++ f2.items)
  }

  implicit val jsonArrayBuilderEq: Eq[JsonArrayBuilder] =
    Eq.by(_.items)

  def item(json: Json): JsonArrayBuilder = new JsonArrayBuilder(List(json))
}

class JsonObjectBuilder private[formlet] (
  private val items: List[(String, Json)],
) extends JsonBuilder {
  def toJson: Json = Json.obj(items: _*)

  override def toString: String =
    items.mkString("JsonObjectBuilder(", ", ", ")")
}

object JsonObjectBuilder {

  implicit val jsonObjectBuilderMonoid: Monoid[JsonObjectBuilder] = new Monoid[JsonObjectBuilder] {
    override def empty: JsonObjectBuilder = new JsonObjectBuilder(Nil)

    override def combine(f1: JsonObjectBuilder, f2: JsonObjectBuilder): JsonObjectBuilder =
      new JsonObjectBuilder(f1.items ++ f2.items)
  }

  implicit val jsonObjectBuildEq: Eq[JsonObjectBuilder] =
    Eq.by(_.items)

  def row(name: String, json: Json): JsonObjectBuilder =
    new JsonObjectBuilder(List((name, json)))
}
