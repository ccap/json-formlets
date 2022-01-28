package gov.wicourts.json.formlet

import argonaut.Json.jNumber
import cats.Apply
import cats.kernel.laws.discipline.EqTests
import cats.kernel.laws.discipline.MonoidTests
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen
import org.scalacheck.Gen
import org.scalacheck.Shapeless._
import org.scalacheck.cats.implicits._
import org.specs2.mutable.Specification
import org.typelevel.discipline.specs2.mutable.Discipline

class JsonBuilderLawTests extends Specification with Discipline {
  "JsonArrayBuilder" >> {
    "Type class laws" >> {
      implicit val arbitraryJsonArrayBuilder: Arbitrary[JsonArrayBuilder] = Arbitrary(
        Gen.listOf(Gen.choose(1, 100)).map(l => new JsonArrayBuilder(l.map(jNumber(_)))),
      )

      implicit def cogenJsonObjectBuilder: Cogen[JsonArrayBuilder] =
        Cogen[String].contramap(_.toJson.nospaces)

      "Monoid" >> {
        checkAll("JsonArrayBuilder.MonoidLaws", MonoidTests[JsonArrayBuilder].monoid)
      }

      "Equal" >> {
        checkAll("JsonArrayBuilder.EqualLaws", EqTests[JsonArrayBuilder].eqv)
      }
    }
  }

  "JsonObjectBuilder" >> {
    "Type class laws" >> {
      implicit val arbitraryJsonObjectBuilder: Arbitrary[JsonObjectBuilder] = Arbitrary(
        Gen
          .listOf(Apply[Gen].tuple2(Gen.alphaStr, Gen.choose(1, 100)))
          .map(l => new JsonObjectBuilder(l.map { case (s, n) => (s, jNumber(n)) })),
      )

      implicit def cogenJsonObjectBuilder: Cogen[JsonObjectBuilder] =
        Cogen[String].contramap(_.toJson.nospaces)

      "Monoid" >> {
        checkAll("JsonObjectBuilder.MonoidLaws", MonoidTests[JsonObjectBuilder].monoid)
      }

      "Equal" >> {
        checkAll("JsonObjectBuilder.EqualLaws", EqTests[JsonObjectBuilder].eqv)
      }
    }
  }
}
