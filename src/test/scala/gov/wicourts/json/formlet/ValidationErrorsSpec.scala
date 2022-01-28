package gov.wicourts.json.formlet

import Predef.ArrowAssoc
import cats.Eq
import cats.data.NonEmptyList
import cats.kernel.laws.discipline.EqTests
import cats.kernel.laws.discipline.MonoidTests
import cats.syntax.all._
import org.scalacheck.Arbitrary._
import org.scalacheck.ScalacheckShapeless._
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import org.typelevel.discipline.specs2.mutable.Discipline

class ValidationErrorsSpec extends Specification {
  "Fields errors" >> {
    "can render themselves to JSON" >> {
      val errors = ValidationErrors.fieldErrors(NonEmptyList.of("a", "b"))

      errors.toJson.nospaces must_== """["a","b"]"""
    }

    "can be added" >> {
      val e1 = ValidationErrors.fieldErrors(NonEmptyList.of("a"))
      val e2 = ValidationErrors.fieldErrors(NonEmptyList.of("b"))

      (e1 |+| e2).toJson.nospaces must_== """["a","b"]"""
    }

    "can be deduped" >> {
      val in = ValidationErrors.fieldErrors(NonEmptyList.of("a", "b", "c", "b"))
      val out = ValidationErrors.fieldErrors(NonEmptyList.of("c", "b", "a"))

      Eq[ValidationErrors].eqv(ValidationErrors.dedup(in), out) must_== true
    }
  }

  "Object errors" >> {
    val errors = ValidationErrors.objectErrors(
      List(
        "field1" -> ValidationErrors.fieldErrors(NonEmptyList.of("a")),
        "field2" -> ValidationErrors.fieldErrors(NonEmptyList.of("b")),
        "obj1" -> ValidationErrors.objectErrors(
          List(
            "field3" -> ValidationErrors.fieldErrors(NonEmptyList.of("c")),
          ),
        ),
      ),
    )

    "can render themselves to JSON" >> {
      errors.toJson.nospaces must_== """{"field1":["a"],"field2":["b"],"obj1":{"field3":["c"]}}"""
    }

    "can be added" >> {
      val other = ValidationErrors.objectErrors(
        List(
          "field1" -> ValidationErrors.fieldErrors(NonEmptyList.of("d")),
          "field4" -> ValidationErrors.fieldErrors(NonEmptyList.of("b")),
          "obj1" -> ValidationErrors.objectErrors(
            List(
              "field3" -> ValidationErrors.fieldErrors(NonEmptyList.of("x")),
            ),
          ),
        ),
      )

      (errors |+| other).toJson.nospaces must_== """{"field1":["a","d"],"field2":["b"],"obj1":{"field3":["c","x"]},"field4":["b"]}"""
    }

    "can be deduped" >> {
      val in = ValidationErrors.objectErrors(
        List(
          "field1" -> ValidationErrors.fieldErrors(NonEmptyList.of("a", "c")),
          "field1" -> ValidationErrors.fieldErrors(NonEmptyList.of("a")),
          "field2" -> ValidationErrors.fieldErrors(NonEmptyList.of("b")),
        ),
      )

      val out = ValidationErrors.objectErrors(
        List(
          "field1" -> ValidationErrors.fieldErrors(NonEmptyList.of("a", "c")),
          "field2" -> ValidationErrors.fieldErrors(NonEmptyList.of("b")),
        ),
      )

      Eq[ValidationErrors].eqv(ValidationErrors.dedup(in), out) must_== true
    }
  }

  "Array errors" >> {
    val errors = ValidationErrors.arrayErrors(
      List(
        1 -> ValidationErrors.fieldErrors(NonEmptyList.of("a")),
        4 -> ValidationErrors.fieldErrors(NonEmptyList.of("b")),
      ),
    )

    "can render themselves to JSON" >> {
      errors.toJson.nospaces must_== """[null,["a"],null,null,["b"]]"""
    }

    "can be added" >> {
      val other = ValidationErrors.arrayErrors(
        List(
          1 -> ValidationErrors.fieldErrors(NonEmptyList.of("a")),
          6 -> ValidationErrors.fieldErrors(NonEmptyList.of("c")),
        ),
      )
      (errors |+| other).toJson.nospaces must_== """[null,["a","a"],null,null,["b"],null,["c"]]"""
    }

    "can be deduped" >> {
      val in = ValidationErrors.arrayErrors(
        List(
          1 -> ValidationErrors.fieldErrors(NonEmptyList.of("a", "c")),
          1 -> ValidationErrors.fieldErrors(NonEmptyList.of("a")),
          2 -> ValidationErrors.fieldErrors(NonEmptyList.of("b")),
        ),
      )

      val out = ValidationErrors.arrayErrors(
        List(
          1 -> ValidationErrors.fieldErrors(NonEmptyList.of("a", "c")),
          2 -> ValidationErrors.fieldErrors(NonEmptyList.of("b")),
        ),
      )

      Eq[ValidationErrors].eqv(ValidationErrors.dedup(in), out) must_== true
    }
  }

  "Validation errors" >> {
    val in = ValidationErrors.objectErrors(
      List(
        "something" -> ValidationErrors.fieldErrors(NonEmptyList.of("a", "b")),
        "other" -> ValidationErrors.arrayErrors(
          List(
            5 -> ValidationErrors.objectErrors(
              List(
                "else" -> ValidationErrors.fieldErrors(NonEmptyList.of("a", "c")),
              ),
            ),
          ),
        ),
      ),
    )

    "can be collapsed to field errors" >> {
      val out = ValidationErrors.fieldErrors(NonEmptyList.of("a", "b", "c"))

      Eq[ValidationErrors].eqv(ValidationErrors.collapse(in), out) must_== true
    }

    "can be collapsed to a particular name" >> {
      val out = ValidationErrors.objectErrors(
        List("_error" -> ValidationErrors.fieldErrors(NonEmptyList.of("a", "b", "c"))),
      )

      Eq[ValidationErrors].eqv(ValidationErrors.collapseTo("_error", in), out) must_== true
    }

    "can be folded to a value" >> {
      val fieldError = ValidationErrors.fieldErrors(NonEmptyList.of("a", "b"))
      val arrayError = ValidationErrors.arrayErrors(List(5 -> fieldError))
      val objectError = ValidationErrors.objectErrors(List("other" -> fieldError))

      def foldToString(errors: ValidationErrors): String =
        errors.fold(
          _.toList.mkString(", "),
          _.map { case (i, v) => s"$i: ${foldToString(v)}" }.mkString("[", ", ", "]"),
          _.map { case (k, v) => s"$k: ${foldToString(v)}" }.mkString("{", ", ", "}"),
        )

      foldToString(fieldError) must_== "a, b"
      foldToString(arrayError) must_== "[5: a, b]"
      foldToString(objectError) must_== "{other: a, b}"
    }

    "can be formatted to a String" >> {
      import Predef.augmentString

      val fieldError = ValidationErrors.fieldErrors(NonEmptyList.of("a", "b"))
      fieldError.pretty must_== "a; b"

      val objectError1 =
        ValidationErrors.objectErrors(List("first" -> fieldError, "second" -> fieldError))
      objectError1.pretty must_== """- First: a; b
                                    |- Second: a; b""".stripMargin

      val objectError2 = ValidationErrors.objectErrors(
        List(
          "third" -> fieldError,
          "fourth" -> objectError1,
          "fifth" -> objectError1,
          "sixth" -> fieldError,
        ),
      )

      objectError2.pretty must_== """- Third: a; b
                                    |- Fourth:
                                    |  - First: a; b
                                    |  - Second: a; b
                                    |- Fifth:
                                    |  - First: a; b
                                    |  - Second: a; b
                                    |- Sixth: a; b""".stripMargin

      val arrayError1 = ValidationErrors.arrayErrors(List(5 -> fieldError, 6 -> fieldError))

      arrayError1.pretty must_== """- 5: a; b
                                   |- 6: a; b""".stripMargin

      val arrayError2 = ValidationErrors.arrayErrors(List(5 -> objectError1))

      arrayError2.pretty must_== """- 5:
                                   |  - First: a; b
                                   |  - Second: a; b""".stripMargin

      val objectError3 = ValidationErrors.objectErrors(
        List(
          "seventhName" -> fieldError,
          "eighthName" -> arrayError2,
          "ninthName" -> fieldError,
          "tenthName" -> objectError1,
        ),
      )

      objectError3.pretty must_== """- Seventh name: a; b
                                    |- Eighth name:
                                    |  - 5:
                                    |    - First: a; b
                                    |    - Second: a; b
                                    |- Ninth name: a; b
                                    |- Tenth name:
                                    |  - First: a; b
                                    |  - Second: a; b""".stripMargin
    }

    "can be nested" >> {
      val in =
        ValidationErrors.nested(
          "a",
          ValidationErrors.string("b", "bad_b") |+|
            ValidationErrors.string("c", "bad_c"),
        )
      in.toJson.nospaces must_== """{"a":{"b":["bad_b"],"c":["bad_c"]}}"""
    }

  }
}

class ValidationErrorsLawTests extends Specification with ScalaCheck with Discipline {

  "Type class laws" >> {
    "Monoid" >> {
      checkAll("ValidationErrors.MonoidLaws", MonoidTests[ValidationErrors].monoid)
    }

    "Equal" >> {
      checkAll("ValidationErrors.EqualLaws", EqTests[ValidationErrors].eqv)
    }
  }
}
