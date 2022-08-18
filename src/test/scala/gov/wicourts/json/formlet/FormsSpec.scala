package gov.wicourts.json.formlet

import Predef.ArrowAssoc
import argonaut._
import cats.Applicative
import cats.Apply
import cats.FlatMap
import cats.Id
import cats.data.NonEmptyList
import cats.syntax.all._
import org.specs2.mutable.Specification

import gov.wicourts.json.formlet.Forms.Id._

class FormsSpec extends Specification {

  private def parse(s: String): Option[Cursor] =
    Parse.parseOption(s).getOrElse(throw new Exception("Unexpected parse failure")).cursor.some

  "A string form" >> {
    "should be able to render its value" >> {
      val view = string("nameL", "Smith".some).view(None)
      view.toJson.nospaces must_== """{"nameL":{"value":"Smith"}}"""
    }

    "should be able to extract its value" >> {
      val result = string("nameL", None).eval(
        parse("""{"nameL":"Smith"}"""),
      )

      result must_== "Smith".some.valid
    }

    "should fail with an error message if value exists, but is not a string" >> {
      val result = string("nameL", None).eval(
        parse("""{"nameL":1}"""),
      )

      result.toString must contain("Field nameL must be a(n) string")
    }

    "should treat null as empty" >> {
      val result = string("nameL", None).eval(
        parse("""{"nameL":null}"""),
      )

      result must_== None.valid
    }

    "should treat a zero-length string as empty" >> {
      val result = string("nameL", None).eval(
        parse("""{"nameL":"  "}"""),
      )

      result must_== None.valid
    }

    "should trim spaces from the result" >> {
      val result = string("nameL", None).eval(
        parse("""{"nameL":" Smith  "}"""),
      )

      result must_== "Smith".some.valid
    }

    "should omit the value from the rendered view if value is not defined" >> {
      val view = string("nameL", None).view(None)

      view.toJson.nospaces must_== "{}"
    }

    "can be assigned a label" >> {
      val view = string("nameL", None).label("Last name").view(None)

      view.toJson.nospaces must_== """{"nameL":{"metadata":{"label":"Last name"}}}"""
    }

    "can be required" >> {
      val a = string("nameL", None).required.eval(None)

      a must_== NonEmptyList.one("This field is required").invalid
    }

    "can associate an error with another field" >> {
      val r = string("nameL", None)
        .mapValidation(o => "Nope".invalidNel[String])
        .errorName("nameLOther")
      val results = r.obj.eval(None)
      results.leftMap(_.toJson.nospaces) must_== """{"nameLOther":["Nope"]}""".invalid
    }

    "can fail to an alternative" >> {
      val failing = string("nameL", None)
        .mapValidation(_ => "No way!".invalidNel[Option[String]])
      val good = string("nameL", None)
        .mapValidation(_ => "Success!".some.validNel[String])
      val results = (failing orElse good).obj.eval(None)
      results must_== "Success!".some.valid
    }

    "using .flatMap (with another name)" >> {
      "sums its views on success" >> {
        val first = string("nameF", "John".some).obj
        val second = string("nameL", "Smith".some).obj
        val result = first.using(_ => second).view(None)
        result.toJson.nospaces must_== """{"nameF":{"value":"John"},"nameL":{"value":"Smith"}}"""
      }

      "works (!)" >> {
        val nameL = string("nameL", None).obj

        val result = nameL
          .using(n => Applicative[IdObjectFormlet].pure(n))
          .eval(
            parse("""{"nameL":" Smith  "}"""),
          )

        result must_== "Smith".some.valid
      }
    }
  }

  "A number form" >> {
    "can be validated" >> {
      val f = number("count", None).required
        .validate(
          _.valid
            .ensure(NonEmptyList.one("count must be bigger than 7"))(_.truncateToInt > 7)
            .ensure(NonEmptyList.one("count must be less than 5"))(_.truncateToInt < 5),
        )
      val result = f.eval(parse("""{"count":6}"""))

      result must_== NonEmptyList.one("count must be bigger than 7").invalid
    }

    "treats an empty string as empty" >> {
      val f = number("count", None)

      val result = f.eval(parse("""{"count":""}"""))

      result must_== None.valid
    }
  }

  "A boolean form" >> {
    "treats an empty string as empty" >> {
      val f = boolean("isActive", None)

      val result = f.eval(parse("""{"isActive":""}"""))

      result must_== None.valid
    }

    "can be used to determine which of two forms are evaluated where the" >> {
      "first form is evaluated if the condition form is true" >> {
        val cond = boolean("condition", None)
          .mapValidation(_ => true.validNel[String])
        val first = string("nameL", None)
          .mapValidation(_ => "First".validNel[String])
        val second = string("nameL", None)
          .mapValidation(_ => "Second".invalidNel[String])
        val results = Formlet.ifM(cond, first, second).obj.eval(None)

        results must_== "First".valid
      }

      "second form is evaluated if the condition form is false" >> {
        val cond = boolean("condition", None)
          .mapValidation(_ => false.validNel[String])
        val first = string("nameL", None)
          .mapValidation(_ => "First".invalidNel[String])
        val second = string("nameL", None)
          .mapValidation(_ => "Second".validNel[String])
        val results = Formlet.ifM(cond, first, second).obj.eval(None)

        results must_== "Second".valid
      }
    }
  }

  "A JSON form" >> {
    "should be able to render its value" >> {
      val view = json("test1", Parse.parseOption("""{"test2":"testValue"}""")).view(None)
      view.toJson.nospaces must_== """{"test1":{"value":{"test2":"testValue"}}}"""
    }

    "should be able to extract its value" >> {
      val result = json("test1", None).eval(
        parse("""{"test1":{"test2":"testValue"}}"""),
      )
      result must_== Parse.parseOption("""{"test2":"testValue"}""").valid
    }
  }

  "A string array form" >> {
    "should be able to render its value" >> {
      val view = listOfString("colors", List("red", "blue", "green").some).view(None)
      view.toJson.nospaces must_== """{"colors":{"value":["red","blue","green"]}}"""
    }

    "should be able to extract its value" >> {
      val result = listOfString("colors", None).eval(
        parse("""{"colors":["red","green"]}"""),
      )

      result must_== List("red", "green").some.valid
    }

    "should fail if property is not an array" >> {
      val result = listOfString("colors", None).eval(
        parse("""{"colors":1}"""),
      )

      result must_== NonEmptyList.one("Field colors must be a(n) array of string").invalid
    }

    "should fail if array does not contain required type" >> {
      val result = listOfString("colors", None).eval(
        parse("""{"colors":["red", 1]}"""),
      )

      result must_== NonEmptyList.one("Expected a string when processing field colors").invalid
    }

    "should treat an empty string as an empty" >> {
      val result = listOfString("colors", None).eval(
        parse("""{"colors":""}"""),
      )

      result must_== None.valid
    }
  }

  "A JSON array form" >> {
    "should be able to render its value" >> {
      val jsonList = List(
        Parse.parseOption("""{"color":"red"}"""),
        Parse.parseOption("""{"size":"small"}"""),
        Parse.parseOption("""{"count":2,"foo":"bar"}"""),
      ).flatten
      val view = listOfJson("items", jsonList.some).view(None)
      view.toJson.nospaces must_== """{"items":{"value":[{"color":"red"},{"size":"small"},{"count":2,"foo":"bar"}]}}"""
    }

    "should be able to extract its value" >> {
      val jsonList = List(
        Parse.parseOption("""{"color":"red"}"""),
        Parse.parseOption("""{"size":"small"}"""),
        Parse.parseOption("""{"count":2,"foo":"bar"}"""),
      ).flatten
      val result = listOfJson("items", None).eval(
        parse("""{"items":[{"color":"red"},{"size":"small"},{"count":2,"foo":"bar"}]}"""),
      )
      result must_== jsonList.some.valid
    }
  }

  case class FullName(nameF: Option[String], nameL: Option[String])

  def fullNameForm(fullName: FullName): IdObjectFormlet[FullName] =
    Apply[IdObjectFormlet].map2(
      string("nameF", fullName.nameF).obj,
      string("nameL", fullName.nameL).obj,
    )(FullName.apply _)

  case class RequiredLastFullName(nameF: Option[String], nameL: String)

  def requiredLastFullName(fullName: RequiredLastFullName): IdObjectFormlet[RequiredLastFullName] =
    Apply[IdObjectFormlet].map2(
      string("nameF", fullName.nameF).obj,
      string("nameL", fullName.nameL.some).required.obj,
    )(RequiredLastFullName.apply _)

  "A composite form example" >> {

    "should be able to render initial data" >> {
      val view = fullNameForm(FullName("Jack".some, "Sprat".some)).view(None)

      view.toJson.nospaces must_== """{"nameL":{"value":"Sprat"},"nameF":{"value":"Jack"}}"""
    }

    "should be able to extract data" >> {
      val (result, view) = fullNameForm(FullName(None, None)).run(
        parse("""{"nameL":"Sprat","nameF":"Jack"}"""),
      )

      view.toJson.nospaces must_== """{"nameL":{"value":"Sprat"},"nameF":{"value":"Jack"}}"""
      result must_== FullName("Jack".some, "Sprat".some).valid
    }

    "should be able to validate whole name, but associate error with one field" >> {
      val errorForm = fullNameForm(FullName(None, None)).validate(fn =>
        if (fn.nameL == "Sprat".some && fn.nameF != "Jack".some)
          ValidationErrors.string("nameF", "You must be named Jack").invalid
        else
          fn.valid,
      )

      val result1 = errorForm.eval(
        parse("""{"nameL":"Sprat","nameF":"Jack"}"""),
      )
      result1 must_== FullName("Jack".some, "Sprat".some).valid

      val result2 = errorForm.eval(
        parse("""{"nameL":"Sprat","nameF":"Jill"}"""),
      )
      result2.leftMap(_.toJson.nospaces) must_== """{"nameF":["You must be named Jack"]}""".invalid
    }

    "should be able to validate field based on other field value" >> {
      def checkResults(both: IdObjectFormlet[(Option[String], Option[String])]) = {
        val result1 = both.eval(
          parse("""{"nameL":"Sprat","nameF":"Jack"}"""),
        )
        result1 must_== ("Jack".some, "Sprat".some).valid

        val result2 = both.eval(
          parse("""{"nameL":"Sprat","nameF":"Jill"}"""),
        )
        result2.leftMap(_.toJson.nospaces) must_==
          """{"nameL":["If your last name is Sprat, your first name must be Jack"]}""".invalid
      }

      "in M" >> {
        val nameF = string("nameF", None)
        val nameL = string("nameL", None).validateVM(nameF.value) { (other, s) =>
          if (s.exists(_ == "Sprat") && !FlatMap[Option].flatten(other).exists(_ == "Jack"))
            "If your last name is Sprat, your first name must be Jack".invalidNel
              .pure[Id]
          else
            s.valid
        }
        val both = Apply[IdObjectFormlet].tuple2(nameF.obj, nameL.obj)

        checkResults(both)
      }

      "in M (joining on optional value)" >> {
        val nameF = string("nameF", None)
        val nameL = string("nameL", None).validateVM(nameF.valueOpt) { (other, s) =>
          if (s.exists(_ == "Sprat") && !other.exists(_ == "Jack"))
            "If your last name is Sprat, your first name must be Jack".invalidNel
              .pure[Id]
          else
            s.valid
        }
        val both = Apply[IdObjectFormlet].tuple2(nameF.obj, nameL.obj)

        checkResults(both)
      }
    }

    "can be nested" >> {
      "and should be able to render initial data" >> {
        val fullName = FullName("Jack".some, "Sprat".some)
        val view = nested("fullName", fullNameForm(fullName)).view(None)

        val json = """{"fullName":{"nameL":{"value":"Sprat"},"nameF":{"value":"Jack"}}}"""
        view.toJson.nospaces must_== json
      }

      "and should be able to extract data" >> {
        val fullName = FullName(None, None)
        val json = """{"fullName":{"nameL":"Sprat","nameF":"Jack"}}"""
        val result = nested("fullName", fullNameForm(fullName)).eval(parse(json))

        result must_== FullName("Jack".some, "Sprat".some).valid
      }

      "and should associate error information with name" >> {
        val fullName = FullName(None, None)
        val json = """{"fullName":{"nameL":1,"nameF":"Jack"}}"""
        val result = nested("fullName", fullNameForm(fullName)).eval(parse(json))

        val expected = """{"fullName":{"nameL":["Field nameL must be a(n) string"]}}"""
        result.leftMap(_.toJson.nospaces) must_== expected.invalid
      }
    }

    "can be nested (optionally)" >> {
      "and should be able to return None if property doesn't exist" >> {
        val fullName = RequiredLastFullName(None, "")
        val json = """{"other": "one"}"""
        val result = nestedOptional("fullName", requiredLastFullName(fullName)).eval(parse(json))

        result must_== None.valid
      }

      "and should fail validation if property exists, but is wrong/incomplete" >> {
        val fullName = RequiredLastFullName(None, "")
        val json = """{"fullName": {}}"""
        val result = nestedOptional("fullName", requiredLastFullName(fullName)).eval(parse(json))

        val expected = """{"fullName":{"nameL":["This field is required"]}}"""
        result.leftMap(_.toJson.nospaces) must_== expected.invalid
      }

      "and should be able to extract data" >> {
        val fullName = RequiredLastFullName(None, "")
        val json = """{"fullName":{"nameL":"Sprat","nameF":"Jack"}}"""
        val result = nestedOptional("fullName", requiredLastFullName(fullName)).eval(parse(json))

        result must_== RequiredLastFullName("Jack".some, "Sprat").some.valid
      }
    }
  }

  "List forms" >> {
    val fullNames = list(
      fullNameForm(FullName(None, None)),
      List(
        fullNameForm(FullName("Jack".some, "Sprat".some)),
        fullNameForm(FullName("Jill".some, "Smith".some)),
      ),
    )

    "should be able to render initial data" >> {
      val view = fullNames.view(None)

      val expected =
        """[{"nameL":{"value":"Sprat"},"nameF":{"value":"Jack"}},{"nameL":{"value":"Smith"},"nameF":{"value":"Jill"}}]"""

      view.toJson.nospaces must_== expected
    }

    "should be able to extract data" >> {
      val json = """[{"nameL":"Jones"},{"nameL":"Johnson"}]"""
      val result = fullNames.eval(parse(json))

      result must_== List(FullName(None, "Jones".some), FullName(None, "Johnson".some)).valid
    }

    "should be able to report nested errors properly" >> {
      val json = """{"fullName":[{"nameL":"Jones"},{"nameL":1},{"nameL":"Jensen"}]}"""
      val form = nested("fullName", list(fullNameForm(FullName(None, None)), Nil))

      val result = form.eval(parse(json))

      val expected = """{"fullName":[null,{"nameL":["Field nameL must be a(n) string"]}]}"""
      result.leftMap(_.toJson.nospaces) must_== expected.invalid
    }

    "should treat an empty string as an empty list" >> {
      val json = """{"fullName":""}"""
      val form = nested("fullName", list(fullNameForm(FullName(None, None)), Nil))

      val result = form.eval(parse(json))

      result must_== List().valid
    }
  }

  "A form with .fromRoot" >> {
    "should always read its value starting from the input root" >> {
      val name = string("name", None).required.fromRoot
      val json = Json.obj(
        "name" -> Json.jString("John"),
        "names" -> Json.array(
          Json.obj(
            "nested" ->
              Json.obj(
                "otherName" -> Json.jString("John"),
              ),
          ),
        ),
      )

      val form =
        name.obj <*
          nested(
            "names",
            list(
              nested(
                "nested",
                string("otherName", None).required
                  .validateVM(name.value)((root, other) =>
                    if (root.exists(_ == other))
                      other.valid
                    else
                      "Names should have matched!".invalidNel,
                  )
                  .obj,
              ),
              Nil,
            ),
          )

      val result = form.eval(json.cursor.some)

      result must_== "John".valid
    }
  }
}
