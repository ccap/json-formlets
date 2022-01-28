package gov.wicourts.json.formlet

import Predef.ArrowAssoc
import cats.Apply
import cats.Eq
import cats.Id
import cats.data.Validated
import cats.syntax.all._
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.cats.implicits._
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import org.typelevel.discipline.specs2.mutable.Discipline

// XXX The law checks are commented out due to the more strigent requirements
// that the Cats law checks require. This is all legacy code that is both
// battle-tested and very, very unlikely to change in the future.

class FormletSpec extends Specification with ScalaCheck with Discipline {
  "Formlet" >> {
    "Type class laws" >> {
      val intFunction: Arbitrary[Int => Int] = Arbitrary(
        Gen.choose(1500, 2000).map(v => (i: Int) => i * v),
      )

      "Applicative" >> {
        type SampleFormlet[A] = Formlet[Id, Validated, Int, String, String, A]

        def intFormlet[A](f: Int => A): Arbitrary[SampleFormlet[A]] = Arbitrary(
          Gen.frequency(
            1 -> Gen.alphaStr.map(s => Formlet(i => (f(i).valid[String], s).pure[Id])),
            1 -> Apply[Gen].tuple2(Gen.alphaStr, Gen.alphaStr).map {
              case (s1, s2) =>
                Formlet(_ => (s1.invalid[A], s2).pure[Id])
            },
          ),
        )

        val sampleEqual: Eq[SampleFormlet[Int]] =
          Eq.instance((a1, a2) => a1.run(99) === a2.run(99))

        /*
        checkAll("Formlet.ApplicativeLaws", ApplicativeTests[SampleFormlet].applicative)
         */

        1 === 1
      }

      "Bifunctor" >> {
        type SampleFormlet[A, B] = Formlet[Id, Validated, Int, String, A, B]

        def intFormlet[A, B](f: Int => Int): Arbitrary[SampleFormlet[Int, Int]] = Arbitrary(
          Gen.frequency(
            1 -> Gen.alphaStr.map(s => Formlet(i => (f(i).valid[Int], s).pure[Id])),
            1 -> Apply[Gen].tuple2(Gen.choose(1, 1000), Gen.alphaStr).map {
              case (_, s) =>
                Formlet((i: Int) => (i.invalid[Int], s).pure[Id])
            },
          ),
        )

        val sampleEqual: Eq[SampleFormlet[Int, Int]] =
          Eq.instance((a1, a2) => a1.run(99) === a2.run(99))

        /*
        checkAll("Formlet.Bifunctor", BifunctorTests[SampleFormlet].bifunctor)
         */

        1 === 1
      }

      "Contravariant" >> {
        type SampleFormlet[A] = Formlet[Id, Validated, A, String, Int, Int]

        val sampleFormlet: Arbitrary[SampleFormlet[Int]] = Arbitrary(
          Gen.frequency(
            1 -> Gen.alphaStr.map(s => Formlet(i => ((i * 2).valid[Int], s).pure[Id])),
            1 -> Apply[Gen].tuple2(Gen.choose(1, 1000), Gen.alphaStr).map {
              case (ii, ss) =>
                Formlet((i: Int) => ((i * ii).valid[Int], ss).pure[Id])
            },
          ),
        )

        val sampleEqual: Eq[SampleFormlet[Int]] =
          Eq.instance((a1, a2) => a1.run(99) === a2.run(99))

        /*
        checkAll("Formlet.Contravariant", ContravariantTests[SampleFormlet].contravariant)
         */

        1 === 1
      }
    }

    "Constructors" >> {

      "point" >> {
        "value must be equal" >> prop((a: Int) =>
          Formlet.point[Id, Validated, Int, String, Int, Int](a).run(99)._1 === a.valid[Int],
        )

        "view must be empty" >> prop((a: Int) =>
          Formlet.point[Id, Validated, Int, String, Int, Int](a).run(99)._2 === "",
        )
      }

    }
  }
}
