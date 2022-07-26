package gov.wicourts.json.formlet

//import Predef.ArrowAssoc
import cats.Id
import cats.Monoid
import cats.data.Validated
import cats.syntax.all._
//import org.scalacheck.Gen
//import org.scalacheck.Arbitrary
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

//import gov.wicourts.json.formlet.test.instances._

class FormletSpec extends Specification with ScalaCheck {
  "Formlet" >> {
    /* TODO
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
                Formlet(i => (s1.invalid[A], s2).pure[Id])
            },
          ),
        )

        val sampleEqual: Equal[SampleFormlet[Int]] =
          Equal.equal((a1, a2) => a1.run(99) === a2.run(99))

        applicative.laws[SampleFormlet](
          Applicative[SampleFormlet],
          intFormlet((i: Int) => i * 2),
          intFormlet[Int => Int](i => i2 => i + i2 + 1),
          sampleEqual,
        )
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

        val sampleEqual: Equal[SampleFormlet[Int, Int]] =
          Equal.equal((a1, a2) => a1.run(99) === a2.run(99))

        bifunctor.laws[SampleFormlet](
          Bifunctor[SampleFormlet],
          sampleEqual,
          intFormlet(i => i * 2),
          intFunction,
        )
      }

      "Contravariant" >> {
        type SampleFormlet[A] = Formlet[Id, Validated, A, String, Int, Int]

        val sampleFormlet: Arbitrary[SampleFormlet[Int]] = Arbitrary(
          Gen.frequency(
            1 -> Gen.alphaStr.map(s => Formlet(i => ((i * 2).valid[Int], s).pure[Id])),
            1 -> Apply[Gen].tuple2(Gen.choose(1, 1000), Gen.alphaStr).map {
              case (ii, ss) =>
                Formlet((i: Int) => ((i * ii).invalid[Int], ss).pure[Id])
            },
          ),
        )

        val sampleEqual: Equal[SampleFormlet[Int]] =
          Equal.equal((a1, a2) => a1.run(99) === a2.run(99))

        contravariant.laws[SampleFormlet](
          Contravariant[SampleFormlet],
          sampleFormlet,
          intFunction,
          sampleEqual,
        )
      }
    }
     */

    "Constructors" >> {

      "pure" >> {
        "value must be equal" >> prop((a: Int) =>
          Formlet.pure[Id, Validated, Int, String, Int, Int](a).run(99)._1 === a.valid,
        )

        "view must be empty" >> prop((a: Int) =>
          Formlet.pure[Id, Validated, Int, String, Int, Int](a).run(99)._2 === Monoid[String].empty,
        )
      }

    }
  }
}
