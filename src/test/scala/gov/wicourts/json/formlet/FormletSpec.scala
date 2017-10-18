package gov.wicourts.json.formlet

import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

import scalaz.{Equal, Apply, Applicative, Bifunctor, Contravariant}
import scalaz.Id.Id
import scalaz.scalacheck.ScalazProperties._
import scalaz.std.string._
import scalaz.syntax.monad._
import scalaz.syntax.validation._

import org.scalacheck.{Gen, Arbitrary}

import Predef.ArrowAssoc

class FormletSpec extends Specification with ScalaCheck {
  "Formlet" >> {
    "Type class laws" >> {
      val intFunction: Arbitrary[Int => Int] = Arbitrary(
        Gen.choose(1500, 2000).map(v => (i: Int) => i * v)
      )

      "Applicative" >> {
        type SampleFormlet[A] = Formlet[Id, Int, String, String, A]

        def intFormlet[A](f: Int => A): Arbitrary[SampleFormlet[A]] = Arbitrary(
          Gen.frequency(
            1 -> Gen.alphaStr.map(s => Formlet(i => (f(i).success[String], s).point[Id])),
            1 -> Apply[Gen].tuple2(Gen.alphaStr, Gen.alphaStr).map { case (s1, s2) =>
              Formlet(i => (s1.failure[A], s2).point[Id])
            }
          )
        )

        val sampleEqual: Equal[SampleFormlet[Int]] = Equal.equal((a1, a2) =>
          a1.run(99) === a2.run(99)
        )

        applicative.laws[SampleFormlet](
          Applicative[SampleFormlet],
          intFormlet((i: Int) => i*2),
          intFormlet[Int => Int](i => i2 => i + i2 + 1),
          sampleEqual
        )
      }

      "Bifunctor" >> {
        type SampleFormlet[A, B] = Formlet[Id, Int, String, A, B]

        def intFormlet[A, B](f: Int => Int): Arbitrary[SampleFormlet[Int, Int]] = Arbitrary(
          Gen.frequency(
            1 -> Gen.alphaStr.map(s => Formlet(i => (f(i).success[Int], s).point[Id])),
            1 -> Apply[Gen].tuple2(Gen.choose(1, 1000), Gen.alphaStr).map { case (_, s) =>
              Formlet((i: Int) => (i.failure[Int], s).point[Id])
            }
          )
        )

        val sampleEqual: Equal[SampleFormlet[Int, Int]] = Equal.equal((a1, a2) =>
          a1.run(99) === a2.run(99)
        )

        bifunctor.laws[SampleFormlet](
          Bifunctor[SampleFormlet],
          sampleEqual,
          intFormlet(i => i * 2),
          intFunction
        )
      }

      "Contravariant" >> {
        type SampleFormlet[A] = Formlet[Id, A, String, Int, Int]

        val sampleFormlet: Arbitrary[SampleFormlet[Int]] = Arbitrary(
          Gen.frequency(
            1 -> Gen.alphaStr.map(s => Formlet(i => ((i*2).success[Int], s).point[Id])),
            1 -> Apply[Gen].tuple2(Gen.choose(1, 1000), Gen.alphaStr).map { case (ii, ss) =>
              Formlet((i: Int) => ((i * ii).failure[Int], ss).point[Id])
            }
          )
        )

        val sampleEqual: Equal[SampleFormlet[Int]] = Equal.equal((a1, a2) =>
          a1.run(99) === a2.run(99)
        )

        contravariant.laws[SampleFormlet](
          Contravariant[SampleFormlet],
          sampleFormlet,
          intFunction,
          sampleEqual
        )
      }
    }
  }
}
