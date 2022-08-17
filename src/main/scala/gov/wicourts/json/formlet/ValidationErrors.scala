package gov.wicourts.json.formlet

import argonaut.Json
import argonaut.Json.jString
import cats.Eq
import cats.Monoid
import cats.data.NonEmptyList
import cats.data.State
import cats.syntax.all._
import scala.util.matching._

sealed trait ValidationErrors {
  def toJson: Json

  def fold[A](
    fieldErrors: NonEmptyList[String] => A,
    arrayErrors: List[(Int, ValidationErrors)] => A,
    objectErrors: List[(String, ValidationErrors)] => A,
  ): A = this match {
    case FieldErrors(errors) => fieldErrors(errors)
    case ArrayErrors(errors) => arrayErrors(errors)
    case ObjectErrors(errors) => objectErrors(errors)
  }

  def pretty: String = {
    def pretty0(e: ValidationErrors, indent: String, pendingWs: Boolean): String = {
      import ValidationErrors.unCamelCase

      e.fold(
        l => (if (pendingWs) " " else "") + l.toList.mkString("; "),
        a =>
          (if (pendingWs) "\n" else "") +
            a.map {
                case (index, errors) =>
                  indent + "- " + index + ":" + pretty0(errors, indent + "  ", true)
              }
              .mkString("\n"),
        o =>
          (if (pendingWs) "\n" else "") +
            o.map {
                case (name, errors) =>
                  indent + "- " + unCamelCase(name) + ":" + pretty0(errors, indent + "  ", true)
              }
              .mkString("\n"),
      )
    }

    pretty0(this, "", false)
  }
}

case class FieldErrors private (errors: NonEmptyList[String]) extends ValidationErrors {
  def toJson: Json = Json.array(errors.map(jString(_)).toList: _*)
}

case class ObjectErrors private (errors: List[(String, ValidationErrors)])
  extends ValidationErrors {
  def toJson: Json = Json.obj(errors.map { case (n, e) => (n, e.toJson) }: _*)
}

case class ArrayErrors private (errors: List[(Int, ValidationErrors)]) extends ValidationErrors {

  def toJson: Json = {
    val jsonErrors = errors
      .sortBy(_._1)
      .foldLeft(Vector[Json]()) { case (l, (n, e)) => l.padTo(n, Json.jNull) :+ e.toJson }
    Json.array(jsonErrors: _*)
  }
}

object ValidationErrors {
  val camelRegex = new Regex("""([\p{Lower}])(\p{Upper})""")

  private def unCamelCase(s: String): String =
    if (!s.isEmpty) {
      val s2 = Character.toUpperCase(s.charAt(0)) + s.substring(1)
      camelRegex.replaceAllIn(s2, m => m.group(1) + " " + m.group(2).toLowerCase)
    } else
      ""

  private[formlet] def fieldErrors(errors: NonEmptyList[String]): ValidationErrors =
    FieldErrors(errors)

  private[formlet] def objectErrors(errors: List[(String, ValidationErrors)]): ValidationErrors =
    ObjectErrors(errors)

  private[formlet] def arrayErrors(errors: List[(Int, ValidationErrors)]): ValidationErrors =
    ArrayErrors(errors)

  def dedup(errors: ValidationErrors): ValidationErrors = {
    def dedup_[A](l: List[(A, ValidationErrors)]): List[(A, ValidationErrors)] =
      l.groupBy(_._1).toList.map {
        case (s, vs) =>
          (s, dedup(vs.map(_._2).combineAll))
      }

    errors match {
      case FieldErrors(l) => FieldErrors(l.distinct)
      case ObjectErrors(l) => ObjectErrors(dedup_(l))
      case ArrayErrors(l) => ArrayErrors(dedup_(l))
    }
  }

  def collapse(errors: ValidationErrors): ValidationErrors = {
    def collapseAll[A](l: List[(A, ValidationErrors)]): ValidationErrors =
      l.map(_._2).map(collapse).combineAll

    dedup(
      errors match {
        case f @ FieldErrors(_) => f
        case ObjectErrors(l) => collapseAll(l)
        case ArrayErrors(l) => collapseAll(l)
      },
    )
  }

  def string(name: String, error: String): ValidationErrors =
    ObjectErrors(List((name, FieldErrors(NonEmptyList.one(error)))))

  def list(name: String, errors: NonEmptyList[String]): ValidationErrors =
    ObjectErrors(List((name, FieldErrors(errors))))

  def array(name: String, errors: NonEmptyList[(Int, ValidationErrors)]): ValidationErrors =
    ObjectErrors(List((name, ArrayErrors(errors.toList))))

  def collapseTo(name: String, errors: ValidationErrors): ValidationErrors =
    ObjectErrors(List((name, collapse(errors))))

  def relabel(errors: ValidationErrors, from: String, to: String): ValidationErrors =
    errors match {
      case ObjectErrors(errors) =>
        ObjectErrors(
          errors.map { case (k, v) => if (k === from) (to, v) else (k, v) },
        )
      case v => v
    }

  def nested(name: String, errors: ValidationErrors): ValidationErrors =
    ObjectErrors(List((name, errors)))

  implicit val validationErrorsEq: Eq[ValidationErrors] = Eq.instance((a1, a2) => {
    (a1, a2) match {
      case (FieldErrors(l1), FieldErrors(l2)) => l1.toList.sorted === l2.toList.sorted
      case (ArrayErrors(l1), ArrayErrors(l2)) => l1.sortBy(_._1) === l2.sortBy(_._1)
      case (ObjectErrors(l1), ObjectErrors(l2)) => l1.sortBy(_._1) === l2.sortBy(_._1)
      case (_, _) => false
    }
  })

  implicit val validationErrorsMonoid: Monoid[ValidationErrors] = new Monoid[ValidationErrors] {
    override def empty: ValidationErrors = ObjectErrors(Nil)

    private def merge[A: Eq](
      e1: List[(A, ValidationErrors)],
      e2: List[(A, ValidationErrors)],
    ): List[(A, ValidationErrors)] = {
      val (remaining, merged) =
        e1.traverse {
            case (a, errors) =>
              for {
                s <- State.get[List[(A, ValidationErrors)]]
                (ours, others) = s.partition { case (a1, _) => a1 === a }
                _ <- State.set(others)
              } yield (a, NonEmptyList.of(errors, ours.map { case (_, e) => e }: _*).combineAll)
          }
          .run(e2)
          .value
      merged ++ remaining
    }

    override def combine(f1: ValidationErrors, f2: ValidationErrors): ValidationErrors = {
      (f1, f2) match {
        case (FieldErrors(l1), FieldErrors(l2)) => FieldErrors(l1 |+| l2)
        case (ObjectErrors(e1), ObjectErrors(e2)) => ObjectErrors(merge(e1, e2))
        case (ArrayErrors(e1), ArrayErrors(e2)) => ArrayErrors(merge(e1, e2))
        case (_, f @ FieldErrors(_)) => f
        case (f @ FieldErrors(_), _) => f
        case (_, a @ ArrayErrors(_)) => a
        case (a @ ArrayErrors(_), _) => a
      }
    }
  }
}
