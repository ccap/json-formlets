package gov.wicourts.json.formlet

import argonaut._
import argonaut.Argonaut._
import cats.Applicative
import cats.Endo
import cats.Functor
import cats.Id
import cats.data.Kleisli
import cats.data.NonEmptyList
import cats.data.State
import cats.data.Validated
import cats.syntax.all._
import org.slf4j.LoggerFactory
import scala.language.higherKinds

import Predef.identity

object Forms {
  private val logger = LoggerFactory.getLogger(loggerName)

  private def loggerName: String = getClass.getName.substring(0, getClass.getName.length - 1)

  private def primitive[M[_]: Applicative, A](
    descr: String,
    toJson: A => Json,
    matches: Json => Boolean,
    fromJson: Json => Either[NonEmptyList[String], A],
    name: String,
    value: Option[A],
  ): FieldFormlet[M, Option[A]] =
    Formlet { c =>
      val result: Validated[NonEmptyList[String], Option[A]] =
        (for {
          cursor <- c
          fld <- cursor.downField(name)
          json = fld.focus
          if !json.isNull && !json.string.exists(_ === "")
        } yield json)
          .traverse(j =>
            j.asRight[NonEmptyList[String]]
              .ensure(NonEmptyList.one(s"Field $name must be a(n) $descr"))(matches)
              .flatMap(fromJson),
          )
          .toValidated

      if (logger.isDebugEnabled) {
        logger.debug(s"Attempting to parse a $descr named $name")
      }

      if (logger.isTraceEnabled) {
        logger.trace(s"Input JSON is ${cursorToString(c)}")
        logger.trace(s"Field JSON is ${cursorToString(c.flatMap(_.downField(name)))}")
      }

      val view = FieldView(name, (result.toOption.flatten.orElse(value)).map(toJson), None, name)

      (result, view).pure[M]
    }

  private def cursorToString(c: Option[Cursor]): String = c.map(_.focus.nospaces).toString

  private def namedContext[M[_], E, A, V](
    name: String,
    inner: JsonFormlet[M, E, A, V],
  ): JsonFormlet[M, E, A, V] =
    inner.contramap { c =>
      val result = c.flatMap(_.downField(name))

      if (logger.isDebugEnabled) {
        logger.debug(s"Moving parse location into property $name")
      }

      if (logger.isTraceEnabled) {
        logger.trace(s"Before JSON is ${cursorToString(c)}")
        logger.trace(s"After JSON is ${cursorToString(result)}")
      }

      result
    }

  def nestedM[M[_]: Functor, A, V <: JsonBuilder](
    name: String,
    inner: JsonFormlet[M, V, ValidationErrors, A],
  ): ObjectFormlet[M, A] =
    namedContext(name, inner).mapResult((r, v) =>
      (
        r.leftMap(x => ValidationErrors.objectErrors(List((name, x)))),
        JsonObjectBuilder.row(name, v.toJson),
      ),
    )

  def nestedOptionalM[M[_], A, V <: JsonBuilder](
    name: String,
    inner: JsonFormlet[M, V, ValidationErrors, A],
  )(
    implicit M: Applicative[M],
  ): ObjectFormlet[M, Option[A]] =
    Formlet(c =>
      (if (c.flatMap(_.downField(name)).isDefined)
         nestedM(name, inner).map(_.some)
       else
         Applicative[ObjectFormlet[M, ?]].pure(none[A])).run(c),
    )

  // This was removed from Argonaut in v6.2 as part of moving the Scalaz
  // integration to a separate library.
  private def traverseBreak[X](
    c: Cursor,
    r: Kleisli[State[X, ?], Cursor, Option[Cursor]],
  ): Endo[X] =
    (x => {
      @annotation.tailrec
      def spin(z: X, d: Cursor): X = {
        val (q, k) = r.run(d).run(z).value
        k match {
          case None => q
          case Some(a) => spin(q, a)
        }
      }

      spin(x, c)
    })

  def listM[M[_], A](
    template: ObjectFormlet[M, A],
    defaultValue: List[ObjectFormlet[M, A]],
  )(
    implicit M: Applicative[M],
  ): JsonFormlet[M, JsonArrayBuilder, ValidationErrors, List[A]] =
    Formlet { c =>
      val l: List[(Option[Cursor], ObjectFormlet[M, A])] =
        if (c.isEmpty || c.map(_.focus).exists(_.string.exists(_ === "")))
          defaultValue.map((c, _))
        else {
          type X = Vector[(Option[Cursor], ObjectFormlet[M, A])]
          c.flatMap(_.downArray)
            .map(arrCursor =>
              traverseBreak(
                arrCursor,
                Kleisli[State[X, ?], Cursor, Option[Cursor]](elCursor =>
                  //Note: .right means "the element to the right", not Right(_)
                  State(l => (l :+ ((elCursor.some, template)), elCursor.right)),
                ),
              ).apply(Vector()).toList,
            )
            .getOrElse(Nil)
        }

      type G[AA] = M[(JsonArrayBuilder, Validated[ValidationErrors, AA])]
      val X = M
        .compose[Tuple2[JsonArrayBuilder, ?]]
        .compose[Validated[ValidationErrors, ?]]
      M.map(
        l.zipWithIndex
          .traverse[G, A] {
            case ((i, x), idx) =>
              M.map(
                x.mapResult((r, v) =>
                    (
                      r.leftMap(o => ValidationErrors.arrayErrors(List((idx, o)))),
                      JsonArrayBuilder.item(v.toJson),
                    ),
                  )
                  .run(i),
              )(_.swap)
          }(X),
      ) { case (x, y) => (y, x) }
    }

  private def check[A](name: String, descr: String, a: Option[A]): Either[NonEmptyList[String], A] =
    a.toRight(NonEmptyList.one(s"Expected a $descr when processing field $name"))

  private def fromArray[A](
    name: String,
    descr: String,
    fromItem: Json => Option[A],
  ): Json => Either[NonEmptyList[String], List[A]] =
    j =>
      check(name, s"array of $descr", j.array)
        .flatMap(_.traverse(i => check(name, descr, fromItem(i))))

  def listOfJsonM[M[_]: Applicative](
    name: String,
    value: Option[List[Json]],
  ): FieldFormlet[M, Option[List[Json]]] =
    primitive(
      "array of JSON",
      l => Json.array(l: _*),
      _.isArray,
      fromArray(name, "JSON", j => j.some),
      name,
      value,
    )

  def listOfStringM[M[_]: Applicative](
    name: String,
    value: Option[List[String]],
  ): FieldFormlet[M, Option[List[String]]] =
    primitive(
      "array of string",
      l => Json.array(l.map(jString(_)): _*),
      _.isArray,
      fromArray(name, "string", _.string.map(_.trim)),
      name,
      value,
    )

  def listOfNumberM[M[_]: Applicative](
    name: String,
    value: Option[List[JsonNumber]],
  ): FieldFormlet[M, Option[List[JsonNumber]]] =
    primitive(
      "array of number",
      l => Json.array(l.map(jNumber(_)): _*),
      _.isArray,
      fromArray(name, "number", _.number),
      name,
      value,
    )

  def listOfBooleanM[M[_]: Applicative](
    name: String,
    value: Option[List[Boolean]],
  ): FieldFormlet[M, Option[List[Boolean]]] =
    primitive(
      "array of boolean",
      l => Json.array(l.map(jBool(_)): _*),
      _.isArray,
      fromArray(name, "boolean", _.bool),
      name,
      value,
    )

  def stringM[M[_]: Applicative](
    name: String,
    value: Option[String],
  ): FieldFormlet[M, Option[String]] = {
    val result: FieldFormlet[M, Option[String]] =
      primitive(
        "string",
        jString(_),
        _.isString,
        ((_: Json).string) andThen (a => check(name, "string", a)),
        name,
        value,
      )
    result.map(_.map(_.trim).filterNot(_.isEmpty))
  }

  def jsonM[M[_]: Applicative](
    name: String,
    value: Option[Json],
  ): FieldFormlet[M, Option[Json]] =
    primitive(
      "JSON",
      identity,
      _ => true,
      x => x.asRight,
      name,
      value,
    )

  def numberM[M[_]: Applicative](
    name: String,
    value: Option[JsonNumber],
  ): FieldFormlet[M, Option[JsonNumber]] =
    primitive(
      "number",
      jNumber(_),
      _.isNumber,
      ((_: Json).number) andThen (a => check(name, "number", a)),
      name,
      value,
    )

  def booleanM[M[_]: Applicative](
    name: String,
    value: Option[Boolean],
  ): FieldFormlet[M, Option[Boolean]] =
    primitive(
      "boolean",
      jBool(_),
      _.isBool,
      ((_: Json).bool) andThen (a => check(name, "boolean", a)),
      name,
      value,
    )

  def label[M[_]: Functor, A](
    field: FieldFormlet[M, A],
    label: String,
  ): FieldFormlet[M, A] =
    field.mapView(FieldView.label.set(label.some)(_))

  def errorName[M[_]: Functor, A](
    field: FieldFormlet[M, A],
    errorName: String,
  ): FieldFormlet[M, A] =
    field.mapView(FieldView.errorName.set(errorName)(_))

  def required[M[_]: Functor, A](
    field: FieldFormlet[M, Option[A]],
  ): FieldFormlet[M, A] =
    field.mapValidation(_.toValid(NonEmptyList.one("This field is required")))

  def requiredObj[M[_]: Functor, A](
    name: String,
    obj: ObjectFormlet[M, Option[A]],
  ): ObjectFormlet[M, A] =
    obj.mapValidation(_.toValid(ValidationErrors.string(name, "This field is required")))

  def obj[M[_]: Functor, A](
    field: FieldFormlet[M, A],
  ): ObjectFormlet[M, A] =
    field.mapResult((a, v) =>
      (
        a.leftMap(l => ValidationErrors.list(v.errorName, l)),
        v.obj,
      ),
    )

  def fromRoot[M[_], E, A, V](formlet: JsonFormlet[M, E, A, V]): JsonFormlet[M, E, A, V] =
    fromLocation("root", _.undo.cursor.some, formlet)

  def fromParent[M[_], E, A, V](formlet: JsonFormlet[M, E, A, V]): JsonFormlet[M, E, A, V] =
    fromLocation("parent", _.up, formlet)

  def setRoot[M[_], E, A, V](formlet: JsonFormlet[M, E, A, V]): JsonFormlet[M, E, A, V] =
    fromLocation("new root", _.focus.cursor.some, formlet)

  private def fromLocation[M[_], E, A, V](
    descr: String,
    f: Cursor => Option[Cursor],
    formlet: JsonFormlet[M, E, A, V],
  ): JsonFormlet[M, E, A, V] =
    formlet.contramap { c =>
      val result = c.flatMap(f)

      if (logger.isDebugEnabled) {
        logger.debug(s"Returning to $descr")
      }

      if (logger.isTraceEnabled) {
        logger.trace(s"Before returning to $descr JSON is ${cursorToString(c)}")
        logger.trace(s"After returning to $descr JSON is ${cursorToString(result)}")
      }

      result
    }

  object Id {

    def listOfString(
      name: String,
      value: Option[List[String]],
    ): IdFieldFormlet[Option[List[String]]] =
      listOfStringM(name, value)

    def listOfNumber(
      name: String,
      value: Option[List[JsonNumber]],
    ): IdFieldFormlet[Option[List[JsonNumber]]] =
      listOfNumberM(name, value)

    def listOfBoolean(
      name: String,
      value: Option[List[Boolean]],
    ): IdFieldFormlet[Option[List[Boolean]]] =
      listOfBooleanM(name, value)

    def listOfJson(
      name: String,
      value: Option[List[Json]],
    ): IdFieldFormlet[Option[List[Json]]] =
      listOfJsonM(name, value)

    def string(
      name: String,
      value: Option[String],
    ): IdFieldFormlet[Option[String]] =
      stringM(name, value)

    def json(
      name: String,
      value: Option[Json],
    ): IdFieldFormlet[Option[Json]] =
      jsonM(name, value)

    def number(
      name: String,
      value: Option[JsonNumber],
    ): IdFieldFormlet[Option[JsonNumber]] =
      numberM(name, value)

    def boolean(
      name: String,
      value: Option[Boolean],
    ): IdFieldFormlet[Option[Boolean]] =
      booleanM(name, value)

    def list[E, A](
      template: IdObjectFormlet[A],
      defaultValue: List[IdObjectFormlet[A]],
    ): JsonFormlet[Id, JsonArrayBuilder, ValidationErrors, List[A]] =
      listM[Id, A](template, defaultValue)

    def nested[A, V <: JsonBuilder](
      name: String,
      inner: JsonFormlet[Id, V, ValidationErrors, A],
    ): IdObjectFormlet[A] =
      nestedM[Id, A, V](name, inner)

    def nestedOptional[A, V <: JsonBuilder](
      name: String,
      inner: JsonFormlet[Id, V, ValidationErrors, A],
    ): IdObjectFormlet[Option[A]] =
      nestedOptionalM[Id, A, V](name, inner)
  }
}
