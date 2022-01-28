name := "json-formlets"

organization := "gov.wicourts"

version := "0.9.0"

scalaVersion := "2.12.11"

libraryDependencies ++= Seq(
  "io.argonaut" %% "argonaut" % "6.2.5",
  "io.argonaut" %% "argonaut-cats" % "6.2.5",
  "org.slf4j" % "slf4j-api" % "1.7.25",
  "org.typelevel" %% "cats-core" % "2.7.0",
)

libraryDependencies ++= Seq(
  "com.github.alexarchambault" %% "scalacheck-shapeless_1.14" % "1.2.3" % Test,
  "io.chrisdavenport" %% "cats-scalacheck" % "0.3.1" % Test,
  "org.slf4j" % "slf4j-simple" % "1.7.25" % Test,
  "org.specs2" %% "specs2-core" % "4.13.2" % Test,
  "org.specs2" %% "specs2-scalacheck" % "4.13.2" % Test,
  "org.typelevel" %% "cats-laws" % "2.7.0" % Test,
  "org.typelevel" %% "discipline-specs2" % "1.3.1" % Test,
)

// https://tpolecat.github.io/2014/04/11/scalac-flags.html
// https://tpolecat.github.io/2017/04/25/scalac-flags.html

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding",
  "UTF-8", // yes, this is 2 args
  "-feature",
  "-unchecked",
  "-Ypartial-unification",
  "-Xfatal-warnings",
  "-Xfuture",
  "-Xlint:adapted-args",
  "-Xlint:by-name-right-associative",
  "-Xlint:delayedinit-select",
  "-Xlint:doc-detached",
  "-Xlint:inaccessible",
  "-Xlint:infer-any",
  "-Xlint:missing-interpolator",
  "-Xlint:nullary-override",
  "-Xlint:nullary-unit",
  "-Xlint:option-implicit",
  "-Xlint:package-object-classes",
  "-Xlint:poly-implicit-overload",
  "-Xlint:private-shadow",
  "-Xlint:stars-align",
  "-Xlint:type-parameter-shadow",
  "-Xlint:unsound-match",
  // "-Xlint:unused", same as all the -Ywarn-unused:*
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-inaccessible",
  "-Ywarn-infer-any",
  "-Ywarn-nullary-override",
  "-Ywarn-nullary-unit",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Yno-predef", // no automatic import of Predef (removes irritating implicits)
  "-Xlint:constant",
  "-Ywarn-extra-implicit",
  "-Ywarn-unused:implicits",
  "-Ywarn-unused:imports",
//      "-Ywarn-unused:locals", // gives an incorrect warning in Forms.scala
  "-Ywarn-unused:params",
  "-Ywarn-unused:patvars",
  "-Ywarn-unused:privates",
)

//scalacOptions in Test ++= Seq("-Yrangepos")

licenses += ("MIT", url("http://opensource.org/licenses/MIT"))

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3")
