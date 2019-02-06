name := "json-formlets"

organization := "gov.wicourts"

version := "0.8.1"

scalaVersion := "2.12.6"
crossScalaVersions := Seq("2.11.12", "2.12.6")

resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.2.20" % "compile",
  "io.argonaut" %% "argonaut" % "6.2",
  "io.argonaut" %% "argonaut-scalaz" % "6.2",
  "org.slf4j" % "slf4j-api" % "1.7.25",
)

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2-core" % "3.8.6" % "test",
  "org.specs2" %% "specs2-matcher-extra" % "3.8.6" % "test",
  "org.specs2" %% "specs2-scalacheck" % "3.8.6" % "test",
  "org.scalaz" %% "scalaz-scalacheck-binding" % "7.2.7-scalacheck-1.13" % "test",
  "org.slf4j" % "slf4j-simple" % "1.7.25" % "test"
)

// https://tpolecat.github.io/2014/04/11/scalac-flags.html
// https://tpolecat.github.io/2017/04/25/scalac-flags.html

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",       // yes, this is 2 args
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
  "-Yno-predef"   // no automatic import of Predef (removes irritating implicits)
) ++ (CrossVersion.partialVersion(scalaVersion.value) match {
  case Some((2, scalaMajor)) if scalaMajor == 11 =>
    Seq("-Ywarn-unused-import")
  case Some((2, scalaMajor)) if scalaMajor >= 12 =>
    Seq(
      "-Xlint:constant",
      "-Ywarn-extra-implicit",
      "-Ywarn-unused:implicits",
      "-Ywarn-unused:imports",
//      "-Ywarn-unused:locals", // gives an incorrect warning in Forms.scala
      "-Ywarn-unused:params",
      "-Ywarn-unused:patvars",
      "-Ywarn-unused:privates"
    )
  case _ =>
    Seq.empty
})

//scalacOptions in Test ++= Seq("-Yrangepos")

licenses += ("MIT", url("http://opensource.org/licenses/MIT"))

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")
