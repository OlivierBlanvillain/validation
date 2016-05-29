val home = "https://github.com/jto/validation"
val repo = "git@github.com:jto/validation.git"
val org = "io.github.jto"
val license = ("Apache License", url("http://www.apache.org/licenses/LICENSE-2.0.txt"))

val catsVersion = "0.6.0"
val jodaConvertVersion = "1.8.1"
val jodaTimeVersion = "2.9.4"
val json4sAstVersion = "4.0.0-M1"
val kindProjectorVersion = "0.7.1"
val parserCombinatorsVersion = "1.0.2"
val playVersion = "2.5.3"
val scalacVersion = "2.11.8"
val scalatestVersion = "3.0.0-M16-SNAP6"
val scalaXmlVersion = "1.0.5"
val simulacrumVersion = "0.7.0"
val si2712fixVersion = "1.2.0"
val paradiseVersion = "2.1.0"

libraryDependencies += "com.github.mpilquist" %% "simulacrum" % "0.7.0"

val json4sAST = libraryDependencies += "org.json4s" %%% "json4s-ast" % json4sAstVersion

lazy val root = aggregate("validation", validationJVM, validationJS, docs).in(file("."))
lazy val validationJVM = aggregate("validationJVM", coreJVM, formJVM, delimitedJVM, json4sJVM, `validation-playjson`, `validation-xml`, `date-tests`)
lazy val validationJS = aggregate("validationJS", coreJS, formJS, delimitedJS, json4sJS, `validation-jsjson`)
lazy val play = aggregate("play", coreJVM, `validation-playjson`)

lazy val `validation-core` = crossProject
  .crossType(CrossType.Pure)
  .settings(validationSettings: _*)
  .settings(generateBoilerplate: _*)
lazy val coreJVM = `validation-core`.jvm
lazy val coreJS = `validation-core`.js
lazy val core = aggregate("validation-core", coreJVM, coreJS)

lazy val `validation-form` = crossProject
  .crossType(CrossType.Pure)
  .settings(validationSettings: _*)
  .jvmSettings(libraryDependencies +=
    "org.scala-lang.modules" %% "scala-parser-combinators" % parserCombinatorsVersion)
  .jsSettings(libraryDependencies +=
    "org.scala-js" %%% "scala-parser-combinators" % parserCombinatorsVersion)
  .dependsOn(`validation-core`)
lazy val formJVM = `validation-form`.jvm
lazy val formJS = `validation-form`.js
lazy val form = aggregate("validation-form", formJVM, formJS)

lazy val `validation-delimited` = crossProject
  .crossType(CrossType.Pure)
  .settings(validationSettings: _*)
  .dependsOn(`validation-core`)
lazy val delimitedJVM = `validation-delimited`.jvm
lazy val delimitedJS = `validation-delimited`.js
lazy val delimited = aggregate("validation-delimited", delimitedJVM, delimitedJS)

lazy val `validation-json4s` = crossProject
  .crossType(CrossType.Pure)
  .settings(validationSettings: _*)
  .settings(json4sAST)
  .dependsOn(`validation-core`)
lazy val json4sJVM = `validation-json4s`.jvm
lazy val json4sJS = `validation-json4s`.js
lazy val json4s = aggregate("validation-json4s", json4sJVM, json4sJS)

lazy val `validation-playjson` = project
  .settings(validationSettings: _*)
  .settings(libraryDependencies +=
    "com.typesafe.play" %% "play-json" % playVersion)
  .dependsOn(coreJVM)

lazy val `validation-xml` = project
  .settings(validationSettings: _*)
  .settings(libraryDependencies +=
    "org.scala-lang.modules" %% "scala-xml" % scalaXmlVersion)
  .dependsOn(coreJVM)

lazy val `validation-jsjson` = project
  .enablePlugins(ScalaJSPlugin)
  .settings(validationSettings: _*)
  .dependsOn(coreJS)

lazy val docs = project
  .settings(validationSettings: _*)
  .settings(dontPublish: _*)
  .settings(crossTarget := file(".") / "docs" / "target")
  .settings(tutSettings: _*)
  .settings(scalacOptions -= "-Ywarn-unused-import")
  .dependsOn(coreJVM, formJVM, delimitedJVM, json4sJVM, `validation-playjson`, `validation-xml`)

lazy val `date-tests` = project
  .settings(validationSettings: _*)
  .settings(dontPublish: _*)
  .settings(json4sAST)
  .dependsOn(coreJVM, formJVM, json4sJVM, `validation-playjson`, `validation-xml`)

def aggregate(name: String, projects: ProjectReference*): Project =
  Project(name, file("." + name))
    .aggregate(projects: _*)
    .settings(validationSettings: _*)
    .settings(dontPublish: _*)

lazy val validationSettings = settings ++ dependencies ++ doPublish ++ scoverageSettings

lazy val settings = Seq(
  scalaVersion := scalacVersion,
  organization := org,
  scalacOptions ++= commonScalacOptions,
  resolvers ++= commonResolvers,
  testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oDF"),
  scalaJSStage in Global := FastOptStage,
  parallelExecution := false
)

val commonScalacOptions = Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
  "-language:experimental.macros",
  "-language:postfixOps",
  "-unchecked",
  // "-Xfatal-warnings",
  "-Xlint",
  "-Yinline-warnings",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Ywarn-unused-import",
  "-Xfuture"
)

val commonResolvers = Seq(
  Resolver.sonatypeRepo("snapshots"),
  Resolver.sonatypeRepo("releases")
)

val dependencies = Seq(libraryDependencies ++= Seq(
  "org.typelevel" %%% "cats" % catsVersion,
  "org.scalatest" %%% "scalatest" % scalatestVersion % "test",
  "joda-time" % "joda-time" % jodaTimeVersion,
  "org.joda" % "joda-convert" % jodaConvertVersion,
  compilerPlugin("org.spire-math" %% "kind-projector" % kindProjectorVersion),
  compilerPlugin("com.milessabin" % "si2712fix-plugin" % si2712fixVersion cross CrossVersion.full)
))

val generateBoilerplate = Seq(
  sourceGenerators in Compile <+= (sourceManaged in Compile).map(Boilerplate.gen)
)

val doPublish = Seq(
  homepage := Some(url(home)),
  scmInfo :=  Some(ScmInfo(url(home), "scm:git:" + repo)),
  licenses := Seq(license),
  publishMavenStyle := true,
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases"  at nexus + "service/local/staging/deploy/maven2")
  },
  pomExtra := (
    <developers>
      <developer>
        <id>jto</id>
        <name>Julien Tournay</name>
        <url>http://jto.github.io</url>
      </developer>
    </developers>)
)

val dontPublish = Seq(
  publish := (),
  publishLocal := (),
  publishArtifact := false
)

lazy val scoverageSettings = Seq(
  scoverage.ScoverageKeys.coverageExcludedPackages := """jto\.validation\.jsjson\..*""""
)
