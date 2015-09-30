val home = "https://github.com/jto/validation"
val repo = "git@github.com:jto/validation.git"
val org = "io.github.jto"
val license = ("Apache License", url("http://www.apache.org/licenses/LICENSE-2.0.txt"))

val catsVersion = "0.2.0"
val jodaConvertVersion = "1.3.1"
val jodaTimeVersion = "2.2"
val json4sVersion = "3.2.10"
val kindProjectorVersion = "0.6.3"
val paradiseVersion = "2.1.0-M5"
val parserCombinatorsVersion = "1.0.2"
val playVersion = "2.4.3"
val scalacVersion = "2.11.7"
val scalatestVersion = "3.0.0-M7"
val scalaXmlVersion = "1.0.5"
val shapelessVersion = "2.2.5"
// val specs2Version = "2.4.9"

val commonScalacOptions = Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
  "-language:reflectiveCalls", // Needed? TODO
  "-language:implicitConversions",
  "-language:experimental.macros",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xlint",
  "-Yinline-warnings",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Ywarn-unused-import",
  "-Xfuture"
)

val resolver = Seq(
  // Resolver.bintrayRepo("scalaz", "releases"),
  Resolver.sonatypeRepo("releases")
)

lazy val commonSettings = Seq(
  scalaVersion := scalacVersion,
  organization := org,
  scalacOptions ++= commonScalacOptions,
  resolvers ++= resolver,
  parallelExecution in Test := true
)

lazy val validationSettings = commonSettings ++ publishSettings ++ coreDependencies // ++  specsDependency

lazy val commonJsSettings = Seq(
  scalaJSStage in Global := FastOptStage
)

lazy val commonJvmSettings = Seq(
  // testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oDF)
)

lazy val root = project.in(file("."))
  .aggregate(coreJVM, json, formJVM, delimited, xml, json4s, experimental)
  .settings(validationSettings: _*)
  .settings(noPublishSettings: _*)

lazy val `validation-core` = crossProject.crossType(CrossType.Pure)
  .settings(validationSettings: _*)
  .settings(generateBoilerplate: _*)
lazy val coreJVM = `validation-core`.jvm
lazy val coreJS = `validation-core`.js

lazy val `validation-form` = crossProject.crossType(CrossType.Pure)
  .settings(validationSettings: _*)
  .dependsOn(`validation-core`)
  .jvmSettings(libraryDependencies +=
    "org.scala-lang.modules" %% "scala-parser-combinators" % parserCombinatorsVersion)
  .jsSettings(libraryDependencies +=
    "org.scala-js" %%% "scala-parser-combinators" % parserCombinatorsVersion)
lazy val formJVM = `validation-form`.jvm
lazy val formJS = `validation-form`.js

lazy val delimited = project.in(file("validation-delimited"))
  .settings(moduleName := "validation-delimited")
  .settings(validationSettings: _*)
  .dependsOn(coreJVM)

lazy val experimental = project.in(file("validation-experimental"))
  .settings(moduleName := "validation-experimental")
  .settings(validationSettings: _*)
  .dependsOn(coreJVM)

lazy val json = project.in(file("validation-json"))
  .settings(moduleName := "validation-json")
  .settings(validationSettings: _*)
  .settings(libraryDependencies +=
    "com.typesafe.play" %% "play-json" % playVersion)
  .dependsOn(coreJVM % "test->test;compile->compile")

lazy val json4s = project.in(file("validation-json4s"))
  .settings(moduleName := "validation-json4s")
  .settings(validationSettings: _*)
  .settings(libraryDependencies +=
    "org.json4s" %% "json4s-native" % json4sVersion)
  .dependsOn(coreJVM)
lazy val xml = project.in(file("validation-xml"))
  .settings(moduleName := "validation-xml")
  .settings(validationSettings: _*)
  .settings(libraryDependencies +=
    "org.scala-lang.modules" %% "scala-xml" % scalaXmlVersion)
  .dependsOn(coreJVM)

lazy val docs = project.in(file("validation-docs"))
  .settings(validationSettings: _*)
  .settings(noPublishSettings: _*)
  .settings(crossTarget := file(".") / "documentation")
  .settings(tutSettings: _*)
  .settings(scalacOptions -= "-Ywarn-unused-import")
  .dependsOn(coreJVM, json, json4s, formJVM, xml, experimental)

// lazy val specsDependency = libraryDependencies ++= Seq(
//   "org.specs2" %% "specs2" % "2.4.9" % "test",
//   "org.specs2" %% "specs2-junit" % "2.4.9" % "test")

lazy val coreDependencies = Seq(
  libraryDependencies ++= Seq(
    // "joda-time" % "joda-time" % jodaTimeVersion,
    // "org.joda" % "joda-convert" % jodaConvertVersion,
    "org.spire-math" %%% "cats" % catsVersion,
    "com.chuusai" %% "shapeless" % shapelessVersion,
    "org.scalatest" %%% "scalatest" % scalatestVersion % "test"
  ),
  addCompilerPlugin("org.spire-math" %% "kind-projector" % kindProjectorVersion),
  addCompilerPlugin("org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.full)
)

lazy val generateBoilerplate = Seq(
  sourceGenerators in Compile <+= (sourceManaged in Compile).map(Boilerplate.gen)
)

lazy val publishSettings = Seq(
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

lazy val noPublishSettings = Seq(
  publish := (),
  publishLocal := (),
  publishArtifact := false
)
