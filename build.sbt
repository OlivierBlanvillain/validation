val home = "https://github.com/jto/validation"
val repo = "git@github.com:jto/validation.git"
val org = "io.github.jto"
val license = ("Apache License", url("http://www.apache.org/licenses/LICENSE-2.0.txt"))

val catsVersion = "0.2.0"
val json4sVersion = "3.2.10"
val jodaConvertVersion = "1.3.1"
val jodaTimeVersion = "2.2"
val kindProjectorVersion = "0.6.3"
val paradiseVersion = "2.1.0-M5"
val parserCombinatorsVersion = "1.0.2"
val playVersion = "2.4.3"
val scalaXmlVersion = "1.0.5"
val shapelessVersion = "2.2.5"
val specs2Version = "2.4.9"
val scalacVersion = "2.11.7"

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
  // Resolver.sonatypeRepo("snapshots"),
  Resolver.bintrayRepo("scalaz", "releases"),
  Resolver.sonatypeRepo("releases")
  // Resolver.typesafeRepo("releases")
)

lazy val commonSettings = Seq(
  scalaVersion := scalacVersion,
  organization := org,
  scalacOptions ++= commonScalacOptions,
  resolvers ++= resolver,
  parallelExecution in Test := true,
  fork in Test := true
)

lazy val validationSettings = commonSettings ++ publishSettings ++ specsDependency

// lazy val commonJsSettings = Seq(
//   scalaJSStage in Global := FastOptStage
// )

// lazy val commonJvmSettings = Seq(
//   // testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oDF)
// )

lazy val root = project.in(file("."))
  .aggregate(core, json, form, delimited, xml, json4s, experimental)
  .settings(validationSettings: _*)
  .settings(noPublishSettings: _*)

lazy val core = project.in(file("validation-core"))
  .settings(moduleName := "validation-core")
  .settings(validationSettings: _*)
  .settings(coreDependencies: _*)
  .settings(generateBoilerplate: _*)

lazy val json = project.in(file("validation-json"))
  .settings(moduleName := "validation-json")
  .settings(validationSettings: _*)
  .settings(playDependency: _*)
  .dependsOn(core % "test->test;compile->compile")

lazy val json4s = project.in(file("validation-json4s"))
  .settings(moduleName := "validation-json4s")
  .settings(validationSettings: _*)
  .settings(json4sDependency: _*)
  .dependsOn(core)

lazy val form = project.in(file("validation-form"))
  .settings(moduleName := "validation-form")
  .settings(validationSettings: _*)
  .dependsOn(core)

lazy val delimited = project.in(file("validation-delimited"))
  .settings(moduleName := "validation-delimited")
  .settings(validationSettings: _*)
  .dependsOn(core)

lazy val xml = project.in(file("validation-xml"))
  .settings(moduleName := "validation-xml")
  .settings(validationSettings: _*)
  .settings(xmlDependency: _*)
  .dependsOn(core)

lazy val experimental = project.in(file("validation-experimental"))
  .settings(moduleName := "validation-experimental")
  .settings(validationSettings: _*)
  .dependsOn(core)

lazy val docs = project.in(file("validation-docs"))
  .settings(validationSettings: _*)
  .settings(noPublishSettings: _*)
  .settings(crossTarget := file(".") / "documentation")
  .settings(tutSettings: _*)
  .settings(scalacOptions -= "-Ywarn-unused-import")
  .dependsOn(core, json, json4s, form, xml, experimental)

lazy val specsDependency = libraryDependencies ++= Seq(
  "org.specs2" %% "specs2" % "2.4.9" % "test",
  "org.specs2" %% "specs2-junit" % "2.4.9" % "test")

lazy val xmlDependency = libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.5"

lazy val json4sDependency = libraryDependencies += "org.json4s" %% "json4s-native" % json4sVersion

lazy val playDependency = libraryDependencies += "com.typesafe.play" %% "play-json" % playVersion

lazy val coreDependencies = Seq(
  libraryDependencies ++= Seq(
    "joda-time" % "joda-time" % "2.2",
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2",
    "org.joda" % "joda-convert" % "1.3.1",
    "org.spire-math" %% "cats" % "0.2.0",
    "com.chuusai" %% "shapeless" % "2.2.5"
  ),
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.6.3"),
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
