# The unified data validation library

## Overview

The unified validation API aims to provide a comprehensive toolkit to validate data from any format against user defined rules, and transform them to other types.

Basically, assuming you have this:

```scala
import play.api.libs.json._
import play.api.data.mapping._

case class Person(name: String, age: Int, lovesChocolate: Boolean)

val json = Json.parse("""{
  "name": "Julien",
  "age": 28,
  "lovesChocolate": true
}""")

implicit val personRule = {
  import play.api.data.mapping.json.Rules._
  Rule.gen[JsValue, Person]
}
```

It can do this:

```scala
scala> personRule.validate(json)
res0: play.api.data.mapping.VA[Person] = Valid(Person(Julien,28,true))
```

> **BUT IT'S NOT LIMITED TO JSON**

It's also a unification of play's [Form Validated API](https://www.playframework.com/documentation/2.3.x/ScalaForms), and its [Json validation API](https://www.playframework.com/documentation/2.3.x/ScalaJsonCombinators).

Being based on the same concepts as play's Json validation API, it should feel very similar to any developer already working with it. The unified validation API is, rather than a totally new design, a simple generalization of those concepts.

## Design

The unified validation API is designed around a core defined in package `play.api.data.mapping`, and "extensions". Each extension provides primitives to validate and serialize data from / to a particular format ([Json](documentation/tut/ScalaValidatedJson.md), [form encoded request body](documentation/tut/ScalaValidatedMigrationForm.md), etc.). See [the extensions documentation](documentation/tut/ScalaValidatedExtensions.md) for more information.

To learn more about data validation, please consult [Validated and transformation with Rule](documentation/tut/ScalaValidatedRule.md), for data serialization read [Serialization with Write](documentation/tut/ScalaValidatedWrite.md). If you just want to figure all this out by yourself, please see the [Cookbook](documentation/tut/ScalaValidatedCookbook.md).

## Using the validation api in your project

Add the following lines in your `build.sbt`

```scala
resolvers += Resolver.sonatypeRepo("releases")

// If you want only the core
libraryDependencies +="io.github.jto" %% "validation-core" % "1.1"

// Json validation
libraryDependencies +="io.github.jto" %% "validation-json" % "1.1"

// Form validation
libraryDependencies +="io.github.jto" %% "validation-form" % "1.1"

// ...
```

## Play dependency

The 1.1.X versions are builded with play 2.4, if you are using play 2.3 you need to use the 1.0.2 version.

## Documentation

All the required documentation is directly readable from Github: https://github.com/jto/validation/tree/master/documentation/tut

- [Validating and transforming data](documentation/tut/ScalaValidatedRule.md)
- [Combining Rules](documentation/tut/ScalaValidatedRuleCombinators.md)
- [Validated Inception](documentation/tut/ScalaValidatedMacros.md)
- [Validating Json](documentation/tut/ScalaValidatedJson.md)
- [Serializing data with Write](documentation/tut/ScalaValidatedWrite.md)
- [Combining Writes](documentation/tut/ScalaValidatedWriteCombinators.md)
- [Play's Form API migration](documentation/tut/ScalaValidatedMigrationForm.md)
- [Play's Json API migration](documentation/tut/ScalaValidatedMigrationJson.md)
- [Extensions: Supporting new types](documentation/tut/ScalaValidatedExtensions.md)
- [Cookbook](documentation/tut/ScalaValidatedCookbook.md)

## Contributors

- Julien Tournay - http://jto.github.io
- Nick - https://github.com/stanch
- Ian Hummel - https://github.com/themodernlife
- Arthur Gautier - https://github.com/baloo
- Jacques B - https://github.com/Timshel
- Alexandre Tamborrino - https://github.com/atamborrino
