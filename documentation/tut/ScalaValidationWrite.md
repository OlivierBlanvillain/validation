# Serializing data

## Introduction

To serialize data, the validation API provides the `Write` type. A `Write[I, O]` defines a way to transform data, from type `I` to type `O`. It's basically a function `I => O`, where `I` is the type of the input to serialize, and `O` is the expected output type.

## A simple example

Let's say you want to serialize a `Float` to `String`.
All you need to do is to define a `Write` from `Float` to `String`:

```scala
scala> import play.api.data.mapping._
<console>:11: error: object mapping is not a member of package play.api.data
       import play.api.data.mapping._
                            ^
scala> def floatToString: Write[Float, String] = ???
<console>:11: error: not found: type Write
       def floatToString: Write[Float, String] = ???
                          ^
```

For now we'll not implement `floatToString`, actually the validation API comes with a number of built-in Writes, including `Writes.floatW[T]`.

All you have to do is import the default Writes.

```scala
scala> object Writes extends NumericTypes2StringWrites
<console>:11: error: not found: type NumericTypes2StringWrites
       object Writes extends NumericTypes2StringWrites
                             ^
scala> Writes.floatW
<console>:12: error: not found: value Writes
       Writes.floatW
       ^
```

Let's now test it against different `Float` values:

```scala
scala> Writes.floatW.writes(12.8F)
<console>:12: error: not found: value Writes
       Writes.floatW.writes(12.8F)
       ^
scala> Writes.floatW.writes(12F)
<console>:12: error: not found: value Writes
       Writes.floatW.writes(12F)
       ^
```

## Defining your own `Write`

Creating a new `Write` is almost as simple as creating a new function.
This example creates a new `Write` serializing a Float with a custom format.

```scala
scala> val currency = Write[Double, String]{ money =>
     |   import java.text.NumberFormat
     |   import java.util.Locale
     |   val f = NumberFormat.getCurrencyInstance(Locale.FRANCE)
     |   f.format(money)
     | }
<console>:11: error: not found: value Write
       val currency = Write[Double, String]{ money =>
                      ^
```

Testing it:

```scala
     | currency.writes(9.99)
```

## Composing Writes

Writes composition is very important in this API. `Write` composition means that given two writes `a: Write[I, J]` and `b: Write[J, O]`, we can create a new write `c: Write[I, O]`.

### Example

Let's see we're working working on a e-commerce website. We have defined a `Product` class.
Each product has a name and a price:

```scala
     | case class Product(name: String, price: Double)
```

Now we'd like to create a `Write[Product, String]` that serializes a product to a `String` of it price: `Product("demo", 123)` becomes `123,00 €`

We have already defined `currency: Write[Double, String]`, so we'd like to reuse that.
First, we'll create a `Write[Product, Double]` extracting the price of the product:

```scala
     | val productPrice = Write[Product, Double]{ _.price }
```

Now we just have to compose it with `currency`:

```scala
     | val productAsPrice: Write[Product,String] = productPrice compose currency
```

Let's test our new `Write`:

```scala
     | productAsPrice.writes(Product("Awesome product", 9.99))
```

> **Next:** [Complex serialization with Writes combinators](ScalaValidationWriteCombinators.md)
