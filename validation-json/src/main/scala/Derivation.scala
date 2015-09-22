package jto.validation
package json

import jto.validation.json.Rules._
import jto.validation.json.Writes._
import shapeless.{Path => _, _}

import shapeless.labelled._
import cats.Monoid

import play.api.libs.json.{JsValue, JsObject, Json}

// Derivation of RuleLike

trait RuleProduct {
  implicit def ruleProductBaseCase[I](p: Path): RuleLike[I, HNil] =
    new RuleLike[I, HNil] {
      def validate(i: I): VA[HNil] = Valid(HNil)
    }

  implicit def ruleProductInductionStep[I, K <: Symbol, V, T <: HList](p: Path)
    (implicit
      key: Witness.Aux[K],
      sv: Lazy[Path => RuleLike[I, V]],
      st: Lazy[Path => RuleLike[I, T]]
    ) =
    new RuleLike[I, FieldType[K, V] :: T] {
      def validate(i: I): VA[FieldType[K, V] :: T] = {
        val pathed = From[I] { __ =>
          (__ \ key.value.name).read[V](sv.value)
        }
        val head = pathed.validate(i)
        val tail = st.value(p).validate(i).map((t: T) => (v: V) => field[K](v) :: t)
        head.ap(tail)
      }
    }
}

trait RuleCoproduct {
  def typePath: Path
  
  implicit def ruleCoproductBaseCase[I](p: Path): RuleLike[I, CNil] =
    new RuleLike[I, CNil] {
      def validate(i: I): VA[CNil] =
        Invalid(Seq((typePath, Seq(ValidationError("Unknown $type")))))
    }
  
  implicit def ruleCoproductInductionStep[I, K <: Symbol, V, T <: Coproduct](p: Path)
    (implicit
      key: Witness.Aux[K],
      sv: Lazy[Path => RuleLike[I, V]],
      st: Lazy[Path => RuleLike[I, T]],
      rl: Path => RuleLike[I, String]
    ) =
    new RuleLike[I, FieldType[K, V] :+: T] {
      def validate(i: I): VA[FieldType[K, V] :+: T] = {
        typePath.read[I, String](rl).validate(i) match {
          case Valid(key.value.name) =>
            sv.value(p).validate(i).map(v => Inl(field[K](v)))
          case Valid(_) =>
            st.value(p).validate(i).map(Inr.apply)
          case _ =>
            Invalid(Seq((typePath, Seq(ValidationError("Missing $type")))))
        }
      }
    }
}

trait RuleGeneric {
  implicit def ruleGeneric[I, F, G]
    (implicit
      gen: LabelledGeneric.Aux[F, G],
      sg: Lazy[Path => RuleLike[I, G]]
    ) =
    new RuleLike[I, F] {
      def validate(i: I): VA[F] = {
        sg.value(Path).validate(i).map(gen.from)
      }
    }
}

// Derivation of WriteLike

trait WriteProduct {
  type Output
  
  implicit def writeProductBaseCase(p: Path)(implicit m: Monoid[Output]) =
    new WriteLike[HNil, Output] {
      def writes(i: HNil): Output = m.empty
    }
  
  implicit def writeProductInductionStep[K <: Symbol, V, T <: HList](p: Path)
    (implicit
      key: Witness.Aux[K],
      sv: Lazy[Path => WriteLike[V, Output]],
      st: Lazy[Path => WriteLike[T, Output]],
      m: Monoid[Output]
    ) =
    new WriteLike[FieldType[K, V] :: T, Output] {
      def writes(i: FieldType[K, V] :: T): Output = {
        val pathed = To[Output] { __ =>
          (__ \ key.value.name).write[V](sv.value)
        }
        val head = pathed.writes(i.head)
        val tail = st.value(p).writes(i.tail)
        m.combine(head, tail)
      }
    }
}

trait WriteCoproduct {
  def typePath: Path
  type Output
  
  implicit val writeCoproductBaseCase =
    new WriteLike[CNil, Output] {
      def writes(i: CNil): Output = ???
    }
  
  implicit def writeCoproductInductionStep[K <: Symbol, V, T <: Coproduct](p: Path)
    (implicit
      key: Witness.Aux[K],
      sv: Lazy[Path => WriteLike[V, Output]],
      st: Lazy[Path => WriteLike[T, Output]],
      wl: Path => WriteLike[String, Output],
      m: Monoid[Output]
    ) =
    new WriteLike[FieldType[K, V] :+: T, Output] {
      def writes(i: FieldType[K, V] :+: T): Output = {
        i match {
          case Inl(v) => 
            val typeInfo = typePath.write(wl).writes(key.value.name)
            val value = sv.value(p).writes(v)
            m.combine(value, typeInfo)
          case Inr(t) =>
            st.value(p).writes(t)
        }
      }
    }
}

trait WriteGeneric {
  implicit def writeGeneric[O, F, G]
    (implicit
      gen: LabelledGeneric.Aux[F, G],
      sg: Lazy[Path => WriteLike[G, O]]
    ) =
    new WriteLike[F, O] {
      def writes(i: F): O = {
        sg.value(Path).writes(gen.to(i))
      }
    }
}

// Syntax

object DRule {
  def derive[I, F, G]
    (implicit
      gen: LabelledGeneric.Aux[F, G],
      sg: Lazy[Path => RuleLike[I, G]]
    ): RuleLike[I, F] =
      new RuleGeneric{}.ruleGeneric
}

object DWrite {
  def derive[O, F, G]
    (implicit
      gen: LabelledGeneric.Aux[F, G],
      sg: Lazy[Path => WriteLike[G, O]]
    ): WriteLike[F, O] =
      new WriteGeneric{}.writeGeneric
}

trait JsonStuff {
  type Output = JsObject
  val typePath = Path \ "$type"
}

object Derive
  extends WriteProduct
  with WriteCoproduct
  with RuleProduct
  with RuleCoproduct
  with JsonStuff

object DeriveReq
  extends WriteProduct
  with WriteCoproduct
  with WriteGeneric
  with RuleProduct
  with RuleCoproduct
  with RuleGeneric
  with JsonStuff

object DeriveProductReq
  extends WriteProduct
  with WriteGeneric
  with RuleProduct
  with RuleGeneric
  with JsonStuff

object Test extends App {
  import shapeless.test.illTyped
  
  def test[A](arg1: A, arg2: A): Unit =
    if(arg1 == arg2)
      println(arg1)
    else
      throw new Exception(s"""
      |  arg1: $arg1
      |  arg2: $arg2""".stripMargin)
  
  def test[A, AA <: A, T](r: RuleLike[A, T], w: WriteLike[T, AA])(v: T): Unit =
    test(Some(v), r.validate(w.writes(v)).toOption)
  
  def test[I, O](r1: RuleLike[I, O], r2: RuleLike[I, O])(i: I): Unit =
    test(r1.validate(i), r2.validate(i))

  def test[I, O](w1: WriteLike[I, O], w2: WriteLike[I, O])(i: I): Unit =
    test(w1.writes(i), w2.writes(i))
  
  sealed trait Animal
  case class Cat(name: String, fish: Int, friend: Dog) extends Animal
  case class Dog(name: String, bones: Int) extends Animal
  case class WithOptions(i: Int, os: Option[String])
  
  val dogJson = Json.parse("""
    {
      "name": "doge",
      "bones": 0
    }
  """).as[JsObject]
  
  val dog = Dog("doge", 0)
  val cat = Cat("miaou", 1, dog)
  
  "Derivated instance for Dog behaves the same as macro generated ones"; {
    import Derive._
    
    test(
      Rule.gen[JsObject, Dog],
      DRule.derive: RuleLike[JsObject, Dog]
    )(dogJson)

    test(
      Rule.gen[JsValue, Dog],
      DRule.derive: RuleLike[JsValue, Dog]
    )(dogJson)
    
    test(
      Write.gen[Dog, JsObject],
      DWrite.derive: WriteLike[Dog, JsObject]
    )(dog)
  }
  
  "Non recursive derivation fails when generation with macro fails"; {
    import Derive._
    
    illTyped("Rule.gen[JsValue, Cat]")
    illTyped("DRule.derive: RuleLike[JsValue, Cat]")
    
    illTyped("Write.gen[Cat, JsObject]")
    illTyped("DWrite.derive: WriteLike[Cat, JsObject]")

    illTyped("Rule.gen[JsValue, Animal]")
    illTyped("DRule.derive: RuleLike[JsValue, Animal]")
    
    illTyped("Write.gen[Animal, JsObject]")
    illTyped("DWrite.derive: WriteLike[Animal, JsObject]")
  }
  
  "Recursive product derivation handles nested case classes"; {
    import DeriveReq._
    
    test(
      implicitly[RuleLike[JsValue, Cat]],
      implicitly[WriteLike[Cat, JsObject]]
    )(cat)
  }

  "Coproduct derivation handles ADTs"; {
    import DeriveReq._
    
    illTyped("Rule.gen[JsValue, Animal]")
    illTyped("Write.gen[Animal, JsObject]")
    
    List(dog, cat) foreach test(
      implicitly[RuleLike[JsValue, Animal]],
      implicitly[WriteLike[Animal, JsObject]])
  }
  
  "Derivation instance with Option behaves the same as macro generated ones"; {
    import Derive._
    
    val wo = WithOptions(1, Some("ksjdf"))
    val woJson = Json.parse("""{"i": 1, "os": "ksjdf"}""")
    val woNone = WithOptions(1, None)
    val woNoneJson = Json.parse("""{"i": 1}""")
    
    List(wo, woNone) foreach test(
      Write.gen[WithOptions, JsObject],
      DWrite.derive: WriteLike[WithOptions, JsObject])
    
    List(woJson, woNoneJson) foreach test(
      Rule.gen[JsValue, WithOptions],
      DRule.derive: RuleLike[JsValue, WithOptions])
  }
}
