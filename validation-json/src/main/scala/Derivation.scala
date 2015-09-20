package jto.validation
package json

import Rules._
import Writes._
import shapeless.{Path => _, _}
import play.api.libs.json.{JsValue, JsObject, Json}
import labelled._
import cats.Monoid

object TypePath {
  val typePath = Path \ "$type"
}
import TypePath._

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

trait WriteProduct extends WriteDeps {
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

trait WriteCoproduct extends WriteDeps {
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

trait WriteGeneric extends WriteDeps {
  type Output
  
  implicit def writeGeneric[F, G]
    (implicit
      gen: LabelledGeneric.Aux[F, G],
      sg: Lazy[Path => WriteLike[G, Output]]
    ) =
    new WriteLike[F, Output] {
      def writes(i: F): Output = {
        sg.value(Path).writes(gen.to(i))
      }
    }
}

object DeriveJsonCoproduct
  extends WriteProduct
  with WriteCoproduct
  with WriteGeneric
  with RuleProduct
  with RuleCoproduct
  with RuleGeneric
  { type Output = JsObject }

object DeriveJsonProduct
  extends WriteProduct
  with WriteGeneric
  with RuleProduct
  with RuleGeneric
  { type Output = JsObject }

sealed trait Animal
case class Cat(name: String, fish: Int, friend: Dog) extends Animal
case class Dog(name: String, bones: Int) extends Animal

case class Dogception(animal: Animal, dogception: Option[Dogception])

object main extends App {
  val catJson = Json.parse("""
    {
      "name": "miam",
      "fish": 3,
      "friend": {
        "name": "doge",
        "bones": 0
      }
    }
  """)
  
  val dogJson = Json.parse("""
    {
      "name": "doge",
      "bones": 0
    }
  """).as[JsObject]

  val dogceptionJson = Json.parse("""
    {
      "animal": {
        "$type": "Dog",
        "name": "doge",
        "bones": 0
      },
      "dogception": {
        "$type": "Some",
        "x": {
          "animal": {
            "$type": "Dog",
            "name": "nested doge",
            "bones": 1
          },
          "dogception": {
            "$type": "None"
          }
        }
      }
    }
  """).as[JsObject]
  
  val dog = Dog("doge", 0)
  val cat = Cat("miaou", 1, dog)
  
  val (genRuleJsValue2Dog, genRuleJsObject2Dog, genWriteDog2JsObject) = (
    Rule.gen[JsValue, Dog],
    Rule.gen[JsObject, Dog],
    Write.gen[Dog, JsObject]
  )
  
  val (derRuleJsValue2Dog, derRuleJsObject2Dog, derWriteDog2JsObject) = {
    import DeriveJsonCoproduct._
    (
      implicitly[RuleLike[JsObject, Dog]],
      implicitly[RuleLike[JsValue, Dog]],
      implicitly[WriteLike[Dog, JsObject]]
    )
  }
  
  def test[A](arg1: A, arg2: A): Unit = {
    if(arg1 == arg2)
      println(arg1)
    else
      throw new Exception(s"""
      |  arg1: $arg1
      |  arg2: $arg2""".stripMargin)
  }
  
  test(genRuleJsValue2Dog.validate(dogJson), derRuleJsValue2Dog.validate(dogJson))
  test(genRuleJsObject2Dog.validate(dogJson), derRuleJsObject2Dog.validate(dogJson))
  test(genWriteDog2JsObject.writes(dog), derWriteDog2JsObject.writes(dog))
  
  {
    import DeriveJsonCoproduct._
    val r = implicitly[RuleLike[JsValue, Animal]]
    val w = implicitly[WriteLike[Animal, JsObject]]
    List[Animal](dog, cat) foreach { animal =>
      test(animal, r.validate(w.writes(animal)).toOption.get)
    }
  }
  
  {
    case class WithOptions(i: Int, os: Option[String])
    import DeriveJsonProduct._
    val wo = WithOptions(1, Some("ksjdf"))
    val json = implicitly[WriteLike[WithOptions, JsObject]].writes(wo)
    test(wo, implicitly[RuleLike[JsValue, WithOptions]].validate(json).toOption.get)
  }
}
