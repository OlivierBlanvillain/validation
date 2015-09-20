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
  implicit def ruleProductBaseCase[I]: RuleLike[I, HNil] =
    new RuleLike[I, HNil] {
      def validate(i: I): VA[HNil] = Valid(HNil)
    }

  def ruleProductInductionStep[J, I <: J, K <: Symbol, V, T <: HList]
    (rl: Path => RuleLike[I, J])
    (implicit
      key: Witness.Aux[K],
      sv: Lazy[RuleLike[J, V]],
      st: Lazy[RuleLike[I, T]]
    ): RuleLike[I, FieldType[K, V] :: T] =
      new RuleLike[I, FieldType[K, V] :: T] {
        def validate(i: I): VA[FieldType[K, V] :: T] = {
          val pathed = From[I] { __ =>
            (__ \ key.value.name).read[J, V](sv.value)(rl)
          }
          val head = pathed.validate(i)
          val tail = st.value.validate(i).map((t: T) => (v: V) => field[K](v) :: t)
          head ap tail
        }
      }
}

trait RuleCoproduct {
  implicit def ruleCoproductBaseCase[I]: RuleLike[I, CNil] =
    new RuleLike[I, CNil] {
      def validate(i: I): VA[CNil] =
        Invalid(Seq((Path, Seq(ValidationError("meh")))))
    }
  
  implicit def ruleCoproductInductionStep[I, K <: Symbol, V, T <: Coproduct]
    (implicit
      key: Witness.Aux[K],
      sv: Lazy[RuleLike[I, V]],
      st: Lazy[RuleLike[I, T]],
      rl: Path => RuleLike[I, String]
    ): RuleLike[I, FieldType[K, V] :+: T] =
      new RuleLike[I, FieldType[K, V] :+: T] {
        def validate(i: I): VA[FieldType[K, V] :+: T] = {
          typePath.read[I, String](rl).validate(i) match {
            case Valid(key.value.name) =>
              sv.value.validate(i).map(v => Inl(field[K](v)))
            case Valid(_) =>
              st.value.validate(i).map(Inr.apply)
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
      sg: Lazy[RuleLike[I, G]]
    ): RuleLike[I, F] =
      new RuleLike[I, F] {
        def validate(i: I): VA[F] =
          sg.value.validate(i).map(gen.from)
      }
}

object DeriveRule extends RuleProduct with RuleCoproduct with RuleGeneric

trait WriteProduct {
  def writeProductBaseCase[O](m: Monoid[O]): WriteLike[HNil, O] =
    new WriteLike[HNil, O] {
      def writes(i: HNil): O = m.empty
    }
  
  def writeProductInductionStep[J, O <: J, K <: Symbol, V, T <: HList]
    (pw: Path => WriteLike[J, O], m: Monoid[O])
    (implicit
      key: Witness.Aux[K],
      sv: Lazy[WriteLike[V, J]],
      st: Lazy[WriteLike[T, O]]
    ): WriteLike[FieldType[K, V] :: T, O] =
      new WriteLike[FieldType[K, V] :: T, O] {
        def writes(i: FieldType[K, V] :: T): O = {
          val pathed = To[O] { __ =>
            (__ \ key.value.name).write[V, J](sv.value)(pw)
          }
          val head = pathed.writes(i.head)
          val tail = st.value.writes(i.tail)
          m.combine(head, tail)
        }
      }
}

trait WriteCoproduct {
  implicit def writeCoproductBaseCase[O]: WriteLike[CNil, O] =
    new WriteLike[CNil, O] {
      def writes(i: CNil): O = ???
    }
  
  def writeCoproductInductionStep[O, K <: Symbol, V, T <: Coproduct]
    (m: Monoid[O])
    (implicit
      key: Witness.Aux[K],
      sv: Lazy[WriteLike[V, O]],
      st: Lazy[WriteLike[T, O]],
      wl: Path => WriteLike[String, O]
    ): WriteLike[FieldType[K, V] :+: T, O] =
      new WriteLike[FieldType[K, V] :+: T, O] {
        def writes(i: FieldType[K, V] :+: T): O = {
          i match {
            case Inl(v) => 
              val typeInfo = typePath.write(wl).writes(key.value.name)
              val value = sv.value.writes(v)
              m.combine(value, typeInfo)
            case Inr(t) =>
              st.value.writes(t)
          }
        }
      }
}

trait WriteGeneric {
  implicit def writeGeneric[O, F, G]
    (implicit
      gen: LabelledGeneric.Aux[F, G],
      sg: Lazy[WriteLike[G, O]]
    ): WriteLike[F, O] =
      new WriteLike[F, O] {
        def writes(i: F): O = sg.value.writes(gen.to(i))
      }
}

object DeriveWrite extends WriteProduct with WriteCoproduct with WriteGeneric

object Static {
  val ruleJsObjectJsValue = implicitly[Path => RuleLike[JsObject, JsValue]]
  val ruleJsValueJsValue = implicitly[Path => RuleLike[JsValue, JsValue]]
  val writeJsValueJsObject = implicitly[Path => WriteLike[JsValue, JsObject]]
  val monoidJsObject = implicitly[Monoid[JsObject]]
}
import Static._

object DeriveJson {
  implicit def readCompilerCantFindJsValueAsJ[K <: Symbol, V, T <: HList]
    (implicit
      key: Witness.Aux[K],
      sv: Lazy[RuleLike[JsValue, V]],
      st: Lazy[RuleLike[JsObject, T]]
    ): RuleLike[JsObject, FieldType[K, V] :: T] =
      DeriveRule.ruleProductInductionStep[JsValue, JsObject, K, V, T](ruleJsObjectJsValue)(key, sv, st)

  implicit def readCompilerCantFindJsObject[K <: Symbol, V, T <: HList]
    (implicit
      key: Witness.Aux[K],
      sv: Lazy[RuleLike[JsValue, V]],
      st: Lazy[RuleLike[JsValue, T]]
    ): RuleLike[JsValue, FieldType[K, V] :: T] =
      DeriveRule.ruleProductInductionStep[JsValue, JsValue, K, V, T](ruleJsValueJsValue)(key, sv, st)
  
  implicit def writeHConsCompilerCantFindJsValueAsJ[K <: Symbol, V, T <: HList]
    (implicit
      key: Witness.Aux[K],
      sv: Lazy[WriteLike[V, JsValue]],
      st: Lazy[WriteLike[T, JsObject]]
    ): WriteLike[FieldType[K, V] :: T, JsObject] =
      DeriveWrite.writeProductInductionStep[JsValue, JsObject, K, V, T](writeJsValueJsObject, monoidJsObject)(key, sv, st)
      
  implicit val writeHNilCompilerCantFindJsValueAsJ: WriteLike[HNil, JsObject] =
    DeriveWrite.writeProductBaseCase[JsObject](monoidJsObject)
  
  implicit def writeCoproductInductionStepXXX[K <: Symbol, V, T <: Coproduct]
    (implicit
      key: Witness.Aux[K],
      sv: Lazy[WriteLike[V, JsObject]],
      st: Lazy[WriteLike[T, JsObject]],
      wl: Path => WriteLike[String, JsObject]
    ): WriteLike[FieldType[K, V] :+: T, JsObject] =
      DeriveWrite.writeCoproductInductionStep(monoidJsObject)(key, sv, st, wl)
}

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
    import DeriveRule._
    import DeriveWrite._
    import DeriveJson._
    (
      implicitly[RuleLike[JsValue, Dog]],
      implicitly[RuleLike[JsObject, Dog]],
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
    import DeriveRule._
    import DeriveWrite._
    import DeriveJson._
    
    val r = implicitly[RuleLike[JsValue, Animal]]
    val w = implicitly[WriteLike[Animal, JsValue]]
    List[Animal](dog, cat) foreach { animal =>
      test(animal, r.validate(w.writes(animal)).toOption.get)
    }
  }
  
  // {
  //   import DeriveRule._
  //   import DeriveJson._
  //   val r = implicitly[RuleLike[JsValue, Dogception]]
  //   println(r.validate(dogceptionJson))
  // }
}
