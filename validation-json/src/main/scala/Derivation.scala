package jto.validation
package json

import Rules._
import Writes._
import shapeless.{Path => _, _}
import play.api.libs.json.{JsValue, JsObject, Json}
import labelled._
import cats.Monoid

/*

trait WriteLike[I, +O] {
  def writes(i: I): O
}

trait RuleLike[I, O] {
  def validate(data: I): VA[O]
}

*/

object DeriveRule {
  implicit def ruleGeneric[I, F, G]
    (implicit
      gen: LabelledGeneric.Aux[F, G],
      sg: Lazy[RuleLike[I, G]]
    ): RuleLike[I, F] =
      new RuleLike[I, F] {
        def validate(i: I): VA[F] =
          sg.value.validate(i).map(gen.from)
      }

  implicit def ruleHNil[I]: RuleLike[I, HNil] =
    new RuleLike[I, HNil] {
      def validate(i: I): VA[HNil] = Valid(HNil)
    }

  implicit def ruleHCons[J, I <: J, K <: Symbol, V, T <: HList]
    (implicit
      key: Witness.Aux[K],
      sv: Lazy[RuleLike[J, V]],
      st: Lazy[RuleLike[I, T]],
      rl: Path => RuleLike[I, J]
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
      
  implicit def readCompilerCantFindJsValueAsJ[K <: Symbol, V, T <: HList]
    (implicit
      key: Witness.Aux[K],
      sv: Lazy[RuleLike[JsValue, V]],
      st: Lazy[RuleLike[JsObject, T]],
      pr: Path => RuleLike[JsObject, JsValue]
    ): RuleLike[JsObject, FieldType[K, V] :: T] =
      ruleHCons[JsValue, JsObject, K, V, T](key, sv, st, pr)
}

object DeriveWrite {
  implicit def writeGeneric[O, F, G]
    (implicit
      gen: LabelledGeneric.Aux[F, G],
      sg: Lazy[WriteLike[G, O]]
    ): WriteLike[F, O] =
      new WriteLike[F, O] {
        def writes(i: F): O = sg.value.writes(gen.to(i))
      }

  implicit def writeHNil[O](implicit m: Monoid[O]): WriteLike[HNil, O] =
    new WriteLike[HNil, O] {
      def writes(i: HNil): O = m.empty
    }
  
  // O=JsObject, J=JsValue
  implicit def writeHCons[J, O <: J, K <: Symbol, V, T <: HList]
    (implicit
      key: Witness.Aux[K],
      sv: Lazy[WriteLike[V, J]],
      st: Lazy[WriteLike[T, O]],
      pw: Path => WriteLike[J, O],
      m: Monoid[O]
    ): WriteLike[FieldType[K, V] :: T, O] =
      new WriteLike[FieldType[K, V] :: T, O] {
        def writes(i: FieldType[K, V] :: T): O = {
          val pathed = To[O] { __ =>
            (__ \ key.value.name).write[V, J](sv.value)(pw)
          }
          i match {
            case v :: t => m.combine(st.value.writes(t), pathed.writes(v))
          }
        }
      }
  
  implicit def writeCompilerCantFindJsValueAsJ[K <: Symbol, V, T <: HList]
    (implicit
      key: Witness.Aux[K],
      sv: Lazy[WriteLike[V, JsValue]],
      st: Lazy[WriteLike[T, JsObject]],
      pw: Path => WriteLike[JsValue, JsObject],
      m: Monoid[JsObject]
    ): WriteLike[FieldType[K, V] :: T, JsObject] =
      writeHCons[JsValue, JsObject, K, V, T](key, sv, st, pw, m)
}

sealed trait Animal
case class Cat(name: String, fish: Int, friend: Dog) extends Animal
case class Dog(name: String, bones: Int) extends Animal

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
  
  
  {
    Rule.gen[JsValue, Dog].validate(dogJson)
    Rule.gen[JsObject, Dog].validate(dogJson)
    Write.gen[Dog, JsObject]
  }
  
  {
    import DeriveRule._
    import DeriveWrite._
    implicitly[RuleLike[JsValue, Dog]].validate(dogJson)
    implicitly[RuleLike[JsValue, Cat]].validate(dogJson)
    // implicitly[RuleLike[JsObject, Cat]].validate(dogJson)
    
    val dog = implicitly[RuleLike[JsObject, Dog]].validate(dogJson).toOption.get
    
    implicitly[Path => WriteLike[JsValue, JsObject]]
    
    implicit val l = writeHNil[JsObject]
    println(implicitly[WriteLike[Dog, JsObject]].writes(dog))
  }
}
