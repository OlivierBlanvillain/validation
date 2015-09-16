package jto.validation
package json

import Rules._
import Writes._
import shapeless.{Path => _, _}
import play.api.libs.json.{JsValue, JsObject, Json}
import labelled._

object DeriveWrite {
  type Writz[T] = WriteLike[T, JsValue]
  
  implicit def writeGeneric[F, G]
    (implicit
      gen: LabelledGeneric.Aux[F, G],
      sg: Lazy[Writz[G]]
      // m: cats.Monoid[JsObject]
    ): Writz[F] =
      new WriteLike[F, JsValue] {
        def writes(input: F): JsValue = ???
      }

  implicit def writeHNil: Writz[HNil] =
    new WriteLike[HNil, JsValue] {
      def writes(input: HNil): JsValue = ???
    }

  implicit def writeHCons[K <: Symbol, V, T <: HList]
    (implicit
      key: Witness.Aux[K],
      sv: Lazy[Writz[V]],
      st: Lazy[Writz[T]]
    ): Writz[FieldType[K, V] :: T] =
      new WriteLike[FieldType[K, V] :: T, JsValue] {
        def writes(input: FieldType[K, V] :: T): JsValue = ???
      }
}

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
        def validate(s: I): VA[F] =
          sg.value.validate(s).map(gen.from)
      }

  implicit def ruleHNil[I]: RuleLike[I, HNil] =
    new RuleLike[I, HNil] {
      def validate(s: I): VA[HNil] = Valid(HNil)
    }

  // I=JsValue, J=JsObject
  // I <: J
  implicit def ruleHCons[J, I <: J, K <: Symbol, V, T <: HList]
    (implicit
      key: Witness.Aux[K],
      sv: Lazy[RuleLike[J, V]],
      st: Lazy[RuleLike[I, T]],
      rl: Path => RuleLike[I, J]
    ): RuleLike[I, FieldType[K, V] :: T] =
      new RuleLike[I, FieldType[K, V] :: T] {
        def validate(input: I): VA[FieldType[K, V] :: T] = {
          
// def read[I, O](implicit r: Path => RuleLike[I, O]): Rule[I, O] =
// def read[I, J, O](sub: => RuleLike[J, O])(implicit r: Path => RuleLike[I, J]): Rule[I, O] =
          
// def read[O](implicit r: Path => RuleLike[I,O]): Rule[I,O]
// def read[J, O](sub: => RuleLike[J,O])(implicit r: Path => RuleLike[I,J]): Rule[I,O]

          val pathed = From[I] { __ =>
            (__ \ key.value.name).read[J, V](sv.value)(rl)
          }
          val head = pathed.validate(input)
          val tail = st.value.validate(input).map((t: T) => (v: V) => field[K](v) :: t)
          head ap tail
        }
      }
}

sealed trait Animal
case class Cat(name: String, fish: Int, friend: Dog) extends Animal
case class Dog(name: String, bones: Int) extends Animal

object main extends App {
  val cat = Json.parse("""
    {
      "name": "miam",
      "fish": 3,
      "friend": {
        "name": "doge",
        "bones": 0
      }
    }
  """)
  
  val dog = Json.parse("""
    {
      "name": "doge",
      "bones": 0
    }
  """).as[JsObject]
  
  {
    Rule.gen[JsValue, Dog].validate(dog)
    Rule.gen[JsObject, Dog].validate(dog)
    Write.gen[Dog, JsObject]
  }
  
  {
    import DeriveRule._
    // import DeriveWrite._
    implicitly[Path => RuleLike[JsObject, JsValue]]
    implicitly[Path => RuleLike[JsValue, JsObject]]
    
    implicitly[RuleLike[JsValue, Dog]].validate(dog)
    // implicitly[RuleLike[JsObject, Dog]].validate(dog)
    // implicitly[WriteLike[Dog, JsValue]]
  }
}
