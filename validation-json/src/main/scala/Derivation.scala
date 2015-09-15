package jto.validation
package json

import Rules._
import shapeless.{Path => _, _}
import play.api.libs.json.{JsValue, JsObject, Json, JsString, JsNumber, JsBoolean, JsArray, JsNull}

object Derivation {
  import labelled._

  type Rulz[T] = RuleLike[JsValue, T]

  implicit def ruleGeneric[F, G]
    (implicit
      gen: LabelledGeneric.Aux[F, G],
      sg: Lazy[Rulz[G]]
    ): Rulz[F] =
      new RuleLike[JsValue, F] {
        def validate(s: JsValue): VA[F] =
          sg.value.validate(s).map(gen.from)
      }

  implicit def ruleHNil: Rulz[HNil] =
    new Rulz[HNil] {
      def validate(s: JsValue): VA[HNil] = Valid(HNil)
    }

  implicit def ruleHCons[K <: Symbol, V, T <: HList]
    (implicit
      key: Witness.Aux[K],
      sv: Lazy[Rulz[V]],
      st: Lazy[Rulz[T]]
    ): Rulz[FieldType[K, V] :: T] =
      new RuleLike[JsValue, FieldType[K, V] :: T] {
        def validate(s: JsValue): VA[FieldType[K, V] :: T] = {
          val pathed =
            From[JsValue] { __ =>
              (__ \ key.value.name).read(sv.value)
            }
          val svV = pathed.validate(s)
          val stV = st.value.validate(s)
          val stKV = stV.map((t: T) => (v: V) => field[K](v) :: t)
          svV.ap(stKV)
        }
      }
      
  sealed trait Animal
  case class Cat(name: String, fish: Int) extends Animal
  case class Dog(name: String, bones: Int) extends Animal
}

object main extends App {
  import Derivation._
  val json = Json.parse("""
    {
      "name": "miam",
      "fish": 3
    }
  """)
  
  println(
    Rule.gen[JsValue, Cat].validate(json)
  )
  
  println(
    implicitly[Rulz[Cat]].validate(json)
  )
}
