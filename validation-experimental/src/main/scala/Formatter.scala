package jto.validation

import shapeless._

object show {
  // Show using low-level infrastructure ...

  import labelled._

  /**

trait RuleLike[I, O] {
  def validate(data: I): Validated[Seq[(Path, Seq[VE])], O]
}

   */
  type VE = ValidationError
  type Rulz[T] = RuleLike[String, T]

  implicit val stringR: Rulz[String] = Rule.fromMapping { s => Valid(s) }
  implicit val intR: Rulz[Int] = Rule.fromMapping { s =>
    try {
      Valid(s.toInt)
    } catch {
      case _: Exception => Valid(-1)
    }
  }

  implicit def ruleGeneric[F, G]
    (implicit
      gen: LabelledGeneric.Aux[F, G],
      sg: Lazy[Rulz[G]]
    ): Rulz[F] =
      new RuleLike[String, F] {
        def validate(s: String): Validated[Seq[(Path, Seq[VE])], F] =
          sg.value.validate(s).map(gen.from)
      }

  implicit def ruleHNil: Rulz[HNil] =
    new Rulz[HNil] {
      def validate(s: String): Validated[Seq[(Path, Seq[VE])], HNil] = Valid(HNil)
    }

  implicit def ruleHCons[K <: Symbol, V, T <: HList]
    (implicit
      key: Witness.Aux[K],
      sv: Lazy[Rulz[V]],
      st: Lazy[Rulz[T]]
    ): Rulz[FieldType[K, V] :: T] =
      new Rule[String, FieldType[K, V] :: T] {
        def validate(s: String): Validated[Seq[(Path, Seq[VE])], FieldType[K, V] :: T] = {
          val pathed = (Path \ key.value.name).read[String, V](_ => sv.value)
          println(key.value.name)
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
  import show._
  println(
    implicitly[Rulz[Cat]].validate("")
  )
}
