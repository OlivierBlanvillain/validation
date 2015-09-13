package jto.validation

import shapeless._

object show {
  // Show using low-level infrastructure ...

  import labelled._

  /**

trait RuleLike[I, O] {
  def validate(data: I): Validated[Seq[(Path, Seq[ValidationError])], O]
}

   */

  type Rulz[T] = RuleLike[String, T]

    object RuleLike {

    implicit val stringR: Rulz[String] = ???
    implicit val doubleR: Rulz[Double] = ???
    implicit val intR: Rulz[Int] = ???

    implicit def ruleGeneric[F, G]
      (implicit
        gen: LabelledGeneric.Aux[F, G],
        sg: Lazy[Rulz[G]]
      ): Rulz[F] =
        new RuleLike[String, F] {
          def validate(s: String): VA[F] = sg.value.validate(s).map(gen.from)
        }

    implicit def ruleHNil: Rulz[HNil] =
      new Rulz[HNil] {
        def validate(s: String): Validated[Seq[(Path, Seq[ValidationError])], HNil] = ???
      }

    implicit def ruleHCons[K <: Symbol, V, T <: HList]
      (implicit
        key: Witness.Aux[K],
        sv: Lazy[Rulz[V]],
        st: Lazy[Rulz[T]]
      ): Rulz[FieldType[K, V] :: T] =
        new Rule[String, FieldType[K, V] :: T] {
          def validate(s: String): VA[FieldType[K, V] :: T] = {
            ???
          }
        }

  }

  sealed trait Animal
  case class Cat(name: String, fish: Int) extends Animal
  case class Dog(name: String, bones: Int) extends Animal
}

object main extends App {
  import show._
  implicitly[Rulz[Cat]]
}
