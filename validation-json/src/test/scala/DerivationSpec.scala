import jto.validation._
import jto.validation.json._
import jto.validation.json.Rules._
import jto.validation.json.Writes._
import play.api.libs.json.{JsValue, JsObject, Json}
import DerivationTestLib._
import shapeless.test.illTyped

object Test extends App {
  
  sealed trait Animal
  case class Cat(name: String, fish: Int, friend: Dog) extends Animal
  case class Dog(name: String, bones: Int) extends Animal
  case class WithOptions(i: Int, os: Option[String])
  case class WithList(i: Int, ls: List[String])
  
  val dogJson = Json.parse("""
    {
      "name": "doge",
      "bones": 0
    }
  """).as[JsObject]
  
  val dog = Dog("doge", 0)
  val cat = Cat("miaou", 1, dog)
    
  "Derivated instance for Dog behaves the same as macro generated ones"; {
    import Derivation._
    
    test(
      Rule.gen[JsObject, Dog],
      Rule.derive: RuleLike[JsObject, Dog]
    )(dogJson)

    test(
      Rule.gen[JsValue, Dog],
      Rule.derive: RuleLike[JsValue, Dog]
    )(dogJson)
    
    test(
      Write.gen[Dog, JsObject],
      Write.derive: WriteLike[Dog, JsObject]
    )(dog)
  }
  
  "Non recursive derivation fails when generation with macro fails"; {
    import Derivation._
    
    illTyped("Rule.gen[JsValue, Cat]")
    illTyped("Rule.derive: RuleLike[JsValue, Cat]")
    
    illTyped("Write.gen[Cat, JsObject]")
    illTyped("Write.derive: WriteLike[Cat, JsObject]")

    illTyped("Rule.gen[JsValue, Animal]")
    illTyped("Rule.derive: RuleLike[JsValue, Animal]")
    
    illTyped("Write.gen[Animal, JsObject]")
    illTyped("Write.derive: WriteLike[Animal, JsObject]")
  }
  
  "Recursive product derivation handles nested case classes"; {
    import DerivationReq._
    
    test(
      implicitly[RuleLike[JsValue, Cat]],
      implicitly[WriteLike[Cat, JsObject]]
    )(cat)
  }

  "Coproduct derivation handles ADTs"; {
    import DerivationReq._
    
    illTyped("Rule.gen[JsValue, Animal]")
    illTyped("Write.gen[Animal, JsObject]")
    
    List(dog, cat) foreach test(
      implicitly[RuleLike[JsValue, Animal]],
      implicitly[WriteLike[Animal, JsObject]])
  }

  "Derivation instance with Option behaves the same as macro generated ones"; {
    import DerivationReq._
    
    val wo = WithOptions(1, Some("ksjdf"))
    val woJson = Json.parse("""{"i": 1, "os": "ksjdf"}""")
    val woNone = WithOptions(1, None)
    val woNoneJson = Json.parse("""{"i": 1}""")
    
    List(wo, woNone) foreach test(
      Write.gen[WithOptions, JsObject],
      implicitly[WriteLike[WithOptions, JsObject]])
    
    List(woJson, woNoneJson) foreach test(
      Rule.gen[JsValue, WithOptions],
      implicitly[RuleLike[JsValue, WithOptions]])
    
    implicitly[WriteLike[WithList, JsObject]]
    implicitly[RuleLike[JsValue, WithList]]
  }
  
  {
    import DerivationReq._
    testAgainstMacro(Format.gen[JsValue, JsObject, Dog])
    testAgainstItself[JsValue, JsObject, Cat]
  }
}
