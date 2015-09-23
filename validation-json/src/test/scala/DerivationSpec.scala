import jto.validation._
import jto.validation.json._
import play.api.libs.json.{JsValue, JsObject, Json}
import DerivationTestLib._
import org.specs2.mutable._
import org.specs2.execute.Result

object DerivationSpec extends Specification {
  
  implicit def iDontLikeSpec2[T](t: T): Result = Result.unit(())
  
  sealed trait Animal
  case class Dog(name: String, bones: Int) extends Animal
  case class Cat(name: String, fish: Int, friend: Option[Cat]) extends Animal
  
  case class WithOptions(os: Option[String])
  case class WithList(ls: List[String])
  
  "Derivation syntax should be similar to the macro syntax" in {
    import Rules._, Writes._
    import Derivation._
    
    val derivedRuleJsObject: RuleLike[JsObject, Dog] = Rule.derive
    val macroRuleJsObejct: RuleLike[JsObject, Dog] = Rule.gen[JsObject, Dog]

    val derivedRuleJsValue: RuleLike[JsValue, Dog] = Rule.derive
    val macroRuleJsValue: RuleLike[JsValue, Dog] = Rule.gen[JsValue, Dog]

    val derivedWriteJsObject: WriteLike[Dog, JsObject] = Write.derive
    val macroWriteJsObject: WriteLike[Dog, JsObject] = Write.gen[Dog, JsObject]
    
    val dogJson = Json.parse("""{"name": "doge", "bones": 0}""").as[JsObject]
    val dog = Dog("doge", 0)
    
    derivedRuleJsObject.validate(dogJson) mustEqual macroRuleJsObejct.validate(dogJson)
    derivedRuleJsValue.validate(dogJson) mustEqual macroRuleJsValue.validate(dogJson)
    derivedWriteJsObject.writes(dog) mustEqual macroWriteJsObject.writes(dog)
  }
  
  "Non recursive derivation fails when macro generation fails" in {
    import Rules._, Writes._
    import Derivation._
    import shapeless.test.illTyped
    
    illTyped("Rule.gen[JsValue, Cat]")
    illTyped("Rule.derive: RuleLike[JsValue, Cat]")
    
    illTyped("Write.gen[Cat, JsObject]")
    illTyped("Write.derive: WriteLike[Cat, JsObject]")

    illTyped("Rule.gen[JsValue, Animal]")
    illTyped("Rule.derive: RuleLike[JsValue, Animal]")
    
    illTyped("Write.gen[Animal, JsObject]")
    illTyped("Write.derive: WriteLike[Animal, JsObject]")
  }
  
  "Coproduct derivation handles ADTs" in {
    import Rules._, Writes._
    import DerivationReq._
    
    sealed trait ADT
    case object A extends ADT
    case object D extends ADT
    case object T extends ADT
    
    val a = A
    val aJson = Json.parse("""{"$type":"A"}""")
    
    implicitly[WriteLike[ADT, JsObject]].writes(a) mustEqual aJson
    implicitly[RuleLike[JsValue, ADT]].validate(aJson) mustEqual Valid(a)
  }
  
  "Recursive derivation handles recursive case classes" in {
    import Rules._, Writes._
    import DerivationReq._
    
    val cat = Cat("le chat", 1, Some(Cat("garfield", 0, None)))
    val catJson = Json.parse("""{"name":"le chat","fish":1,"friend":{"name":"garfield","fish":0}}""")

    implicitly[WriteLike[Cat, JsObject]].writes(cat) mustEqual catJson
    implicitly[RuleLike[JsValue, Cat]].validate(catJson) mustEqual Valid(cat)
  }

  "Derivation instance with Option behaves the same as macro generated ones" in {
    import Rules._, Writes._
    import DerivationReq._
    
    val woSome = WithOptions(Some("ksjdf"))
    val woSomeJson = Json.parse("""{"os": "ksjdf"}""")
    
    val woNone = WithOptions(None)
    val woNoneJson = Json.parse("{}")
    
    Write.gen[WithOptions, JsObject].writes(woSome) mustEqual implicitly[WriteLike[WithOptions, JsObject]].writes(woSome)
    Write.gen[WithOptions, JsObject].writes(woNone) mustEqual implicitly[WriteLike[WithOptions, JsObject]].writes(woNone)

    Rule.gen[JsValue, WithOptions].validate(woSomeJson) mustEqual implicitly[RuleLike[JsValue, WithOptions]].validate(woSomeJson)
    Rule.gen[JsValue, WithOptions].validate(woNoneJson) mustEqual implicitly[RuleLike[JsValue, WithOptions]].validate(woNoneJson)
  }
  
  "Moar tests!!!" in { 
    import Rules._, Writes._
    import DerivationReq._
    testAgainstMacro(Format.gen[JsValue, JsObject, Dog])
    testAgainstItself[JsValue, JsObject, Cat]
  }
}
