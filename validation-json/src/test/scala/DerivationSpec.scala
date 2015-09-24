import jto.validation._
import jto.validation.json._
import play.api.libs.json.{JsValue, JsObject, Json}

import org.specs2.mutable._
import org.specs2.execute.Result
import org.specs2.matcher.MatchResult
import shapeless.test.illTyped

object DerivationSpec extends Specification {
  
  implicit def iDontLikeSpec2[T](t: T): Result = Result.unit(())
  
  sealed trait Animal
  case class Dog(name: String, bones: Int) extends Animal
  case class Cat(name: String, fish: Int, friend: Option[Cat]) extends Animal
  
  "Derivation syntax should be similar macro syntax" in {
    import Rules._, Writes._
    
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
  
  "Recursive derivation handles recursive case classes" in {
    import Rules._, Writes._
    
    illTyped("Rule.gen[JsValue, Cat]")
    illTyped("Write.gen[Cat, JsObject]")
    
    import DerivationRec._

    val cat = Cat("le chat", 1, Some(Cat("garfield", 0, None)))
    val catJson = Json.parse("""{"name":"le chat","fish":1,"friend":{"name":"garfield","fish":0}}""")

    implicitly[WriteLike[Cat, JsObject]].writes(cat) mustEqual catJson
    implicitly[RuleLike[JsValue, Cat]].validate(catJson) mustEqual Valid(cat)
  }

  "Non-recursive derivation fails when macro generation fails" in {
    import Rules._, Writes._
    
    illTyped("Rule.gen[JsValue, Cat]")
    illTyped("Rule.derive: RuleLike[JsValue, Cat]")
    
    illTyped("Write.gen[Cat, JsObject]")
    illTyped("Write.derive: WriteLike[Cat, JsObject]")

    illTyped("Rule.gen[JsValue, Animal]")
    illTyped("Rule.derive: RuleLike[JsValue, Animal]")
    
    illTyped("Write.gen[Animal, JsObject]")
    illTyped("Write.derive: WriteLike[Animal, JsObject]")
  }
  
  sealed trait ADT
  case object ADT1 extends ADT
  case object ADT2 extends ADT
  case object ADT3 extends ADT

  "Derivation handles ADTs" in {
    import Rules._, Writes._
    import DerivationRec._
    
    illTyped("Write.gen[ADT, JsObject]")
    illTyped("Rule.gen[JsValue, ADT]")
    
    val a = ADT1
    val aJson = Json.parse("""{"$type":"ADT1"}""")
    
    implicitly[WriteLike[ADT, JsObject]].writes(a) mustEqual aJson
    implicitly[RuleLike[JsValue, ADT]].validate(aJson) mustEqual Valid(a)
  }
  
  case class WithOptions(os: Option[String])
  
  "Derivated instance with Option behaves like macro generated instance" in {
    import Rules._, Writes._
    import DerivationRec._
    
    val woSome = WithOptions(Some("ksjdf"))
    val woSomeJson = Json.parse("""{"os": "ksjdf"}""")
    
    val woNone = WithOptions(None)
    val woNoneJson = Json.parse("{}")
    
    Write.gen[WithOptions, JsObject].writes(woSome) mustEqual implicitly[WriteLike[WithOptions, JsObject]].writes(woSome)
    Write.gen[WithOptions, JsObject].writes(woNone) mustEqual implicitly[WriteLike[WithOptions, JsObject]].writes(woNone)

    Rule.gen[JsValue, WithOptions].validate(woSomeJson) mustEqual implicitly[RuleLike[JsValue, WithOptions]].validate(woSomeJson)
    Rule.gen[JsValue, WithOptions].validate(woNoneJson) mustEqual implicitly[RuleLike[JsValue, WithOptions]].validate(woNoneJson)
  }
  
  case class Cat2(name: String)
  case class Contact2(firstname: String, lastname: String, company: Option[String], informations: Seq[ContactInformation])
  case class ContactInformation(label: String, email: Option[String], phones: Seq[String])
  case class Dog2(name: String, master: User)
  case class Foo(name: String)
  case class Id(value: String) extends AnyVal
  case class Person(name: String, age: Int)
  case class Person2(names: List[String])
  case class RecUser(name: String, cat: Option[Cat] = None, hobbies: Seq[String] = Seq(), friends: Seq[RecUser] = Seq())
  case class RecUser2(name: String, friends: List[RecUser] = Nil)
  case class RecUser3(name: String, friends: Seq[RecUser] = Nil)
  case class Toto(name: String)
  case class Toto2(name: Option[String])
  case class Toto3(name: List[Double])
  case class Toto4(name: Set[Long])
  case class Toto5(name: Map[String, Int])
  case class Toto6(name: Seq[Dog])
  case class User(age: Int, name: String)
  case class User1(name: String, friend: Option[User1] = None)
  case class User2(id: Long, name: String)
  case class UserFail(name: String, bd: Toto)
  case class UserMap(name: String, friends: Map[String, UserMap] = Map())
  case class WithList(ls: List[String])

  trait A
  case class B(foo: Int) extends A
  case class C(bar: Int) extends A

  case class X(_1: String, _2: String, _3: String, _4: String, _5: String, _6: String, _7: String, _8: String, _9: String, _10: String, _11: String, _12: String, _13: String, _14: String, _15: String, _16: String, _17: String, _18: String, _19: String, _20: String, _21: String)

  case class Program(id: Long, name: String, logoPath: Option[String], logoThumb: Option[String])
  object Program { def programs = List.empty[Program] }

  case class ManyApplies(foo: String, bar: Int)
  object ManyApplies {
    def apply(x: Option[Int]) = 9
    def apply(y: String) = 4
    def apply(x: String, y: String) = 10
  }

  "S'il vous plait... derive-moi un mouton !" in {
    import Rules._, Writes._
    import DerivationRec._

    def testAgainstMacro[O, OO <: O, T](macroFormat: Format[O, OO, T])
      (implicit
        derivedRule: RuleLike[O, T],
        derivedWrite: WriteLike[T, OO],
        arbitrary: Arbitrary[T]
      ): MatchResult[Any] = {
        val t = arbitrary.value
        val a = macroFormat.writes(t)
        a mustEqual derivedWrite.writes(t)
        macroFormat.validate(a) mustEqual derivedRule.validate(a)
        derivedRule.validate(a) mustEqual Valid(t)
      }

    def testAgainstItself[O, OO <: O, T]
      (implicit
        derivedRule: RuleLike[O, T],
        derivedWrite: WriteLike[T, OO],
        arbitrary: Arbitrary[T]
      ): MatchResult[Any] = {
        val t = arbitrary.value
        t mustEqual derivedRule.validate(derivedWrite.writes(t)).toOption.get
      }

    testAgainstMacro(Format.gen[JsValue, JsObject, B])
    // testAgainstMacro(Format.gen[JsValue, JsObject, C])
    // testAgainstMacro(Format.gen[JsValue, JsObject, Cat2])
    // testAgainstMacro(Format.gen[JsValue, JsObject, ContactInformation])
    // testAgainstMacro(Format.gen[JsValue, JsObject, Foo])
    // testAgainstMacro(Format.gen[JsValue, JsObject, Id])
    // testAgainstMacro(Format.gen[JsValue, JsObject, IdxPathNode])
    // testAgainstMacro(Format.gen[JsValue, JsObject, KeyPathNode])
    // testAgainstMacro(Format.gen[JsValue, JsObject, Person2])
    // testAgainstMacro(Format.gen[JsValue, JsObject, Person])
    // testAgainstMacro(Format.gen[JsValue, JsObject, Program])
    // testAgainstMacro(Format.gen[JsValue, JsObject, Toto2])
    // testAgainstMacro(Format.gen[JsValue, JsObject, Toto3])
    // testAgainstMacro(Format.gen[JsValue, JsObject, Toto4])
    // testAgainstMacro(Format.gen[JsValue, JsObject, Toto5])
    // testAgainstMacro(Format.gen[JsValue, JsObject, Toto])
    // testAgainstMacro(Format.gen[JsValue, JsObject, User2])
    // testAgainstMacro(Format.gen[JsValue, JsObject, User])
    // testAgainstMacro(Format.gen[JsValue, JsObject, WithList])
    // testAgainstMacro(Format.gen[JsValue, JsObject, X])
    
    testAgainstItself[JsValue, JsObject, Contact2]
    // testAgainstItself[JsValue, JsObject, Dog2]
    // testAgainstItself[JsValue, JsObject, ManyApplies]
    // testAgainstItself[JsValue, JsObject, RecUser3]
    // testAgainstItself[JsValue, JsObject, RecUser2]
    // testAgainstItself[JsValue, JsObject, RecUser]
    // testAgainstItself[JsValue, JsObject, Toto6]
    // testAgainstItself[JsValue, JsObject, User1]
    // testAgainstItself[JsValue, JsObject, User2]
    // testAgainstItself[JsValue, JsObject, UserFail]
    // testAgainstItself[JsValue, JsObject, UserMap]
  }
}
