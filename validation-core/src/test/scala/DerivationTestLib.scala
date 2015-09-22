import jto.validation._
import shapeless.{Path => _, test => _,  _}
import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds
import scala.util.Random

object DerivationTestLib {
/** Type class for generating Arbitrary values of a type T */
  trait Arbitrary[T] {
    def value: T
    def map[A](f: T => A): Arbitrary[A] = Arbitrary[A](f(value))
  }

  /** Automatic type class derivation for Arbitrary */
  object Arbitrary {
    def apply[T](implicit a: Arbitrary[T]): Arbitrary[T] = a
    def apply[T](a: => T): Arbitrary[T] = new Arbitrary[T] { def value = a }
    def value[T](implicit a: Arbitrary[T]): T = a.value
    
    implicit val arbitraryInt = Arbitrary(1 + Random.nextInt(100))
    implicit val arbitraryDouble = Arbitrary(Math.round((1 + Random.nextDouble) * 100.0) / 100.0)
    
    implicit val arbitraryBoolean = Arbitrary(Random.nextBoolean)
    implicit val arbitraryString = Arbitrary(Random.alphanumeric take 10 mkString "" toLowerCase)
    implicit def arbitraryDateTime = Arbitrary(org.joda.time.DateTime.now)
    
    // Arbitrarly generate scala collections
    implicit def arbitraryColl[Coll[_], T]
      (implicit
        cbf: CanBuildFrom[Nothing, T, Coll[T]],
        a: Arbitrary[T]
      ): Arbitrary[Coll[T]] =
        Arbitrary {
          val builder = cbf()
          while(Random.nextBoolean) builder += a.value
          builder.result()
        }

    implicit def arbitraryMap[A, B](implicit a: Arbitrary[A], b: Arbitrary[B]): Arbitrary[Map[A, B]] =
      Arbitrary(Arbitrary.value[List[(A, B)]].toMap)
    
    // Base case for coproducts
    implicit def arbitraryCLast[H](implicit h: Arbitrary[H]): Arbitrary[H :+: CNil] =
      Arbitrary(Inl(h.value))

    // Induction step for products
    implicit def arbitraryCCons[H, T <: Coproduct]
      (implicit
        h: Arbitrary[H],
        t: Arbitrary[T]
      ): Arbitrary[H :+: T] =
        Arbitrary((if(Random.nextBoolean) Inl(h.value) else Inr(t.value)))

    // Base case for products
    implicit val arbitraryHNil: Arbitrary[HNil] =
      Arbitrary(HNil)

    // Induction step for products
    implicit def arbitraryHCons[H, T <: HList]
      (implicit
        h: Arbitrary[H],
        t: Arbitrary[T]
      ): Arbitrary[H :: T] =
        Arbitrary(h.value :: t.value)

    implicit def arbitraryGeneric[T, R]
      (implicit
        gen: Generic.Aux[T, R],
        arb: Lazy[Arbitrary[R]]
      ): Arbitrary[T] =
        Arbitrary(gen.from(arb.value.value))
  }
  
  def testAgainstMacro[A, AA <: A, T](macroFormat: Format[A, AA, T])
    (implicit
      derivedRule: RuleLike[A, T],
      derivedWrite: WriteLike[T, AA],
      arbitrary: Arbitrary[T]
    ): Unit = {
      val t = arbitrary.value
      val a = macroFormat.writes(t)
      test(a, derivedWrite.writes(t))
      test(macroFormat.validate(a), derivedRule.validate(a))
    }

  def testAgainstItself[A, AA <: A, T]
    (implicit
      derivedRule: RuleLike[A, T],
      derivedWrite: WriteLike[T, AA],
      arbitrary: Arbitrary[T]
    ): Unit = {
      val t = arbitrary.value
      test(t, derivedRule.validate(derivedWrite.writes(t)).toOption.get)
    }
    
  def test[A](arg1: A, arg2: A): Unit =
    if(arg1 == arg2)
      println(arg1)
    else
      throw new Exception(s"""
      |  arg1: $arg1
      |  arg2: $arg2""".stripMargin)
  
  def test[A, AA <: A, T](r: RuleLike[A, T], w: WriteLike[T, AA])(v: T): Unit =
    test(Some(v), r.validate(w.writes(v)).toOption)
  
  def test[I, O](r1: RuleLike[I, O], r2: RuleLike[I, O])(i: I): Unit =
    test(r1.validate(i), r2.validate(i))

  def test[I, O](w1: WriteLike[I, O], w2: WriteLike[I, O])(i: I): Unit =
    test(w1.writes(i), w2.writes(i))
}
