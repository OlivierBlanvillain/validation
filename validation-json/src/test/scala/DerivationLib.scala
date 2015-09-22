import shapeless.{Path => _, _}
import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds
import scala.util.Random

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
