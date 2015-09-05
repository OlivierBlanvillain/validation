package play.api.data.mapping

import scala.language.implicitConversions
import cats._
import cats.functor.Contravariant

trait WriteLike[I, +O] {
  /**
   * "Serialize" `i` to the output type
   */
  def writes(i: I): O
}

object WriteLike {
  implicit def zero[I]: WriteLike[I, I] = Write(identity[I] _)
}

trait Write[I, +O] extends WriteLike[I, O] {

  /**
   * returns a new Write that applies function `f` to the result of this write.
   * {{{
   *  val w = Writes.int.map("Number: " + _)
   *  w.writes(42) == "Number: 42"
   * }}}
   */
  def map[B](f: O => B) = Write[I, B] {
    f.compose(x => this.writes(x))
  }

  /**
   * Returns a new Write that applies `this` Write, and then applies `w` to its result
   */
  def compose[OO >: O, P](w: WriteLike[OO, P]) =
    this.map(o => w.writes(o))
}

object Write {
  import scala.language.experimental.macros

  def apply[I, O](w: I => O): Write[I, O] = new Write[I, O] {
    def writes(i: I) = w(i)
  }

  def toWrite[I, O](r: WriteLike[I, O]) = new Write[I, O] {
    def writes(data: I): O = r.writes(data)
  }

  // def gen[I, O]: Write[I, O] = macro MappingMacros.write[I, O]

  implicit def zero[I]: Write[I, I] = toWrite(WriteLike.zero[I])

  implicit def applicativeWrite[O](implicit m: Monoid[O]) = new Applicative[Write[?, O]] {
    def pure[A](x: A): Write[A, O] =
      Write(_ => m.empty)
      
    def ap[A, B](fa: Write[A, O])(f: Write[A => B, O]): Write[B, O] =
      Write { (b: B) =>
        m.empty
      }
  }
  
  // def bind[A, B](fa: T => A)(f: A => T => B): T => B = (t) => f(fa(t))(t)
  
  // implicit def applicativeWritz[I] = new Applicative[Write[I, ?]] {
  //   def pure[A](x: A): Write[I, A] = ???
  //   def ap[A, B](fa: Write[I, A])(f: Write[I, A => B]): Write[I, B] = ???
  // }

  // implicit def writeMonoid[I, O](implicit m: Monoid[O]): Monoid[Write[I, O]] =
  //   new Monoid[Write[I, O]] {
  //     def empty: Write[I, O] =
  //       Write(_ => m.empty)
      
  //     def combine(x: Write[I, O], y: Write[I, O]): Write[I, O] =
  //       Write(i => m.combine(x.writes(i), y.writes(i)))
  //   }

  // implicit def functionalCanBuildWrite[O](implicit m: Monoid[O]) = new FunctionalCanBuild[({ type λ[I] = Write[I, O] })#λ] {
  //   def apply[A, B](wa: Write[A, O], wb: Write[B, O]): Write[A ~ B, O] = Write[A ~ B, O] { (x: A ~ B) =>
  //     x match {
  //       case a ~ b => m.append(wa.writes(a), wb.writes(b))
  //     }
  //   }
  // }

  implicit def contravariantFunctorWrite[O] = new Contravariant[Write[?, O]] {
    def contramap[A, B](wa: Write[A, O])(f: B => A): Write[B, O] =
      Write[B, O]((b: B) => wa.writes(f(b)))
  }

  // implicit def contravariantFunctorExtractorWrite[I, O]: VariantExtractor[({ type λ[I] = Write[I, O] })#λ] =
  //   VariantExtractor.contravariantFunctor[({ type λ[I] = Write[I, O] })#λ](contravariantFunctorWrite)

  // XXX: Helps the compiler a bit
  // implicit def fboWrite[I, O: Monoid](a: Write[I, O]) = toFunctionalBuilderOps[({ type λ[I] = Write[I, O] })#λ, I](a)
  // implicit def cfoWrite[I, O](a: Write[I, O]) = toContraFunctorOps[({ type λ[I] = Write[I, O] })#λ, I](a)
}
