package play.api.data.mapping

import scala.language.higherKinds
import cats._
import cats.functor._

case class ~[A, B](_1: A, _2: B)

trait FunctionalCanBuild[M[_]] {

  def apply[A, B](ma: M[A], mb: M[B]): M[A ~ B]

}

class FunctionalBuilderOps[M[_], A](ma: M[A])(implicit fcb: FunctionalCanBuild[M]) {

  def ~[B](mb: M[B]): FunctionalBuilder[M]#CanBuild2[A, B] = {
    val b = new FunctionalBuilder(fcb)
    new b.CanBuild2[A, B](ma, mb)
  }

  def and[B](mb: M[B]): FunctionalBuilder[M]#CanBuild2[A, B] = this.~(mb)
}

class FunctionalBuilder[M[_]](canBuild: FunctionalCanBuild[M]) {

  class CanBuild2[A1, A2](m1: M[A1], m2: M[A2]) {

    def ~[A3](m3: M[A3]) = new CanBuild3[A1, A2, A3](canBuild(m1, m2), m3)

    def and[A3](m3: M[A3]) = this.~(m3)

    def apply[B](f: (A1, A2) => B)(implicit fu: Functor[M]): M[B] =
      fu.map[A1 ~ A2, B](canBuild(m1, m2))({ case a1 ~ a2 => f(a1, a2) })

    def apply[B](f: B => (A1, A2))(implicit fu: Contravariant[M]): M[B] =
      fu.contramap(canBuild(m1, m2))((b: B) => { val (a1, a2) = f(b); new ~(a1, a2) })

    def apply[B](f1: (A1, A2) => B, f2: B => (A1, A2))(implicit fu: Invariant[M]): M[B] =
      fu.imap[A1 ~ A2, B](
        canBuild(m1, m2))({ case a1 ~ a2 => f1(a1, a2) })(
        (b: B) => { val (a1, a2) = f2(b); new ~(a1, a2) }
      )

    def join[A >: A1](implicit witness1: <:<[A, A1], witness2: <:<[A, A2], fu: Contravariant[M]): M[A] =
      apply[A]((a: A) => (a: A1, a: A2))(fu)
      
    def tupled(implicit fu: Invariant[M]): M[(A1, A2)] = apply[(A1, A2)]({ (a1: A1, a2: A2) => (a1, a2) }, { (a: (A1, A2)) => (a._1, a._2) })(fu)
  }
  
  class CanBuild3[A1, A2, A3](m1: M[A1 ~ A2], m2: M[A3]) {

    def ~[A4](m3: M[A4]) = new CanBuild4[A1, A2, A3, A4](canBuild(m1, m2), m3)

    def and[A4](m3: M[A4]) = this.~(m3)

    def apply[B](f: (A1, A2, A3) => B)(implicit fu: Functor[M]): M[B] =
      fu.map[A1 ~ A2 ~ A3, B](canBuild(m1, m2))({ case a1 ~ a2 ~ a3 => f(a1, a2, a3) })

    def apply[B](f: B => (A1, A2, A3))(implicit fu: Contravariant[M]): M[B] =
      fu.contramap(canBuild(m1, m2))((b: B) => { val (a1, a2, a3) = f(b); new ~(new ~(a1, a2), a3) })

    def apply[B](f1: (A1, A2, A3) => B, f2: B => (A1, A2, A3))(implicit fu: Invariant[M]): M[B] =
      fu.imap[A1 ~ A2 ~ A3, B](
        canBuild(m1, m2))({ case a1 ~ a2 ~ a3 => f1(a1, a2, a3) })(
        (b: B) => { val (a1, a2, a3) = f2(b); new ~(new ~(a1, a2), a3) }
      )

    def join[A >: A1](implicit witness1: <:<[A, A1], witness2: <:<[A, A2], witness3: <:<[A, A3], fu: Contravariant[M]): M[A] =
      apply[A]((a: A) => (a: A1, a: A2, a: A3))(fu)
  }

  class CanBuild4[A1, A2, A3, A4](m1: M[A1 ~ A2 ~ A3], m2: M[A4]) {

    def ~[A5](m3: M[A5]) = new CanBuild5[A1, A2, A3, A4, A5](canBuild(m1, m2), m3)

    def and[A5](m3: M[A5]) = this.~(m3)

    def apply[B](f: (A1, A2, A3, A4) => B)(implicit fu: Functor[M]): M[B] =
      fu.map[A1 ~ A2 ~ A3 ~ A4, B](canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 => f(a1, a2, a3, a4) })

    def apply[B](f: B => (A1, A2, A3, A4))(implicit fu: Contravariant[M]): M[B] =
      fu.contramap(canBuild(m1, m2))((b: B) => { val (a1, a2, a3, a4) = f(b); new ~(new ~(new ~(a1, a2), a3), a4) })

    def apply[B](f1: (A1, A2, A3, A4) => B, f2: B => (A1, A2, A3, A4))(implicit fu: Invariant[M]): M[B] =
      fu.imap[A1 ~ A2 ~ A3 ~ A4, B](
        canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 => f1(a1, a2, a3, a4) })(
        (b: B) => { val (a1, a2, a3, a4) = f2(b); new ~(new ~(new ~(a1, a2), a3), a4) }
      )

    def join[A >: A1](implicit witness1: <:<[A, A1], witness2: <:<[A, A2], witness3: <:<[A, A3], witness4: <:<[A, A4], fu: Contravariant[M]): M[A] =
      apply[A]((a: A) => (a: A1, a: A2, a: A3, a: A4))(fu)
  }

  class CanBuild5[A1, A2, A3, A4, A5](m1: M[A1 ~ A2 ~ A3 ~ A4], m2: M[A5]) {

    def ~[A6](m3: M[A6]) = new CanBuild6[A1, A2, A3, A4, A5, A6](canBuild(m1, m2), m3)

    def and[A6](m3: M[A6]) = this.~(m3)

    def apply[B](f: (A1, A2, A3, A4, A5) => B)(implicit fu: Functor[M]): M[B] =
      fu.map[A1 ~ A2 ~ A3 ~ A4 ~ A5, B](canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 ~ a5 => f(a1, a2, a3, a4, a5) })

    def apply[B](f: B => (A1, A2, A3, A4, A5))(implicit fu: Contravariant[M]): M[B] =
      fu.contramap(canBuild(m1, m2))((b: B) => { val (a1, a2, a3, a4, a5) = f(b); new ~(new ~(new ~(new ~(a1, a2), a3), a4), a5) })

    def apply[B](f1: (A1, A2, A3, A4, A5) => B, f2: B => (A1, A2, A3, A4, A5))(implicit fu: Invariant[M]): M[B] =
      fu.imap[A1 ~ A2 ~ A3 ~ A4 ~ A5, B](
        canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 ~ a5 => f1(a1, a2, a3, a4, a5) })(
        (b: B) => { val (a1, a2, a3, a4, a5) = f2(b); new ~(new ~(new ~(new ~(a1, a2), a3), a4), a5) }
      )

    def join[A >: A1](implicit witness1: <:<[A, A1], witness2: <:<[A, A2], witness3: <:<[A, A3], witness4: <:<[A, A4], witness5: <:<[A, A5], fu: Contravariant[M]): M[A] =
      apply[A]((a: A) => (a: A1, a: A2, a: A3, a: A4, a: A5))(fu)
  }

  class CanBuild6[A1, A2, A3, A4, A5, A6](m1: M[A1 ~ A2 ~ A3 ~ A4 ~ A5], m2: M[A6]) {

    def ~[A7](m3: M[A7]) = new CanBuild7[A1, A2, A3, A4, A5, A6, A7](canBuild(m1, m2), m3)

    def and[A7](m3: M[A7]) = this.~(m3)

    def apply[B](f: (A1, A2, A3, A4, A5, A6) => B)(implicit fu: Functor[M]): M[B] =
      fu.map[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6, B](canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 => f(a1, a2, a3, a4, a5, a6) })

    def apply[B](f: B => (A1, A2, A3, A4, A5, A6))(implicit fu: Contravariant[M]): M[B] =
      fu.contramap(canBuild(m1, m2))((b: B) => { val (a1, a2, a3, a4, a5, a6) = f(b); new ~(new ~(new ~(new ~(new ~(a1, a2), a3), a4), a5), a6) })

    def apply[B](f1: (A1, A2, A3, A4, A5, A6) => B, f2: B => (A1, A2, A3, A4, A5, A6))(implicit fu: Invariant[M]): M[B] =
      fu.imap[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6, B](
        canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 => f1(a1, a2, a3, a4, a5, a6) })(
        (b: B) => { val (a1, a2, a3, a4, a5, a6) = f2(b); new ~(new ~(new ~(new ~(new ~(a1, a2), a3), a4), a5), a6) }
      )

    def join[A >: A1](implicit witness1: <:<[A, A1], witness2: <:<[A, A2], witness3: <:<[A, A3], witness4: <:<[A, A4], witness5: <:<[A, A5], witness6: <:<[A, A6], fu: Contravariant[M]): M[A] =
      apply[A]((a: A) => (a: A1, a: A2, a: A3, a: A4, a: A5, a: A6))(fu)
  }

  class CanBuild7[A1, A2, A3, A4, A5, A6, A7](m1: M[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6], m2: M[A7]) {

    def ~[A8](m3: M[A8]) = new CanBuild8[A1, A2, A3, A4, A5, A6, A7, A8](canBuild(m1, m2), m3)

    def and[A8](m3: M[A8]) = this.~(m3)

    def apply[B](f: (A1, A2, A3, A4, A5, A6, A7) => B)(implicit fu: Functor[M]): M[B] =
      fu.map[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7, B](canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 ~ a7 => f(a1, a2, a3, a4, a5, a6, a7) })

    def apply[B](f: B => (A1, A2, A3, A4, A5, A6, A7))(implicit fu: Contravariant[M]): M[B] =
      fu.contramap(canBuild(m1, m2))((b: B) => { val (a1, a2, a3, a4, a5, a6, a7) = f(b); new ~(new ~(new ~(new ~(new ~(new ~(a1, a2), a3), a4), a5), a6), a7) })

    def apply[B](f1: (A1, A2, A3, A4, A5, A6, A7) => B, f2: B => (A1, A2, A3, A4, A5, A6, A7))(implicit fu: Invariant[M]): M[B] =
      fu.imap[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7, B](
        canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 ~ a7 => f1(a1, a2, a3, a4, a5, a6, a7) })(
        (b: B) => { val (a1, a2, a3, a4, a5, a6, a7) = f2(b); new ~(new ~(new ~(new ~(new ~(new ~(a1, a2), a3), a4), a5), a6), a7) }
      )

    def join[A >: A1](implicit witness1: <:<[A, A1], witness2: <:<[A, A2], witness3: <:<[A, A3], witness4: <:<[A, A4], witness5: <:<[A, A5], witness6: <:<[A, A6], witness7: <:<[A, A7], fu: Contravariant[M]): M[A] =
      apply[A]((a: A) => (a: A1, a: A2, a: A3, a: A4, a: A5, a: A6, a: A7))(fu)
  }

  class CanBuild8[A1, A2, A3, A4, A5, A6, A7, A8](m1: M[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7], m2: M[A8]) {

    def ~[A9](m3: M[A9]) = new CanBuild9[A1, A2, A3, A4, A5, A6, A7, A8, A9](canBuild(m1, m2), m3)

    def and[A9](m3: M[A9]) = this.~(m3)

    def apply[B](f: (A1, A2, A3, A4, A5, A6, A7, A8) => B)(implicit fu: Functor[M]): M[B] =
      fu.map[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8, B](canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 ~ a7 ~ a8 => f(a1, a2, a3, a4, a5, a6, a7, a8) })

    def apply[B](f: B => (A1, A2, A3, A4, A5, A6, A7, A8))(implicit fu: Contravariant[M]): M[B] =
      fu.contramap(canBuild(m1, m2))((b: B) => { val (a1, a2, a3, a4, a5, a6, a7, a8) = f(b); new ~(new ~(new ~(new ~(new ~(new ~(new ~(a1, a2), a3), a4), a5), a6), a7), a8) })

    def apply[B](f1: (A1, A2, A3, A4, A5, A6, A7, A8) => B, f2: B => (A1, A2, A3, A4, A5, A6, A7, A8))(implicit fu: Invariant[M]): M[B] =
      fu.imap[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8, B](
        canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 ~ a7 ~ a8 => f1(a1, a2, a3, a4, a5, a6, a7, a8) })(
        (b: B) => { val (a1, a2, a3, a4, a5, a6, a7, a8) = f2(b); new ~(new ~(new ~(new ~(new ~(new ~(new ~(a1, a2), a3), a4), a5), a6), a7), a8) }
      )

    def join[A >: A1](implicit witness1: <:<[A, A1], witness2: <:<[A, A2], witness3: <:<[A, A3], witness4: <:<[A, A4], witness5: <:<[A, A5], witness6: <:<[A, A6], witness7: <:<[A, A7], witness8: <:<[A, A8], fu: Contravariant[M]): M[A] =
      apply[A]((a: A) => (a: A1, a: A2, a: A3, a: A4, a: A5, a: A6, a: A7, a: A8))(fu)
  }

  class CanBuild9[A1, A2, A3, A4, A5, A6, A7, A8, A9](m1: M[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8], m2: M[A9]) {

    def ~[A10](m3: M[A10]) = new CanBuild10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](canBuild(m1, m2), m3)

    def and[A10](m3: M[A10]) = this.~(m3)

    def apply[B](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9) => B)(implicit fu: Functor[M]): M[B] =
      fu.map[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9, B](canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 ~ a7 ~ a8 ~ a9 => f(a1, a2, a3, a4, a5, a6, a7, a8, a9) })

    def apply[B](f: B => (A1, A2, A3, A4, A5, A6, A7, A8, A9))(implicit fu: Contravariant[M]): M[B] =
      fu.contramap(canBuild(m1, m2))((b: B) => { val (a1, a2, a3, a4, a5, a6, a7, a8, a9) = f(b); new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(a1, a2), a3), a4), a5), a6), a7), a8), a9) })

    def apply[B](f1: (A1, A2, A3, A4, A5, A6, A7, A8, A9) => B, f2: B => (A1, A2, A3, A4, A5, A6, A7, A8, A9))(implicit fu: Invariant[M]): M[B] =
      fu.imap[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9, B](
        canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 ~ a7 ~ a8 ~ a9 => f1(a1, a2, a3, a4, a5, a6, a7, a8, a9) })(
        (b: B) => { val (a1, a2, a3, a4, a5, a6, a7, a8, a9) = f2(b); new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(a1, a2), a3), a4), a5), a6), a7), a8), a9) }
      )

    def join[A >: A1](implicit witness1: <:<[A, A1], witness2: <:<[A, A2], witness3: <:<[A, A3], witness4: <:<[A, A4], witness5: <:<[A, A5], witness6: <:<[A, A6], witness7: <:<[A, A7], witness8: <:<[A, A8], witness9: <:<[A, A9], fu: Contravariant[M]): M[A] =
      apply[A]((a: A) => (a: A1, a: A2, a: A3, a: A4, a: A5, a: A6, a: A7, a: A8, a: A9))(fu)
  }

  class CanBuild10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](m1: M[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9], m2: M[A10]) {

    def ~[A11](m3: M[A11]) = new CanBuild11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](canBuild(m1, m2), m3)

    def and[A11](m3: M[A11]) = this.~(m3)

    def apply[B](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) => B)(implicit fu: Functor[M]): M[B] =
      fu.map[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10, B](canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 ~ a7 ~ a8 ~ a9 ~ a10 => f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) })

    def apply[B](f: B => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10))(implicit fu: Contravariant[M]): M[B] =
      fu.contramap(canBuild(m1, m2))((b: B) => { val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = f(b); new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(a1, a2), a3), a4), a5), a6), a7), a8), a9), a10) })

    def apply[B](f1: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) => B, f2: B => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10))(implicit fu: Invariant[M]): M[B] =
      fu.imap[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10, B](
        canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 ~ a7 ~ a8 ~ a9 ~ a10 => f1(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) })(
        (b: B) => { val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = f2(b); new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(a1, a2), a3), a4), a5), a6), a7), a8), a9), a10) }
      )

    def join[A >: A1](implicit witness1: <:<[A, A1], witness2: <:<[A, A2], witness3: <:<[A, A3], witness4: <:<[A, A4], witness5: <:<[A, A5], witness6: <:<[A, A6], witness7: <:<[A, A7], witness8: <:<[A, A8], witness9: <:<[A, A9], witness10: <:<[A, A10], fu: Contravariant[M]): M[A] =
      apply[A]((a: A) => (a: A1, a: A2, a: A3, a: A4, a: A5, a: A6, a: A7, a: A8, a: A9, a: A10))(fu)
  }

  class CanBuild11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](m1: M[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10], m2: M[A11]) {

    def ~[A12](m3: M[A12]) = new CanBuild12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](canBuild(m1, m2), m3)

    def and[A12](m3: M[A12]) = this.~(m3)

    def apply[B](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11) => B)(implicit fu: Functor[M]): M[B] =
      fu.map[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11, B](canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 ~ a7 ~ a8 ~ a9 ~ a10 ~ a11 => f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) })

    def apply[B](f: B => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11))(implicit fu: Contravariant[M]): M[B] =
      fu.contramap(canBuild(m1, m2))((b: B) => { val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) = f(b); new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(a1, a2), a3), a4), a5), a6), a7), a8), a9), a10), a11) })

    def apply[B](f1: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11) => B, f2: B => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11))(implicit fu: Invariant[M]): M[B] =
      fu.imap[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11, B](
        canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 ~ a7 ~ a8 ~ a9 ~ a10 ~ a11 => f1(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) })(
        (b: B) => { val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) = f2(b); new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(a1, a2), a3), a4), a5), a6), a7), a8), a9), a10), a11) }
      )

    def join[A >: A1](implicit witness1: <:<[A, A1], witness2: <:<[A, A2], witness3: <:<[A, A3], witness4: <:<[A, A4], witness5: <:<[A, A5], witness6: <:<[A, A6], witness7: <:<[A, A7], witness8: <:<[A, A8], witness9: <:<[A, A9], witness10: <:<[A, A10], witness11: <:<[A, A11], fu: Contravariant[M]): M[A] =
      apply[A]((a: A) => (a: A1, a: A2, a: A3, a: A4, a: A5, a: A6, a: A7, a: A8, a: A9, a: A10, a: A11))(fu)
  }

  class CanBuild12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](m1: M[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11], m2: M[A12]) {

    def ~[A13](m3: M[A13]) = new CanBuild13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](canBuild(m1, m2), m3)

    def and[A13](m3: M[A13]) = this.~(m3)

    def apply[B](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12) => B)(implicit fu: Functor[M]): M[B] =
      fu.map[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11 ~ A12, B](canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 ~ a7 ~ a8 ~ a9 ~ a10 ~ a11 ~ a12 => f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) })

    def apply[B](f: B => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12))(implicit fu: Contravariant[M]): M[B] =
      fu.contramap(canBuild(m1, m2))((b: B) => { val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) = f(b); new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(a1, a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12) })

    def apply[B](f1: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12) => B, f2: B => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12))(implicit fu: Invariant[M]): M[B] =
      fu.imap[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11 ~ A12, B](
        canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 ~ a7 ~ a8 ~ a9 ~ a10 ~ a11 ~ a12 => f1(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) })(
        (b: B) => { val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) = f2(b); new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(a1, a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12) }
      )

    def join[A >: A1](implicit witness1: <:<[A, A1], witness2: <:<[A, A2], witness3: <:<[A, A3], witness4: <:<[A, A4], witness5: <:<[A, A5], witness6: <:<[A, A6], witness7: <:<[A, A7], witness8: <:<[A, A8], witness9: <:<[A, A9], witness10: <:<[A, A10], witness11: <:<[A, A11], witness12: <:<[A, A12], fu: Contravariant[M]): M[A] =
      apply[A]((a: A) => (a: A1, a: A2, a: A3, a: A4, a: A5, a: A6, a: A7, a: A8, a: A9, a: A10, a: A11, a: A12))(fu)
  }

  class CanBuild13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](m1: M[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11 ~ A12], m2: M[A13]) {

    def ~[A14](m3: M[A14]) = new CanBuild14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](canBuild(m1, m2), m3)

    def and[A14](m3: M[A14]) = this.~(m3)

    def apply[B](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13) => B)(implicit fu: Functor[M]): M[B] =
      fu.map[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11 ~ A12 ~ A13, B](canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 ~ a7 ~ a8 ~ a9 ~ a10 ~ a11 ~ a12 ~ a13 => f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) })

    def apply[B](f: B => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13))(implicit fu: Contravariant[M]): M[B] =
      fu.contramap(canBuild(m1, m2))((b: B) => { val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) = f(b); new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(a1, a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13) })

    def apply[B](f1: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13) => B, f2: B => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13))(implicit fu: Invariant[M]): M[B] =
      fu.imap[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11 ~ A12 ~ A13, B](
        canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 ~ a7 ~ a8 ~ a9 ~ a10 ~ a11 ~ a12 ~ a13 => f1(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) })(
        (b: B) => { val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) = f2(b); new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(a1, a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13) }
      )

    def join[A >: A1](implicit witness1: <:<[A, A1], witness2: <:<[A, A2], witness3: <:<[A, A3], witness4: <:<[A, A4], witness5: <:<[A, A5], witness6: <:<[A, A6], witness7: <:<[A, A7], witness8: <:<[A, A8], witness9: <:<[A, A9], witness10: <:<[A, A10], witness11: <:<[A, A11], witness12: <:<[A, A12], witness13: <:<[A, A13], fu: Contravariant[M]): M[A] =
      apply[A]((a: A) => (a: A1, a: A2, a: A3, a: A4, a: A5, a: A6, a: A7, a: A8, a: A9, a: A10, a: A11, a: A12, a: A13))(fu)
  }

  class CanBuild14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](m1: M[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11 ~ A12 ~ A13], m2: M[A14]) {

    def ~[A15](m3: M[A15]) = new CanBuild15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](canBuild(m1, m2), m3)

    def and[A15](m3: M[A15]) = this.~(m3)

    def apply[B](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14) => B)(implicit fu: Functor[M]): M[B] =
      fu.map[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11 ~ A12 ~ A13 ~ A14, B](canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 ~ a7 ~ a8 ~ a9 ~ a10 ~ a11 ~ a12 ~ a13 ~ a14 => f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) })

    def apply[B](f: B => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14))(implicit fu: Contravariant[M]): M[B] =
      fu.contramap(canBuild(m1, m2))((b: B) => { val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) = f(b); new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(a1, a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14) })

    def apply[B](f1: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14) => B, f2: B => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14))(implicit fu: Invariant[M]): M[B] =
      fu.imap[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11 ~ A12 ~ A13 ~ A14, B](
        canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 ~ a7 ~ a8 ~ a9 ~ a10 ~ a11 ~ a12 ~ a13 ~ a14 => f1(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) })(
        (b: B) => { val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) = f2(b); new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(a1, a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14) }
      )

    def join[A >: A1](implicit witness1: <:<[A, A1], witness2: <:<[A, A2], witness3: <:<[A, A3], witness4: <:<[A, A4], witness5: <:<[A, A5], witness6: <:<[A, A6], witness7: <:<[A, A7], witness8: <:<[A, A8], witness9: <:<[A, A9], witness10: <:<[A, A10], witness11: <:<[A, A11], witness12: <:<[A, A12], witness13: <:<[A, A13], witness14: <:<[A, A14], fu: Contravariant[M]): M[A] =
      apply[A]((a: A) => (a: A1, a: A2, a: A3, a: A4, a: A5, a: A6, a: A7, a: A8, a: A9, a: A10, a: A11, a: A12, a: A13, a: A14))(fu)
  }

  class CanBuild15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](m1: M[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11 ~ A12 ~ A13 ~ A14], m2: M[A15]) {

    def ~[A16](m3: M[A16]) = new CanBuild16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](canBuild(m1, m2), m3)

    def and[A16](m3: M[A16]) = this.~(m3)

    def apply[B](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15) => B)(implicit fu: Functor[M]): M[B] =
      fu.map[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11 ~ A12 ~ A13 ~ A14 ~ A15, B](canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 ~ a7 ~ a8 ~ a9 ~ a10 ~ a11 ~ a12 ~ a13 ~ a14 ~ a15 => f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) })

    def apply[B](f: B => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15))(implicit fu: Contravariant[M]): M[B] =
      fu.contramap(canBuild(m1, m2))((b: B) => { val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) = f(b); new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(a1, a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14), a15) })

    def apply[B](f1: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15) => B, f2: B => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15))(implicit fu: Invariant[M]): M[B] =
      fu.imap[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11 ~ A12 ~ A13 ~ A14 ~ A15, B](
        canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 ~ a7 ~ a8 ~ a9 ~ a10 ~ a11 ~ a12 ~ a13 ~ a14 ~ a15 => f1(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) })(
        (b: B) => { val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) = f2(b); new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(a1, a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14), a15) }
      )

    def join[A >: A1](implicit witness1: <:<[A, A1], witness2: <:<[A, A2], witness3: <:<[A, A3], witness4: <:<[A, A4], witness5: <:<[A, A5], witness6: <:<[A, A6], witness7: <:<[A, A7], witness8: <:<[A, A8], witness9: <:<[A, A9], witness10: <:<[A, A10], witness11: <:<[A, A11], witness12: <:<[A, A12], witness13: <:<[A, A13], witness14: <:<[A, A14], witness15: <:<[A, A15], fu: Contravariant[M]): M[A] =
      apply[A]((a: A) => (a: A1, a: A2, a: A3, a: A4, a: A5, a: A6, a: A7, a: A8, a: A9, a: A10, a: A11, a: A12, a: A13, a: A14, a: A15))(fu)
  }

  class CanBuild16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](m1: M[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11 ~ A12 ~ A13 ~ A14 ~ A15], m2: M[A16]) {

    def ~[A17](m3: M[A17]) = new CanBuild17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](canBuild(m1, m2), m3)

    def and[A17](m3: M[A17]) = this.~(m3)

    def apply[B](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) => B)(implicit fu: Functor[M]): M[B] =
      fu.map[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11 ~ A12 ~ A13 ~ A14 ~ A15 ~ A16, B](canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 ~ a7 ~ a8 ~ a9 ~ a10 ~ a11 ~ a12 ~ a13 ~ a14 ~ a15 ~ a16 => f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16) })

    def apply[B](f: B => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16))(implicit fu: Contravariant[M]): M[B] =
      fu.contramap(canBuild(m1, m2))((b: B) => { val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16) = f(b); new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(a1, a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14), a15), a16) })

    def apply[B](f1: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) => B, f2: B => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16))(implicit fu: Invariant[M]): M[B] =
      fu.imap[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11 ~ A12 ~ A13 ~ A14 ~ A15 ~ A16, B](
        canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 ~ a7 ~ a8 ~ a9 ~ a10 ~ a11 ~ a12 ~ a13 ~ a14 ~ a15 ~ a16 => f1(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16) })(
        (b: B) => { val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16) = f2(b); new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(a1, a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14), a15), a16) }
      )

    def join[A >: A1](implicit witness1: <:<[A, A1], witness2: <:<[A, A2], witness3: <:<[A, A3], witness4: <:<[A, A4], witness5: <:<[A, A5], witness6: <:<[A, A6], witness7: <:<[A, A7], witness8: <:<[A, A8], witness9: <:<[A, A9], witness10: <:<[A, A10], witness11: <:<[A, A11], witness12: <:<[A, A12], witness13: <:<[A, A13], witness14: <:<[A, A14], witness15: <:<[A, A15], witness16: <:<[A, A16], fu: Contravariant[M]): M[A] =
      apply[A]((a: A) => (a: A1, a: A2, a: A3, a: A4, a: A5, a: A6, a: A7, a: A8, a: A9, a: A10, a: A11, a: A12, a: A13, a: A14, a: A15, a: A16))(fu)
  }

  class CanBuild17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](m1: M[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11 ~ A12 ~ A13 ~ A14 ~ A15 ~ A16], m2: M[A17]) {

    def ~[A18](m3: M[A18]) = new CanBuild18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](canBuild(m1, m2), m3)

    def and[A18](m3: M[A18]) = this.~(m3)

    def apply[B](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) => B)(implicit fu: Functor[M]): M[B] =
      fu.map[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11 ~ A12 ~ A13 ~ A14 ~ A15 ~ A16 ~ A17, B](canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 ~ a7 ~ a8 ~ a9 ~ a10 ~ a11 ~ a12 ~ a13 ~ a14 ~ a15 ~ a16 ~ a17 => f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17) })

    def apply[B](f: B => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17))(implicit fu: Contravariant[M]): M[B] =
      fu.contramap(canBuild(m1, m2))((b: B) => { val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17) = f(b); new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(a1, a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14), a15), a16), a17) })

    def apply[B](f1: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) => B, f2: B => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17))(implicit fu: Invariant[M]): M[B] =
      fu.imap[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11 ~ A12 ~ A13 ~ A14 ~ A15 ~ A16 ~ A17, B](
        canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 ~ a7 ~ a8 ~ a9 ~ a10 ~ a11 ~ a12 ~ a13 ~ a14 ~ a15 ~ a16 ~ a17 => f1(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17) })(
        (b: B) => { val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17) = f2(b); new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(a1, a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14), a15), a16), a17) }
      )

    def join[A >: A1](implicit witness1: <:<[A, A1], witness2: <:<[A, A2], witness3: <:<[A, A3], witness4: <:<[A, A4], witness5: <:<[A, A5], witness6: <:<[A, A6], witness7: <:<[A, A7], witness8: <:<[A, A8], witness9: <:<[A, A9], witness10: <:<[A, A10], witness11: <:<[A, A11], witness12: <:<[A, A12], witness13: <:<[A, A13], witness14: <:<[A, A14], witness15: <:<[A, A15], witness16: <:<[A, A16], witness17: <:<[A, A17], fu: Contravariant[M]): M[A] =
      apply[A]((a: A) => (a: A1, a: A2, a: A3, a: A4, a: A5, a: A6, a: A7, a: A8, a: A9, a: A10, a: A11, a: A12, a: A13, a: A14, a: A15, a: A16, a: A17))(fu)
  }

  class CanBuild18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](m1: M[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11 ~ A12 ~ A13 ~ A14 ~ A15 ~ A16 ~ A17], m2: M[A18]) {

    def ~[A19](m3: M[A19]) = new CanBuild19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](canBuild(m1, m2), m3)

    def and[A19](m3: M[A19]) = this.~(m3)

    def apply[B](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) => B)(implicit fu: Functor[M]): M[B] =
      fu.map[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11 ~ A12 ~ A13 ~ A14 ~ A15 ~ A16 ~ A17 ~ A18, B](canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 ~ a7 ~ a8 ~ a9 ~ a10 ~ a11 ~ a12 ~ a13 ~ a14 ~ a15 ~ a16 ~ a17 ~ a18 => f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18) })

    def apply[B](f: B => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18))(implicit fu: Contravariant[M]): M[B] =
      fu.contramap(canBuild(m1, m2))((b: B) => { val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18) = f(b); new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(a1, a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14), a15), a16), a17), a18) })

    def apply[B](f1: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) => B, f2: B => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18))(implicit fu: Invariant[M]): M[B] =
      fu.imap[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11 ~ A12 ~ A13 ~ A14 ~ A15 ~ A16 ~ A17 ~ A18, B](
        canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 ~ a7 ~ a8 ~ a9 ~ a10 ~ a11 ~ a12 ~ a13 ~ a14 ~ a15 ~ a16 ~ a17 ~ a18 => f1(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18) })(
        (b: B) => { val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18) = f2(b); new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(a1, a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14), a15), a16), a17), a18) }
      )

    def join[A >: A1](implicit witness1: <:<[A, A1], witness2: <:<[A, A2], witness3: <:<[A, A3], witness4: <:<[A, A4], witness5: <:<[A, A5], witness6: <:<[A, A6], witness7: <:<[A, A7], witness8: <:<[A, A8], witness9: <:<[A, A9], witness10: <:<[A, A10], witness11: <:<[A, A11], witness12: <:<[A, A12], witness13: <:<[A, A13], witness14: <:<[A, A14], witness15: <:<[A, A15], witness16: <:<[A, A16], witness17: <:<[A, A17], witness18: <:<[A, A18], fu: Contravariant[M]): M[A] =
      apply[A]((a: A) => (a: A1, a: A2, a: A3, a: A4, a: A5, a: A6, a: A7, a: A8, a: A9, a: A10, a: A11, a: A12, a: A13, a: A14, a: A15, a: A16, a: A17, a: A18))(fu)
  }

  class CanBuild19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](m1: M[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11 ~ A12 ~ A13 ~ A14 ~ A15 ~ A16 ~ A17 ~ A18], m2: M[A19]) {

    def ~[A20](m3: M[A20]) = new CanBuild20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](canBuild(m1, m2), m3)

    def and[A20](m3: M[A20]) = this.~(m3)

    def apply[B](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) => B)(implicit fu: Functor[M]): M[B] =
      fu.map[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11 ~ A12 ~ A13 ~ A14 ~ A15 ~ A16 ~ A17 ~ A18 ~ A19, B](canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 ~ a7 ~ a8 ~ a9 ~ a10 ~ a11 ~ a12 ~ a13 ~ a14 ~ a15 ~ a16 ~ a17 ~ a18 ~ a19 => f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19) })

    def apply[B](f: B => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19))(implicit fu: Contravariant[M]): M[B] =
      fu.contramap(canBuild(m1, m2))((b: B) => { val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19) = f(b); new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(a1, a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14), a15), a16), a17), a18), a19) })

    def apply[B](f1: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) => B, f2: B => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19))(implicit fu: Invariant[M]): M[B] =
      fu.imap[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11 ~ A12 ~ A13 ~ A14 ~ A15 ~ A16 ~ A17 ~ A18 ~ A19, B](
        canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 ~ a7 ~ a8 ~ a9 ~ a10 ~ a11 ~ a12 ~ a13 ~ a14 ~ a15 ~ a16 ~ a17 ~ a18 ~ a19 => f1(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19) })(
        (b: B) => { val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19) = f2(b); new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(a1, a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14), a15), a16), a17), a18), a19) }
      )

    def join[A >: A1](implicit witness1: <:<[A, A1], witness2: <:<[A, A2], witness3: <:<[A, A3], witness4: <:<[A, A4], witness5: <:<[A, A5], witness6: <:<[A, A6], witness7: <:<[A, A7], witness8: <:<[A, A8], witness9: <:<[A, A9], witness10: <:<[A, A10], witness11: <:<[A, A11], witness12: <:<[A, A12], witness13: <:<[A, A13], witness14: <:<[A, A14], witness15: <:<[A, A15], witness16: <:<[A, A16], witness17: <:<[A, A17], witness18: <:<[A, A18], witness19: <:<[A, A19], fu: Contravariant[M]): M[A] =
      apply[A]((a: A) => (a: A1, a: A2, a: A3, a: A4, a: A5, a: A6, a: A7, a: A8, a: A9, a: A10, a: A11, a: A12, a: A13, a: A14, a: A15, a: A16, a: A17, a: A18, a: A19))(fu)
  }

  class CanBuild20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](m1: M[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11 ~ A12 ~ A13 ~ A14 ~ A15 ~ A16 ~ A17 ~ A18 ~ A19], m2: M[A20]) {

    def ~[A21](m3: M[A21]) = new CanBuild21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](canBuild(m1, m2), m3)

    def and[A21](m3: M[A21]) = this.~(m3)

    def apply[B](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) => B)(implicit fu: Functor[M]): M[B] =
      fu.map[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11 ~ A12 ~ A13 ~ A14 ~ A15 ~ A16 ~ A17 ~ A18 ~ A19 ~ A20, B](canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 ~ a7 ~ a8 ~ a9 ~ a10 ~ a11 ~ a12 ~ a13 ~ a14 ~ a15 ~ a16 ~ a17 ~ a18 ~ a19 ~ a20 => f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20) })

    def apply[B](f: B => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20))(implicit fu: Contravariant[M]): M[B] =
      fu.contramap(canBuild(m1, m2))((b: B) => { val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20) = f(b); new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(a1, a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14), a15), a16), a17), a18), a19), a20) })

    def apply[B](f1: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) => B, f2: B => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20))(implicit fu: Invariant[M]): M[B] =
      fu.imap[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11 ~ A12 ~ A13 ~ A14 ~ A15 ~ A16 ~ A17 ~ A18 ~ A19 ~ A20, B](
        canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 ~ a7 ~ a8 ~ a9 ~ a10 ~ a11 ~ a12 ~ a13 ~ a14 ~ a15 ~ a16 ~ a17 ~ a18 ~ a19 ~ a20 => f1(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20) })(
        (b: B) => { val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20) = f2(b); new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(a1, a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14), a15), a16), a17), a18), a19), a20) }
      )

    def join[A >: A1](implicit witness1: <:<[A, A1], witness2: <:<[A, A2], witness3: <:<[A, A3], witness4: <:<[A, A4], witness5: <:<[A, A5], witness6: <:<[A, A6], witness7: <:<[A, A7], witness8: <:<[A, A8], witness9: <:<[A, A9], witness10: <:<[A, A10], witness11: <:<[A, A11], witness12: <:<[A, A12], witness13: <:<[A, A13], witness14: <:<[A, A14], witness15: <:<[A, A15], witness16: <:<[A, A16], witness17: <:<[A, A17], witness18: <:<[A, A18], witness19: <:<[A, A19], witness20: <:<[A, A20], fu: Contravariant[M]): M[A] =
      apply[A]((a: A) => (a: A1, a: A2, a: A3, a: A4, a: A5, a: A6, a: A7, a: A8, a: A9, a: A10, a: A11, a: A12, a: A13, a: A14, a: A15, a: A16, a: A17, a: A18, a: A19, a: A20))(fu)
  }

  class CanBuild21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](m1: M[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11 ~ A12 ~ A13 ~ A14 ~ A15 ~ A16 ~ A17 ~ A18 ~ A19 ~ A20], m2: M[A21]) {
    def ~[A22](m3: M[A22]) = new CanBuild22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](canBuild(m1, m2), m3)

    def and[A22](m3: M[A22]) = this.~(m3)

    def apply[B](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) => B)(implicit fu: Functor[M]): M[B] =
      fu.map[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11 ~ A12 ~ A13 ~ A14 ~ A15 ~ A16 ~ A17 ~ A18 ~ A19 ~ A20 ~ A21, B](canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 ~ a7 ~ a8 ~ a9 ~ a10 ~ a11 ~ a12 ~ a13 ~ a14 ~ a15 ~ a16 ~ a17 ~ a18 ~ a19 ~ a20 ~ a21 => f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21) })

    def apply[B](f: B => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21))(implicit fu: Contravariant[M]): M[B] =
      fu.contramap(canBuild(m1, m2))((b: B) => { val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21) = f(b); new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(a1, a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14), a15), a16), a17), a18), a19), a20), a21) })

    def apply[B](f1: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) => B, f2: B => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21))(implicit fu: Invariant[M]): M[B] =
      fu.imap[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11 ~ A12 ~ A13 ~ A14 ~ A15 ~ A16 ~ A17 ~ A18 ~ A19 ~ A20 ~ A21, B](
        canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 ~ a7 ~ a8 ~ a9 ~ a10 ~ a11 ~ a12 ~ a13 ~ a14 ~ a15 ~ a16 ~ a17 ~ a18 ~ a19 ~ a20 ~ a21 => f1(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21) })(
        (b: B) => { val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21) = f2(b); new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(a1, a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14), a15), a16), a17), a18), a19), a20), a21) }
      )

    def join[A >: A1](implicit witness1: <:<[A, A1], witness2: <:<[A, A2], witness3: <:<[A, A3], witness4: <:<[A, A4], witness5: <:<[A, A5], witness6: <:<[A, A6], witness7: <:<[A, A7], witness8: <:<[A, A8], witness9: <:<[A, A9], witness10: <:<[A, A10], witness11: <:<[A, A11], witness12: <:<[A, A12], witness13: <:<[A, A13], witness14: <:<[A, A14], witness15: <:<[A, A15], witness16: <:<[A, A16], witness17: <:<[A, A17], witness18: <:<[A, A18], witness19: <:<[A, A19], witness20: <:<[A, A20], witness21: <:<[A, A21], fu: Contravariant[M]): M[A] =
      apply[A]((a: A) => (a: A1, a: A2, a: A3, a: A4, a: A5, a: A6, a: A7, a: A8, a: A9, a: A10, a: A11, a: A12, a: A13, a: A14, a: A15, a: A16, a: A17, a: A18, a: A19, a: A20, a: A21))(fu)
  }

  class CanBuild22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](m1: M[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11 ~ A12 ~ A13 ~ A14 ~ A15 ~ A16 ~ A17 ~ A18 ~ A19 ~ A20 ~ A21], m2: M[A22]) {

    def apply[B](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) => B)(implicit fu: Functor[M]): M[B] =
      fu.map[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11 ~ A12 ~ A13 ~ A14 ~ A15 ~ A16 ~ A17 ~ A18 ~ A19 ~ A20 ~ A21 ~ A22, B](canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 ~ a7 ~ a8 ~ a9 ~ a10 ~ a11 ~ a12 ~ a13 ~ a14 ~ a15 ~ a16 ~ a17 ~ a18 ~ a19 ~ a20 ~ a21 ~ a22 => f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22) })

    def apply[B](f: B => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22))(implicit fu: Contravariant[M]): M[B] =
      fu.contramap(canBuild(m1, m2))((b: B) => { val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22) = f(b); new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(a1, a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14), a15), a16), a17), a18), a19), a20), a21), a22) })

    def apply[B](f1: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) => B, f2: B => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22))(implicit fu: Invariant[M]): M[B] =
      fu.imap[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11 ~ A12 ~ A13 ~ A14 ~ A15 ~ A16 ~ A17 ~ A18 ~ A19 ~ A20 ~ A21 ~ A22, B](
        canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 ~ a7 ~ a8 ~ a9 ~ a10 ~ a11 ~ a12 ~ a13 ~ a14 ~ a15 ~ a16 ~ a17 ~ a18 ~ a19 ~ a20 ~ a21 ~ a22 => f1(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22) })(
        (b: B) => { val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22) = f2(b); new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(a1, a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14), a15), a16), a17), a18), a19), a20), a21), a22) }
      )

    def join[A >: A1](implicit witness1: <:<[A, A1], witness2: <:<[A, A2], witness3: <:<[A, A3], witness4: <:<[A, A4], witness5: <:<[A, A5], witness6: <:<[A, A6], witness7: <:<[A, A7], witness8: <:<[A, A8], witness9: <:<[A, A9], witness10: <:<[A, A10], witness11: <:<[A, A11], witness12: <:<[A, A12], witness13: <:<[A, A13], witness14: <:<[A, A14], witness15: <:<[A, A15], witness16: <:<[A, A16], witness17: <:<[A, A17], witness18: <:<[A, A18], witness19: <:<[A, A19], witness20: <:<[A, A20], witness21: <:<[A, A21], witness22: <:<[A, A22], fu: Contravariant[M]): M[A] =
      apply[A]((a: A) => (a: A1, a: A2, a: A3, a: A4, a: A5, a: A6, a: A7, a: A8, a: A9, a: A10, a: A11, a: A12, a: A13, a: A14, a: A15, a: A16, a: A17, a: A18, a: A19, a: A20, a: A21, a: A22))(fu)
  }
}
