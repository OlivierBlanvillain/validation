package play.api.data.mapping

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
}

class FunctionalBuilder[M[_]](canBuild: FunctionalCanBuild[M]) {

  class CanBuild2[A1, A2](m1: M[A1], m2: M[A2]) {

    def ~[A3](m3: M[A3]) = new CanBuild3[A1, A2, A3](canBuild(m1, m2), m3)

    def apply[B](f: (A1, A2) => B)(implicit fu: Functor[M]): M[B] =
      fu.map[A1 ~ A2, B](canBuild(m1, m2))({ case a1 ~ a2 => f(a1, a2) })

    def apply[B](f: B => (A1, A2))(implicit fu: Contravariant[M]): M[B] =
      fu.contramap(canBuild(m1, m2))((b: B) => { val (a1, a2) = f(b); new ~(a1, a2) })

    def apply[B](f1: (A1, A2) => B, f2: B => (A1, A2))(implicit fu: Invariant[M]): M[B] =
      fu.imap[A1 ~ A2, B](
        canBuild(m1, m2))({ case a1 ~ a2 => f1(a1, a2) })(
        (b: B) => { val (a1, a2) = f2(b); new ~(a1, a2) }
      )
      
    def tupled(implicit fu: Invariant[M]): M[(A1, A2)] = apply[(A1, A2)]({ (a1: A1, a2: A2) => (a1, a2) }, { (a: (A1, A2)) => (a._1, a._2) })(fu)
  }
  
  class CanBuild3[A1, A2, A3](m1: M[A1 ~ A2], m2: M[A3]) {

    def ~[A4](m3: M[A4]) = new CanBuild4[A1, A2, A3, A4](canBuild(m1, m2), m3)

    def apply[B](f: (A1, A2, A3) => B)(implicit fu: Functor[M]): M[B] =
      fu.map[A1 ~ A2 ~ A3, B](canBuild(m1, m2))({ case a1 ~ a2 ~ a3 => f(a1, a2, a3) })

    def apply[B](f: B => (A1, A2, A3))(implicit fu: Contravariant[M]): M[B] =
      fu.contramap(canBuild(m1, m2))((b: B) => { val (a1, a2, a3) = f(b); new ~(new ~(a1, a2), a3) })

    def apply[B](f1: (A1, A2, A3) => B, f2: B => (A1, A2, A3))(implicit fu: Invariant[M]): M[B] =
      fu.imap[A1 ~ A2 ~ A3, B](
        canBuild(m1, m2))({ case a1 ~ a2 ~ a3 => f1(a1, a2, a3) })(
        (b: B) => { val (a1, a2, a3) = f2(b); new ~(new ~(a1, a2), a3) }
      )
      
    def tupled(implicit fu: Invariant[M]): M[(A1, A2, A3)] = apply[(A1, A2, A3)]({ (a1: A1, a2: A2, a3: A3) => (a1, a2, a3) }, { (a: (A1, A2, A3)) => (a._1, a._2, a._3) })(fu)
  }

  class CanBuild4[A1, A2, A3, A4](m1: M[A1 ~ A2 ~ A3], m2: M[A4]) {

    def ~[A5](m3: M[A5]) = new CanBuild5[A1, A2, A3, A4, A5](canBuild(m1, m2), m3)

    def apply[B](f: (A1, A2, A3, A4) => B)(implicit fu: Functor[M]): M[B] =
      fu.map[A1 ~ A2 ~ A3 ~ A4, B](canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 => f(a1, a2, a3, a4) })

    def apply[B](f: B => (A1, A2, A3, A4))(implicit fu: Contravariant[M]): M[B] =
      fu.contramap(canBuild(m1, m2))((b: B) => { val (a1, a2, a3, a4) = f(b); new ~(new ~(new ~(a1, a2), a3), a4) })

    def apply[B](f1: (A1, A2, A3, A4) => B, f2: B => (A1, A2, A3, A4))(implicit fu: Invariant[M]): M[B] =
      fu.imap[A1 ~ A2 ~ A3 ~ A4, B](
        canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 => f1(a1, a2, a3, a4) })(
        (b: B) => { val (a1, a2, a3, a4) = f2(b); new ~(new ~(new ~(a1, a2), a3), a4) }
      )
  }

  class CanBuild5[A1, A2, A3, A4, A5](m1: M[A1 ~ A2 ~ A3 ~ A4], m2: M[A5]) {

    def ~[A6](m3: M[A6]) = new CanBuild6[A1, A2, A3, A4, A5, A6](canBuild(m1, m2), m3)

    def apply[B](f: (A1, A2, A3, A4, A5) => B)(implicit fu: Functor[M]): M[B] =
      fu.map[A1 ~ A2 ~ A3 ~ A4 ~ A5, B](canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 ~ a5 => f(a1, a2, a3, a4, a5) })

    def apply[B](f: B => (A1, A2, A3, A4, A5))(implicit fu: Contravariant[M]): M[B] =
      fu.contramap(canBuild(m1, m2))((b: B) => { val (a1, a2, a3, a4, a5) = f(b); new ~(new ~(new ~(new ~(a1, a2), a3), a4), a5) })

    def apply[B](f1: (A1, A2, A3, A4, A5) => B, f2: B => (A1, A2, A3, A4, A5))(implicit fu: Invariant[M]): M[B] =
      fu.imap[A1 ~ A2 ~ A3 ~ A4 ~ A5, B](
        canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 ~ a5 => f1(a1, a2, a3, a4, a5) })(
        (b: B) => { val (a1, a2, a3, a4, a5) = f2(b); new ~(new ~(new ~(new ~(a1, a2), a3), a4), a5) }
      )
  }

  class CanBuild6[A1, A2, A3, A4, A5, A6](m1: M[A1 ~ A2 ~ A3 ~ A4 ~ A5], m2: M[A6]) {

    def ~[A7](m3: M[A7]) = new CanBuild7[A1, A2, A3, A4, A5, A6, A7](canBuild(m1, m2), m3)

    def apply[B](f: (A1, A2, A3, A4, A5, A6) => B)(implicit fu: Functor[M]): M[B] =
      fu.map[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6, B](canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 => f(a1, a2, a3, a4, a5, a6) })

    def apply[B](f: B => (A1, A2, A3, A4, A5, A6))(implicit fu: Contravariant[M]): M[B] =
      fu.contramap(canBuild(m1, m2))((b: B) => { val (a1, a2, a3, a4, a5, a6) = f(b); new ~(new ~(new ~(new ~(new ~(a1, a2), a3), a4), a5), a6) })

    def apply[B](f1: (A1, A2, A3, A4, A5, A6) => B, f2: B => (A1, A2, A3, A4, A5, A6))(implicit fu: Invariant[M]): M[B] =
      fu.imap[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6, B](
        canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 => f1(a1, a2, a3, a4, a5, a6) })(
        (b: B) => { val (a1, a2, a3, a4, a5, a6) = f2(b); new ~(new ~(new ~(new ~(new ~(a1, a2), a3), a4), a5), a6) }
      )
  }

  class CanBuild7[A1, A2, A3, A4, A5, A6, A7](m1: M[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6], m2: M[A7]) {

    def ~[A8](m3: M[A8]) = new CanBuild8[A1, A2, A3, A4, A5, A6, A7, A8](canBuild(m1, m2), m3)

    def apply[B](f: (A1, A2, A3, A4, A5, A6, A7) => B)(implicit fu: Functor[M]): M[B] =
      fu.map[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7, B](canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 ~ a7 => f(a1, a2, a3, a4, a5, a6, a7) })

    def apply[B](f: B => (A1, A2, A3, A4, A5, A6, A7))(implicit fu: Contravariant[M]): M[B] =
      fu.contramap(canBuild(m1, m2))((b: B) => { val (a1, a2, a3, a4, a5, a6, a7) = f(b); new ~(new ~(new ~(new ~(new ~(new ~(a1, a2), a3), a4), a5), a6), a7) })

    def apply[B](f1: (A1, A2, A3, A4, A5, A6, A7) => B, f2: B => (A1, A2, A3, A4, A5, A6, A7))(implicit fu: Invariant[M]): M[B] =
      fu.imap[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7, B](
        canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 ~ a7 => f1(a1, a2, a3, a4, a5, a6, a7) })(
        (b: B) => { val (a1, a2, a3, a4, a5, a6, a7) = f2(b); new ~(new ~(new ~(new ~(new ~(new ~(a1, a2), a3), a4), a5), a6), a7) }
      )
  }

  class CanBuild8[A1, A2, A3, A4, A5, A6, A7, A8](m1: M[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7], m2: M[A8]) {

    def ~[A9](m3: M[A9]) = new CanBuild9[A1, A2, A3, A4, A5, A6, A7, A8, A9](canBuild(m1, m2), m3)

    def apply[B](f: (A1, A2, A3, A4, A5, A6, A7, A8) => B)(implicit fu: Functor[M]): M[B] =
      fu.map[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8, B](canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 ~ a7 ~ a8 => f(a1, a2, a3, a4, a5, a6, a7, a8) })

    def apply[B](f: B => (A1, A2, A3, A4, A5, A6, A7, A8))(implicit fu: Contravariant[M]): M[B] =
      fu.contramap(canBuild(m1, m2))((b: B) => { val (a1, a2, a3, a4, a5, a6, a7, a8) = f(b); new ~(new ~(new ~(new ~(new ~(new ~(new ~(a1, a2), a3), a4), a5), a6), a7), a8) })

    def apply[B](f1: (A1, A2, A3, A4, A5, A6, A7, A8) => B, f2: B => (A1, A2, A3, A4, A5, A6, A7, A8))(implicit fu: Invariant[M]): M[B] =
      fu.imap[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8, B](
        canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 ~ a7 ~ a8 => f1(a1, a2, a3, a4, a5, a6, a7, a8) })(
        (b: B) => { val (a1, a2, a3, a4, a5, a6, a7, a8) = f2(b); new ~(new ~(new ~(new ~(new ~(new ~(new ~(a1, a2), a3), a4), a5), a6), a7), a8) }
      )
  }

  class CanBuild9[A1, A2, A3, A4, A5, A6, A7, A8, A9](m1: M[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8], m2: M[A9]) {

    def ~[A10](m3: M[A10]) = new CanBuild10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](canBuild(m1, m2), m3)

    def apply[B](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9) => B)(implicit fu: Functor[M]): M[B] =
      fu.map[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9, B](canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 ~ a7 ~ a8 ~ a9 => f(a1, a2, a3, a4, a5, a6, a7, a8, a9) })

    def apply[B](f: B => (A1, A2, A3, A4, A5, A6, A7, A8, A9))(implicit fu: Contravariant[M]): M[B] =
      fu.contramap(canBuild(m1, m2))((b: B) => { val (a1, a2, a3, a4, a5, a6, a7, a8, a9) = f(b); new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(a1, a2), a3), a4), a5), a6), a7), a8), a9) })

    def apply[B](f1: (A1, A2, A3, A4, A5, A6, A7, A8, A9) => B, f2: B => (A1, A2, A3, A4, A5, A6, A7, A8, A9))(implicit fu: Invariant[M]): M[B] =
      fu.imap[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9, B](
        canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 ~ a7 ~ a8 ~ a9 => f1(a1, a2, a3, a4, a5, a6, a7, a8, a9) })(
        (b: B) => { val (a1, a2, a3, a4, a5, a6, a7, a8, a9) = f2(b); new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(a1, a2), a3), a4), a5), a6), a7), a8), a9) }
      )
  }

  class CanBuild10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](m1: M[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9], m2: M[A10]) {

    def ~[A11](m3: M[A11]) = new CanBuild11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](canBuild(m1, m2), m3)

    def apply[B](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) => B)(implicit fu: Functor[M]): M[B] =
      fu.map[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10, B](canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 ~ a7 ~ a8 ~ a9 ~ a10 => f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) })

    def apply[B](f: B => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10))(implicit fu: Contravariant[M]): M[B] =
      fu.contramap(canBuild(m1, m2))((b: B) => { val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = f(b); new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(a1, a2), a3), a4), a5), a6), a7), a8), a9), a10) })

    def apply[B](f1: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) => B, f2: B => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10))(implicit fu: Invariant[M]): M[B] =
      fu.imap[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10, B](
        canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 ~ a7 ~ a8 ~ a9 ~ a10 => f1(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) })(
        (b: B) => { val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = f2(b); new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(a1, a2), a3), a4), a5), a6), a7), a8), a9), a10) }
      )
  }

  class CanBuild11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](m1: M[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10], m2: M[A11]) {

    def ~[A12](m3: M[A12]) = new CanBuild12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](canBuild(m1, m2), m3)

    def apply[B](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11) => B)(implicit fu: Functor[M]): M[B] =
      fu.map[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11, B](canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 ~ a7 ~ a8 ~ a9 ~ a10 ~ a11 => f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) })

    def apply[B](f: B => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11))(implicit fu: Contravariant[M]): M[B] =
      fu.contramap(canBuild(m1, m2))((b: B) => { val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) = f(b); new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(a1, a2), a3), a4), a5), a6), a7), a8), a9), a10), a11) })

    def apply[B](f1: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11) => B, f2: B => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11))(implicit fu: Invariant[M]): M[B] =
      fu.imap[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11, B](
        canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 ~ a7 ~ a8 ~ a9 ~ a10 ~ a11 => f1(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) })(
        (b: B) => { val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) = f2(b); new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(a1, a2), a3), a4), a5), a6), a7), a8), a9), a10), a11) }
      )
  }

  class CanBuild12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](m1: M[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11], m2: M[A12]) {

    def ~[A13](m3: M[A13]) = new CanBuild13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](canBuild(m1, m2), m3)

    def apply[B](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12) => B)(implicit fu: Functor[M]): M[B] =
      fu.map[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11 ~ A12, B](canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 ~ a7 ~ a8 ~ a9 ~ a10 ~ a11 ~ a12 => f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) })

    def apply[B](f: B => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12))(implicit fu: Contravariant[M]): M[B] =
      fu.contramap(canBuild(m1, m2))((b: B) => { val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) = f(b); new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(a1, a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12) })

    def apply[B](f1: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12) => B, f2: B => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12))(implicit fu: Invariant[M]): M[B] =
      fu.imap[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11 ~ A12, B](
        canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 ~ a7 ~ a8 ~ a9 ~ a10 ~ a11 ~ a12 => f1(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) })(
        (b: B) => { val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) = f2(b); new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(a1, a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12) }
      )
  }

  class CanBuild13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](m1: M[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11 ~ A12], m2: M[A13]) {

    def ~[A14](m3: M[A14]) = new CanBuild14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](canBuild(m1, m2), m3)

    def apply[B](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13) => B)(implicit fu: Functor[M]): M[B] =
      fu.map[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11 ~ A12 ~ A13, B](canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 ~ a7 ~ a8 ~ a9 ~ a10 ~ a11 ~ a12 ~ a13 => f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) })

    def apply[B](f: B => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13))(implicit fu: Contravariant[M]): M[B] =
      fu.contramap(canBuild(m1, m2))((b: B) => { val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) = f(b); new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(a1, a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13) })

    def apply[B](f1: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13) => B, f2: B => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13))(implicit fu: Invariant[M]): M[B] =
      fu.imap[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11 ~ A12 ~ A13, B](
        canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 ~ a7 ~ a8 ~ a9 ~ a10 ~ a11 ~ a12 ~ a13 => f1(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) })(
        (b: B) => { val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) = f2(b); new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(a1, a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13) }
      )
  }

  class CanBuild14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](m1: M[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11 ~ A12 ~ A13], m2: M[A14]) {

    def ~[A15](m3: M[A15]) = new CanBuild15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](canBuild(m1, m2), m3)

    def apply[B](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14) => B)(implicit fu: Functor[M]): M[B] =
      fu.map[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11 ~ A12 ~ A13 ~ A14, B](canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 ~ a7 ~ a8 ~ a9 ~ a10 ~ a11 ~ a12 ~ a13 ~ a14 => f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) })

    def apply[B](f: B => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14))(implicit fu: Contravariant[M]): M[B] =
      fu.contramap(canBuild(m1, m2))((b: B) => { val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) = f(b); new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(a1, a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14) })

    def apply[B](f1: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14) => B, f2: B => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14))(implicit fu: Invariant[M]): M[B] =
      fu.imap[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11 ~ A12 ~ A13 ~ A14, B](
        canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 ~ a7 ~ a8 ~ a9 ~ a10 ~ a11 ~ a12 ~ a13 ~ a14 => f1(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) })(
        (b: B) => { val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) = f2(b); new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(a1, a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14) }
      )
  }

  class CanBuild15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](m1: M[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11 ~ A12 ~ A13 ~ A14], m2: M[A15]) {

    def ~[A16](m3: M[A16]) = new CanBuild16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](canBuild(m1, m2), m3)

    def apply[B](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15) => B)(implicit fu: Functor[M]): M[B] =
      fu.map[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11 ~ A12 ~ A13 ~ A14 ~ A15, B](canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 ~ a7 ~ a8 ~ a9 ~ a10 ~ a11 ~ a12 ~ a13 ~ a14 ~ a15 => f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) })

    def apply[B](f: B => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15))(implicit fu: Contravariant[M]): M[B] =
      fu.contramap(canBuild(m1, m2))((b: B) => { val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) = f(b); new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(a1, a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14), a15) })

    def apply[B](f1: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15) => B, f2: B => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15))(implicit fu: Invariant[M]): M[B] =
      fu.imap[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11 ~ A12 ~ A13 ~ A14 ~ A15, B](
        canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 ~ a7 ~ a8 ~ a9 ~ a10 ~ a11 ~ a12 ~ a13 ~ a14 ~ a15 => f1(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) })(
        (b: B) => { val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) = f2(b); new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(a1, a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14), a15) }
      )
  }

  class CanBuild16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](m1: M[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11 ~ A12 ~ A13 ~ A14 ~ A15], m2: M[A16]) {

    def ~[A17](m3: M[A17]) = new CanBuild17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](canBuild(m1, m2), m3)

    def apply[B](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) => B)(implicit fu: Functor[M]): M[B] =
      fu.map[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11 ~ A12 ~ A13 ~ A14 ~ A15 ~ A16, B](canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 ~ a7 ~ a8 ~ a9 ~ a10 ~ a11 ~ a12 ~ a13 ~ a14 ~ a15 ~ a16 => f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16) })

    def apply[B](f: B => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16))(implicit fu: Contravariant[M]): M[B] =
      fu.contramap(canBuild(m1, m2))((b: B) => { val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16) = f(b); new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(a1, a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14), a15), a16) })

    def apply[B](f1: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) => B, f2: B => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16))(implicit fu: Invariant[M]): M[B] =
      fu.imap[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11 ~ A12 ~ A13 ~ A14 ~ A15 ~ A16, B](
        canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 ~ a7 ~ a8 ~ a9 ~ a10 ~ a11 ~ a12 ~ a13 ~ a14 ~ a15 ~ a16 => f1(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16) })(
        (b: B) => { val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16) = f2(b); new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(a1, a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14), a15), a16) }
      )
  }

  class CanBuild17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](m1: M[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11 ~ A12 ~ A13 ~ A14 ~ A15 ~ A16], m2: M[A17]) {

    def ~[A18](m3: M[A18]) = new CanBuild18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](canBuild(m1, m2), m3)

    def apply[B](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) => B)(implicit fu: Functor[M]): M[B] =
      fu.map[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11 ~ A12 ~ A13 ~ A14 ~ A15 ~ A16 ~ A17, B](canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 ~ a7 ~ a8 ~ a9 ~ a10 ~ a11 ~ a12 ~ a13 ~ a14 ~ a15 ~ a16 ~ a17 => f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17) })

    def apply[B](f: B => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17))(implicit fu: Contravariant[M]): M[B] =
      fu.contramap(canBuild(m1, m2))((b: B) => { val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17) = f(b); new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(a1, a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14), a15), a16), a17) })

    def apply[B](f1: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) => B, f2: B => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17))(implicit fu: Invariant[M]): M[B] =
      fu.imap[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11 ~ A12 ~ A13 ~ A14 ~ A15 ~ A16 ~ A17, B](
        canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 ~ a7 ~ a8 ~ a9 ~ a10 ~ a11 ~ a12 ~ a13 ~ a14 ~ a15 ~ a16 ~ a17 => f1(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17) })(
        (b: B) => { val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17) = f2(b); new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(a1, a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14), a15), a16), a17) }
      )
  }

  class CanBuild18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](m1: M[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11 ~ A12 ~ A13 ~ A14 ~ A15 ~ A16 ~ A17], m2: M[A18]) {

    def ~[A19](m3: M[A19]) = new CanBuild19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](canBuild(m1, m2), m3)

    def apply[B](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) => B)(implicit fu: Functor[M]): M[B] =
      fu.map[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11 ~ A12 ~ A13 ~ A14 ~ A15 ~ A16 ~ A17 ~ A18, B](canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 ~ a7 ~ a8 ~ a9 ~ a10 ~ a11 ~ a12 ~ a13 ~ a14 ~ a15 ~ a16 ~ a17 ~ a18 => f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18) })

    def apply[B](f: B => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18))(implicit fu: Contravariant[M]): M[B] =
      fu.contramap(canBuild(m1, m2))((b: B) => { val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18) = f(b); new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(a1, a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14), a15), a16), a17), a18) })

    def apply[B](f1: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) => B, f2: B => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18))(implicit fu: Invariant[M]): M[B] =
      fu.imap[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11 ~ A12 ~ A13 ~ A14 ~ A15 ~ A16 ~ A17 ~ A18, B](
        canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 ~ a7 ~ a8 ~ a9 ~ a10 ~ a11 ~ a12 ~ a13 ~ a14 ~ a15 ~ a16 ~ a17 ~ a18 => f1(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18) })(
        (b: B) => { val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18) = f2(b); new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(a1, a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14), a15), a16), a17), a18) }
      )
  }

  class CanBuild19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](m1: M[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11 ~ A12 ~ A13 ~ A14 ~ A15 ~ A16 ~ A17 ~ A18], m2: M[A19]) {

    def ~[A20](m3: M[A20]) = new CanBuild20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](canBuild(m1, m2), m3)

    def apply[B](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) => B)(implicit fu: Functor[M]): M[B] =
      fu.map[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11 ~ A12 ~ A13 ~ A14 ~ A15 ~ A16 ~ A17 ~ A18 ~ A19, B](canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 ~ a7 ~ a8 ~ a9 ~ a10 ~ a11 ~ a12 ~ a13 ~ a14 ~ a15 ~ a16 ~ a17 ~ a18 ~ a19 => f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19) })

    def apply[B](f: B => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19))(implicit fu: Contravariant[M]): M[B] =
      fu.contramap(canBuild(m1, m2))((b: B) => { val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19) = f(b); new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(a1, a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14), a15), a16), a17), a18), a19) })

    def apply[B](f1: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) => B, f2: B => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19))(implicit fu: Invariant[M]): M[B] =
      fu.imap[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11 ~ A12 ~ A13 ~ A14 ~ A15 ~ A16 ~ A17 ~ A18 ~ A19, B](
        canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 ~ a7 ~ a8 ~ a9 ~ a10 ~ a11 ~ a12 ~ a13 ~ a14 ~ a15 ~ a16 ~ a17 ~ a18 ~ a19 => f1(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19) })(
        (b: B) => { val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19) = f2(b); new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(a1, a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14), a15), a16), a17), a18), a19) }
      )
  }

  class CanBuild20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](m1: M[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11 ~ A12 ~ A13 ~ A14 ~ A15 ~ A16 ~ A17 ~ A18 ~ A19], m2: M[A20]) {

    def ~[A21](m3: M[A21]) = new CanBuild21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](canBuild(m1, m2), m3)

    def apply[B](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) => B)(implicit fu: Functor[M]): M[B] =
      fu.map[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11 ~ A12 ~ A13 ~ A14 ~ A15 ~ A16 ~ A17 ~ A18 ~ A19 ~ A20, B](canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 ~ a7 ~ a8 ~ a9 ~ a10 ~ a11 ~ a12 ~ a13 ~ a14 ~ a15 ~ a16 ~ a17 ~ a18 ~ a19 ~ a20 => f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20) })

    def apply[B](f: B => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20))(implicit fu: Contravariant[M]): M[B] =
      fu.contramap(canBuild(m1, m2))((b: B) => { val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20) = f(b); new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(a1, a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14), a15), a16), a17), a18), a19), a20) })

    def apply[B](f1: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) => B, f2: B => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20))(implicit fu: Invariant[M]): M[B] =
      fu.imap[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11 ~ A12 ~ A13 ~ A14 ~ A15 ~ A16 ~ A17 ~ A18 ~ A19 ~ A20, B](
        canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 ~ a7 ~ a8 ~ a9 ~ a10 ~ a11 ~ a12 ~ a13 ~ a14 ~ a15 ~ a16 ~ a17 ~ a18 ~ a19 ~ a20 => f1(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20) })(
        (b: B) => { val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20) = f2(b); new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(a1, a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14), a15), a16), a17), a18), a19), a20) }
      )
  }

  class CanBuild21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](m1: M[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11 ~ A12 ~ A13 ~ A14 ~ A15 ~ A16 ~ A17 ~ A18 ~ A19 ~ A20], m2: M[A21]) {
    def ~[A22](m3: M[A22]) = new CanBuild22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](canBuild(m1, m2), m3)

    def apply[B](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) => B)(implicit fu: Functor[M]): M[B] =
      fu.map[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11 ~ A12 ~ A13 ~ A14 ~ A15 ~ A16 ~ A17 ~ A18 ~ A19 ~ A20 ~ A21, B](canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 ~ a7 ~ a8 ~ a9 ~ a10 ~ a11 ~ a12 ~ a13 ~ a14 ~ a15 ~ a16 ~ a17 ~ a18 ~ a19 ~ a20 ~ a21 => f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21) })

    def apply[B](f: B => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21))(implicit fu: Contravariant[M]): M[B] =
      fu.contramap(canBuild(m1, m2))((b: B) => { val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21) = f(b); new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(a1, a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14), a15), a16), a17), a18), a19), a20), a21) })

    def apply[B](f1: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) => B, f2: B => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21))(implicit fu: Invariant[M]): M[B] =
      fu.imap[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11 ~ A12 ~ A13 ~ A14 ~ A15 ~ A16 ~ A17 ~ A18 ~ A19 ~ A20 ~ A21, B](
        canBuild(m1, m2))({ case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 ~ a7 ~ a8 ~ a9 ~ a10 ~ a11 ~ a12 ~ a13 ~ a14 ~ a15 ~ a16 ~ a17 ~ a18 ~ a19 ~ a20 ~ a21 => f1(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21) })(
        (b: B) => { val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21) = f2(b); new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(new ~(a1, a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14), a15), a16), a17), a18), a19), a20), a21) }
      )
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
  }
}
