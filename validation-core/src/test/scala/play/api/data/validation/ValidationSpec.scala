import jto.validation._;

import org.specs2.mutable._

object ValidatedSpec extends Specification {

  "Validated" should {

    val success = Valid[String, Int](5)
    val failure = Invalid[String, Int]("err" :: Nil)

    "be a Functor" in {
      // identity
      success.map(identity) must equalTo(success)
      failure.map(identity) must equalTo(failure)
      // composition
      val p = (_: Int) + 2
      val q = (_: Int) * 3
      success.map(p compose q) must equalTo(success.map(q).map(p))
      failure.map(p compose q) must equalTo(failure.map(q).map(p))

      success.map(_ + 2) must equalTo(Valid[String, Int](7))
      failure.map(_ + 2) must equalTo(failure)
    }

    "be foldable" in {
      success.fold(
        err => "err",
        identity
      ) must equalTo(5)

      failure.fold(
        err => "err",
        identity
      ) must equalTo("err")
    }

    "have an Applicative" in {
      val app = implicitly[cats.Applicative[({type f[A] = Validated[String, A]})#f]]

      val u = Valid[String, Int => Int](_ + 2)
      val v = Valid[String, Int => Int](_ * 3)
      val w = Valid[String, Int](5)

      app.ap(app.pure(5))(app.pure((_: Int) + 2)) must equalTo(app.pure(7))

      // identity
      app.ap(success)(app.pure[Int => Int](identity _)) must equalTo(success)
      app.ap(failure)(app.pure[Int => Int](identity _)) must equalTo(failure)

      // composition
      val p = app.pure((f: Int => Int) => f compose (_: Int => Int))
      app.ap(w)(app.ap(v)(app.ap(u)(p))) must equalTo(
        app.ap(app.ap(w)(v))(u))

      // homomorphism
      val f = (_: Int) + 2
      val x = 5
      app.ap(app.pure(x))(app.pure(f)) must equalTo(app.pure(f(x)))

      // interchange
      app.ap(app.pure(x))(u) must equalTo(
        app.ap(u)(app.pure((f: Int => Int) => f(x))))
    }

    "implement filter" in {
      success.filter((_: Int) == 5) must equalTo(success)
      Valid(7).filter("err")((_: Int) == 5) must equalTo(failure)
      failure.filter((_: Int) == 5) must equalTo(failure)
    }

    "support for-comprehension" in {
      (for(x <- success) yield x + 2) must equalTo(Valid(7))
      (for(x <- failure) yield x + 2) must equalTo(failure)
      (for(x <- success if x == 5) yield x + 2) must equalTo(Valid(7))
      (for(x <- success if x == 7) yield x + 2) must equalTo(Invalid(Nil))
      (for(x <- failure if x == 5) yield x + 2) must equalTo(failure)
    }

    "have recovery methods" in {
      success.recover {
        case _ => 42
      } must equalTo(success)

      failure.recover {
        case Invalid("err" :: Nil) => 42
      } must equalTo(Valid(42))

      failure.recover {
        case Invalid(Nil) => 42
      } must equalTo(failure)

      success.recoverTotal {
        case _ => 42
      } must equalTo(5)

      failure.recoverTotal { _ => 42 } must equalTo(42)

      success.getOrElse(42) must equalTo(5)
      failure.getOrElse(42) must equalTo(42)

      success.orElse(Valid(42)) must equalTo(success)
      failure.getOrElse(Valid(42)) must equalTo(Valid(42))
    }

    "be easily convertible to scala standars API types" in {
      success.asOpt must equalTo(Some(5))
      failure.asOpt must equalTo(None)

      success.asEither must equalTo(Right(5))
      failure.asEither must equalTo(Left("err" :: Nil))
    }

    "sequence" in {
      val f1: Validated[String, String] = Invalid(Seq("err1"))
      val f2: Validated[String, String] = Invalid(Seq("err2"))
      val s1: Validated[String, String] = Valid("1")
      val s2: Validated[String, String] = Valid("2")

      Validated.sequence(Seq(s1, s2)) must equalTo(Valid(Seq("1", "2")))
      Validated.sequence(Seq(f1, f2)) must equalTo(Invalid(Seq("err1", "err2")))
    }

  }
}
