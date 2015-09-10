package jto.validation

import cats.{Functor, Applicative}

/**
 * Validated[E, A] is the result of a validation, where E is the type of each error, and A is the type of the result if the validation is successful
 * The only two possible implementations are Valid[E, A](value: A), or Invalid[E, A](errors: Seq[E])
 */
sealed trait Validated[+E, +A] { self =>

  /**
   * [use case] Builds a new Validated by applying a function to the value of this validation if it's a Valid
   * {{{
   *   val f: Int => Int = _ + 2
   *   Valid(5).map(f) == Valid(7)
   *   Invalid(Seq("error")).map(f) == Invalid(Seq("error"))
   * }}}
   * @param f the function to apply if this is a `Valid`
   * @return the result of applying the function
   */
  def map[X](f: A => X): Validated[E, X] = this match {
    case Valid(v) => Valid(f(v))
    case Invalid(e) => Invalid(e)
  }

  def isValid = this match {
    case Valid(_) => true
    case Invalid(_) => false
  }

  def isInvalid = !isValid

  def viaEither[EE, AA](f: Either[Seq[E], A] => Either[Seq[EE], AA]): Validated[EE, AA] =
    f(asEither).fold(Invalid.apply, Valid.apply)

  /**
   * Applies `invalid` if this is a Invalid or `valid` if this is a Valid.
   * @param invalid the function to apply if this is a `Invalid`
   * @param valid the function to apply if this is a `Valid`
   * @return the results of applying the function
   */
  def fold[X](invalid: Seq[E] => X, valid: A => X): X = this match {
    case Valid(v) => valid(v)
    case Invalid(e) => invalid(e)
  }

  def filterNot[EE >: E](error: EE)(p: A => Boolean): Validated[EE, A] =
    viaEither { _.right.flatMap { a => if (p(a)) Left(Seq(error)) else Right(a) } }

  def filterNot(p: A => Boolean): Validated[E, A] =
    viaEither { _.right.flatMap { a => if (p(a)) Left(Nil) else Right(a) } }

  /**
   * filter Validful Validated if it does not match the predicate `p`
   * @param p the predicate to apply if this is a `Valid`
   * @return a Valid if this was a Valid and the predicate matched, a Invalid otherwise
   */
  def filter(p: A => Boolean): Validated[E, A] =
    viaEither { _.right.flatMap { a => if (p(a)) Right(a) else Left(Nil) } }

  /**
   * filter Validful Validated if it does not match the predicate `p`
   * {{{
   *   val isFive: Int => Boolean = _ == 5
   *   Valid(5).filter("Not five")(isFive) == Valid(5)
   *   Valid(7).filter("Not five")(isFive) == Invalid(Seq("Not five"))
   *   Invalid(Seq("error")).filter("Not five")(isFive) == Invalid(Seq("error"))
   * }}}
   * @param otherwise the error to return if the predicate `p` is not verified
   * @param p the predicate to apply if this is a `Valid`
   * @return a Valid if this was a Valid and the predicate matched, a Invalid otherwise
   */
  def filter[EE >: E](otherwise: EE)(p: A => Boolean): Validated[EE, A] =
    viaEither { _.right.flatMap { a => if (p(a)) Right(a) else Left(Seq(otherwise)) } }

  /**
   * Like `map`, but for partial function. If `p` us not defined for the value, it return a Invalid
   * {{{
   *   val p: PartialFunction[Int, String] = { case 5 => "High five!" }
   *   Valid(5).collect("OOoops")(p) == Valid("High five!")
   *   Valid(7).collect("OOoops")(p) == Invalid(Seq("OOoops"))
   *   Invalid(Seq("error")).collect("OOoops")(p) == Invalid(Seq("error"))
   * }}}
   * @param otherwise the error to return if the `p` is not defined
   * @param p the partial function to apply if this is a `Valid`
   * @return a Valid if this was a Valid and `p` was defined, a Invalid otherwise
   */
  def collect[EE >: E, B](otherwise: EE)(p: PartialFunction[A, B]): Validated[EE, B] = viaEither {
    _.right.flatMap {
      case t if p.isDefinedAt(t) => Right(p(t))
      case _ => Left(Seq(otherwise))
    }
  }

  /**
   * Applies the given function `f` if this is a Valid, otherwise returns Unit if this is a Invalid
   * @param f the function to apply if this is a `Valid`
   * @return Unit
   */
  def foreach(f: A => Unit): Unit = this match {
    case Valid(a) => f(a)
    case _ => ()
  }

  /**
   * Creates a non-strict filter of this Validated.
   *
   *  Note: the difference between `c filter p` and `c withFilter p` is that
   *        the former creates a new vlaidation, whereas the latter only
   *        restricts the domain of subsequent `map`, `flatMap`, `foreach`,
   *        and `withFilter` operations.
   *
   *  @param p   the predicate used to test value.
   *  @return    an object of class `WithFilter`, which supports
   *             `map`, `flatMap`, `foreach`, and `withFilter` operations.
   *             All these operations apply to the value of this Validated
   *             which satisfy the predicate `p`.
   */
  def withFilter(p: A => Boolean) = new WithFilter(p)

  final class WithFilter(p: A => Boolean) {
    def map[B](f: A => B): Validated[E, B] = self match {
      case Valid(a) =>
        if (p(a)) Valid(f(a))
        else Invalid(Nil)
      case Invalid(errs) => Invalid(errs)
    }
    def flatMap[EE >: E, B](f: A => Validated[EE, B]): Validated[EE, B] = self match {
      case Valid(a) =>
        if (p(a)) f(a)
        else Invalid(Nil)
      case Invalid(errs) => Invalid(errs)
    }
    def foreach(f: A => Unit): Unit = self match {
      case Valid(a) if p(a) => f(a)
      case _ => ()
    }
    def withFilter(q: A => Boolean) = new WithFilter(a => p(a) && q(a))
  }

  /**
   * Returns the value from this Valid or throws the exception if this is a Invalid.
   */
  def get: A

  /**
   * Returns the value from this Valid or returns `t` if this is a Invalid.
   */
  def getOrElse[AA >: A](t: => AA): AA = this match {
    case Valid(a) => a
    case Invalid(_) => t
  }

  /**
   * Returns this Validated if it is a Valid or returns `t` if it is a Invalid.
   */
  def orElse[EE >: E, AA >: A](t: => Validated[EE, AA]): Validated[EE, AA] = this match {
    case s @ Valid(_) => s
    case Invalid(_) => t
  }

  /**
   * Returns None if this is a Invalid or a Some containing the value if this is a Valid.
   */
  def asOpt: Option[A] = this match {
    case Valid(v) => Some(v)
    case Invalid(_) => None
  }

  /**
   * Returns Left containing the errors if this is a Invalid or a Right containing the value if this is a Valid.
   */
  def asEither: Either[Seq[E], A] = this match {
    case Valid(v) => Right(v)
    case Invalid(e) => Left(e)
  }

  /**
   * Applies the given  partial function `errManager` if this is a Invalid, otherwise returns this if this is a Valid.
   */
  def recover[AA >: A](errManager: PartialFunction[Invalid[E, A], AA]): Validated[E, AA] = this match {
    case Valid(v) => Valid(v)
    case e @ Invalid(_) => if (errManager isDefinedAt e) Valid(errManager(e)) else this
  }

  /**
   * Applies the given  function `errManager` if this is a Invalid, otherwise returns this if this is a Valid.
   */
  def recoverTotal[AA >: A](errManager: Invalid[E, A] => AA): AA = this match {
    case Valid(v) => v
    case e @ Invalid(_) => errManager(e)
  }

  // TODO: rename (keepAnd ?)
  def *>[EE >: E, B](o: Validated[EE, B]): Validated[EE, B] = (this, o) match {
    case (Valid(_), Valid(v)) => Valid(v)
    case (Valid(_), Invalid(e)) => Invalid(e)
    case (Invalid(e), Valid(_)) => Invalid(e)
    case (Invalid(e1), Invalid(e2)) => Invalid((e1: Seq[E]) ++ (e2: Seq[EE])) // dafuk??? why do I need to force types ?
  }

  def fail = FailProjection(this)
  def success = ValidProjection(this)
}

object Validated {

  def sequence[E, A](vs: Seq[Validated[E, A]]): Validated[E, Seq[A]] = {
    vs.foldLeft[Validated[E, Seq[A]]](Valid(Nil)) {
      case (Valid(as), Valid(b)) => Valid(as ++ Seq(b))
      case (Valid(_), Invalid(e)) => Invalid(e)
      case (Invalid(e), Valid(_)) => Invalid(e)
      case (Invalid(e1), Invalid(e2)) => Invalid(e1 ++ e2)
    }
  }

  implicit def functorValidated[I] = new Functor[Validated[I, ?]] {
    def map[A, B](m: Validated[I, A])(f: A => B): Validated[I, B] = Validated.applicativeValidated[I].map(m, f)
  }

  implicit def applicativeValidated[E] = new Applicative[Validated[E, ?]] {
    def pure[A](a: A): Validated[E, A] = Valid(a)

    def map[A, B](m: Validated[E, A], f: A => B): Validated[E, B] = m.map(f)

    def ap[A, B](ma: Validated[E, A])(mf: Validated[E, A => B]): Validated[E, B] = (mf, ma) match {
      case (Valid(f), Valid(a)) => Valid(f(a))
      case (Invalid(e1), Invalid(e2)) => Invalid.merge(Invalid(e1), Invalid(e2))
      case (Invalid(e), _) => Invalid(e)
      case (_, Invalid(e)) => Invalid(e)
    }
  }

  // XXX: Helps the compiler a bit
  // implicit def cba[E] = functionalCanBuildApplicative[({ type λ[A] = Validated[E, A] })#λ]
  // implicit def validationFbo[I, O] = toFunctionalBuilderOps[({ type λ[O] = Validated[I, O] })#λ, O] _
}

final case class FailProjection[+E, +A](v: Validated[E, A]) {
  def map[F](f: Seq[E] => Seq[F]): Validated[F, A] = v match {
    case Valid(v) => Valid(v)
    case Invalid(e) => Invalid(f(e))
  }
}
final case class ValidProjection[+E, +A](v: Validated[E, A]) {
  def map[B](f: A => B): Validated[E, B] = v match {
    case Valid(v) => Valid(f(v))
    case Invalid(e) => Invalid(e)
  }
}

// Those classes should be final, but we neeed to inherit
// from them for backward compatibility of JsValid and JsError
class Valid[+E, +A](val value: A) extends Validated[E, A] {
  def get: A = value
  override def toString = s"Valid($value)"
  override def hashCode = value.hashCode
  override def equals(o: Any) = {
    if (canEqual(o)) {
      val j = o.asInstanceOf[Valid[E, A]]
      this.value == j.value
    } else
      false
  }

  def canEqual(o: Any) = o.isInstanceOf[Valid[E, A]]
}
class Invalid[+E, +A](val errors: Seq[E]) extends Validated[E, A] {
  def get: Nothing = throw new NoSuchElementException("Invalid.get")
  override def toString = s"Invalid($errors)"
  override def hashCode = errors.hashCode
  override def equals(o: Any) = {
    if (canEqual(o)) {
      val j = o.asInstanceOf[Invalid[E, A]]
      this.errors == j.errors
    } else
      false
  }
  def canEqual(o: Any) = o.isInstanceOf[Invalid[E, A]]
}

object Valid {
  def apply[E, A](v: A): Valid[E, A] = new Valid(v)
  def unapply[E, A](s: Valid[E, A]): Option[A] = Some(s.value)
}

object Invalid {
  def apply[E, A](errors: Seq[E]): Invalid[E, A] = new Invalid(errors)
  def unapply[E, A](f: Invalid[E, A]): Option[Seq[E]] = Some(f.errors)

  def merge[E, A](e1: Invalid[E, A], e2: Invalid[E, A]): Invalid[E, A] = {
    Invalid(e1.errors ++ e2.errors)
  }
}
