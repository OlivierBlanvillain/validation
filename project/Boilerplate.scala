import sbt._

/**
 * Copied, with some modifications, from https://github.com/milessabin/shapeless/blob/master/project/Boilerplate.scala
 *
 * Generate a range of boilerplate classes, those offering alternatives with 0-22 params
 * and would be tedious to craft by hand
 *
 * @author Miles Sabin
 */

object Boilerplate {
  import scala.StringContext._

  implicit class BlockHelper(val sc: StringContext) extends AnyVal {
    def block(args: Any*): String = {
      val interpolated = sc.standardInterpolator(treatEscapes, args)
      val rawLines = interpolated split '\n'
      val trimmedLines = rawLines map { _ dropWhile (_.isWhitespace) }
      trimmedLines mkString "\n"
    }
  }

  val header = "// Auto-generated boilerplate"

  val minArity = 2
  val maxArity = 22

  val templates: Seq[Template] = List(
    FFFFSyntax,
    RRRRSyntax,
    WWWWSyntax
  )

  /** Returns a seq of the generated files. As a side-effect, it actually generates them... */
  def gen(dir: File) =
    for(template <- templates) yield {
      val tgtFile = template.filename(dir)
      IO.write(tgtFile, template.body)
      tgtFile
    }

  class TemplateVals(val arity: Int) {
    val synTypes       = (0 until arity) map (n => s"A$n")
    val synVals        = (0 until arity) map (n => s"a$n")
    val synTypedVals   = (synVals zip synTypes) map { case (v,t) => v + ": " + t}
    val `A..N`         = synTypes.mkString(", ")
    val `a..n`         = synVals.mkString(", ")
    val `_.._`         = Seq.fill(arity)("_").mkString(", ")
    val `(A..N)`       = if (arity == 1) "Tuple1[A]" else synTypes.mkString("(", ", ", ")")
    val `(_.._)`       = if (arity == 1) "Tuple1[_]" else Seq.fill(arity)("_").mkString("(", ", ", ")")
    val `(a..n)`       = if (arity == 1) "Tuple1(a)" else synVals.mkString("(", ", ", ")")
    val `a:A..n:N`     = synTypedVals mkString ", "
    val `a~n`          = synVals.mkString(" ~ ")
    val `A~N`          = synTypes.mkString(" ~ ")
    val `A~N-1`        = (0 until arity - 1).map(n => s"A$n").mkString(" ~ ")
    val `a._1..a._N`   = (1 to arity) map (n => s"a._$n") mkString ", "
    val `new ~(.., n)` = synVals.reduce[String] { case (acc, el) => s"new ~($acc, $el)" }
  }

  trait Template {
    def filename(root: File): File
    def content(tv: TemplateVals): String
    def range = minArity to maxArity
    def body: String = {
      val headerLines = header split '\n'
      val rawContents = range map { n => content(new TemplateVals(n)) split '\n' filterNot (_.isEmpty) }
      val preBody = rawContents.head takeWhile (_ startsWith "|") map (_.tail)
      val instances = rawContents flatMap {_ filter (_ startsWith "-") map (_.tail) }
      val postBody = rawContents.head dropWhile (_ startsWith "|") dropWhile (_ startsWith "-") map (_.tail)
      (headerLines ++ preBody ++ instances ++ postBody) mkString "\n"
    }
  }


  /*
    Blocks in the templates below use a custom interpolator, combined with post-processing to produce the body

      - The contents of the `header` val is output first

      - Then the first block of lines beginning with '|'

      - Then the block of lines beginning with '-' is replicated once for each arity,
        with the `templateVals` already pre-populated with relevant relevant vals for that arity

      - Then the last block of lines prefixed with '|'

    The block otherwise behaves as a standard interpolated string with regards to variable substitution.
  */

  object FFFFSyntax extends Template {
    def filename(root: File) = root /  "jto" / "validation" / "FFFFSyntax.scala"

    def content(tv: TemplateVals) = {
      import tv._

      val next = if (arity >= maxArity) "" else
        s"def ~[A$arity](m3: M[A$arity]) = new FFFFSyntax${arity+1}[${`A..N`}, A$arity](combine(m1, m2), m3)"

      block"""
        |package jto.validation
        |
        |import cats.functor.Invariant
        |
        |case class ~[A, B](_1: A, _2: B)
        |
        |trait FFFFSyntaxCombine[M[_]] {
        |  def apply[A, B](ma: M[A], mb: M[B]): M[A ~ B]
        |}
        |
        |class FFFFSyntaxObs[M[_], A](ma: M[A])(implicit fcb: FFFFSyntaxCombine[M]) {
        |  def ~[B](mb: M[B]): FFFFSyntax[M]#FFFFSyntax2[A, B] = {
        |    val b = new FFFFSyntax(fcb)
        |    new b.FFFFSyntax2[A, B](ma, mb)
        |  }
        |}
        |
        |class FFFFSyntax[M[_]](combine: FFFFSyntaxCombine[M]) {
        |
        -  class FFFFSyntax$arity[${`A..N`}](m1: M[${`A~N-1`}], m2: M[A${arity-1}]) {
        -    $next
        -
        -    def apply[B](f1: (${`A..N`}) => B, f2: B => Option[(${`A..N`})])(implicit fu: Invariant[M]): M[B] =
        -      fu.imap[${`A~N`}, B](
        -        combine(m1, m2))({ case ${`a~n`} => f1(${`a..n`}) })(
        -        (b: B) => { val (${`a..n`}) = f2(b).get; ${`new ~(.., n)`} }
        -      )
        -
        -    def tupled(implicit fu: Invariant[M]): M[(${`A..N`})] =
        -      apply[(${`A..N`})]({ (${`a:A..n:N`}) => (${`a..n`}) }, { (a: (${`A..N`})) => Some((${`a._1..a._N`})) })
        -  }
        -
        |}
      """
    }
  }

  object RRRRSyntax extends Template {
    def filename(root: File) = root /  "jto" / "validation" / "RRRRSyntax.scala"

    def content(tv: TemplateVals) = {
      import tv._

      val next = if (arity >= maxArity) "" else
        s"def ~[A$arity](m3: M[A$arity]) = new RRRRSyntax${arity+1}[${`A..N`}, A$arity](combine(m1, m2), m3)"

      block"""
        |package jto.validation
        |
        |import cats.Functor
        |
        |trait RRRRSyntaxCombine[M[_]] {
        |  def apply[A, B](ma: M[A], mb: M[B]): M[A ~ B]
        |}
        |
        |class RRRRSyntaxObs[M[_], A](ma: M[A])(implicit fcb: RRRRSyntaxCombine[M]) {
        |  def ~[B](mb: M[B]): RRRRSyntax[M]#RRRRSyntax2[A, B] = {
        |    val b = new RRRRSyntax(fcb)
        |    new b.RRRRSyntax2[A, B](ma, mb)
        |  }
        |}
        |
        |class RRRRSyntax[M[_]](combine: RRRRSyntaxCombine[M]) {
        |
        -  class RRRRSyntax${arity}[${`A..N`}](m1: M[${`A~N-1`}], m2: M[A${arity-1}]) {
        -    $next
        -
        -    def apply[B](f: (${`A..N`}) => B)(implicit fu: Functor[M]): M[B] =
        -      fu.map[${`A~N`}, B](combine(m1, m2))({ case ${`a~n`} => f(${`a..n`}) })
        -
        -    def tupled(implicit fu: Functor[M]): M[(${`A..N`})] =
        -      apply[(${`A..N`})]({ (${`a:A..n:N`}) => (${`a..n`}) })
        -  }
        -
        |}
      """
    }
  }

  object WWWWSyntax extends Template {
    def filename(root: File) = root /  "jto" / "validation" / "WWWWSyntax.scala"

    def content(tv: TemplateVals) = {
      import tv._

      val next = if (arity >= maxArity) "" else
        s"def ~[A$arity](m3: M[A$arity]) = new WWWSyntax${arity+1}[${`A..N`}, A$arity](combine(m1, m2), m3)"

      block"""
        |package jto.validation
        |
        |import cats.functor.Contravariant
        |
        |trait WWWWSyntaxCombine[M[_]] {
        |  def apply[A, B](ma: M[A], mb: M[B]): M[A ~ B]
        |}
        |
        |class WWWWSyntaxObs[M[_], A](ma: M[A])(implicit fcb: WWWWSyntaxCombine[M]) {
        |  def ~[B](mb: M[B]): WWWWSyntax[M]#WWWSyntax2[A, B] = {
        |    val b = new WWWWSyntax(fcb)
        |    new b.WWWSyntax2[A, B](ma, mb)
        |  }
        |}
        |
        |class WWWWSyntax[M[_]](combine: WWWWSyntaxCombine[M]) {
        |
        -  class WWWSyntax${arity}[${`A..N`}](m1: M[${`A~N-1`}], m2: M[A${arity-1}]) {
        -    $next
        -
        -    def apply[B](f: B => Option[(${`A..N`})])(implicit fu: Contravariant[M]): M[B] =
        -      fu.contramap(combine(m1, m2))((b: B) => { val (${`a..n`}) = f(b).get; ${`new ~(.., n)`} })

        -    def tupled(implicit fu: Contravariant[M]): M[(${`A..N`})] =
        -      apply[(${`A..N`})]({ (a: (${`A..N`})) => Some((${`a._1..a._N`})) })
        -  }
        -
        |}
      """
    }
  }

}
