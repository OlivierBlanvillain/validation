package jto.validation

trait From[I] {
  def apply[O](f: Reader[I] => RuleLike[I, O]): Rule[I, O] =
    Rule.toRule(f(Reader[I]()))
}
object From {

  /**
    * {{{
    *   val r = From[UrlFormEncoded]{ __ =>
    *     ((__ \ "firstname").read(notEmpty) ~
    *      (__ \ "age").read(min(1)).tupled
    *   }
    *   r.validate(valid) == Valid("Julien" -> 28)
    * }}}
    */
  def apply[I] = new From[I] {}

  /**
    * Validate type `I` as an  using the implicit `Write` w
    * {{{
    *   val m = Map(
    *     "name" -> Seq("bob"),
    *     "friend.name" -> Seq("bobby"))
    *   From[UrlFormEncoded, Person](m) == Valid(Person(List("bob", "bobby")))
    * }}}
    */
  def apply[I, O](i: I)(implicit r: RuleLike[I, O]) =
    r.validate(i)
}

trait To[I] {
  def apply[O](f: Writer[I] => WriteLike[O, I]): Write[O, I] =
    Write.toWrite(f(Writer[I]()))
}
object To {

  /**
    * {{{
    *   val w = To[UrlFormEncoded] { __ =>
    *     ((__ \ "email").write[Option[String]] ~
    *      (__ \ "phone").write[String]).tupled
    *   }
    *
    *   val v =  Some("jto@foobar.com") -> "01.23.45.67.89"
    *
    *    w.writes(v) == Map(
    *      "email" -> Seq("jto@foobar.com"),
    *      "phone" -> Seq("01.23.45.67.89"))
    * }}}
    */
  def apply[I] = new To[I] {}

  /**
    * "Serialize" type `O` to type `I` using the implicit `Write` w
    * {{{
    *   To[Person2, UrlFormEncoded](Person(List("bob", "bobby"))) ==
    *      Map(
    *      "name" -> Seq("bob"),
    *      "friend.name" -> Seq("bobby"))
    * }}}
    */
  def apply[O, I](o: O)(implicit w: WriteLike[O, I]) =
    w.writes(o)
}

case class Reader[I](path: Path = Path(Nil)) {

  /**
    * When applied, the rule will lookup for data at the given path, and apply the `sub` Rule on it
    * {{{
    *   val json = Json.parse("""{
    *      "informations": {
    *        "label": "test"
    *      }
    *   }""")
    *   val infoValidated = From[JsValue]{ __ => (__ \ "label").read(nonEmptyText) }
    *   val v = From[JsValue]{ __ => (__ \ "informations").read(infoValidated)) }
    *   v.validate(json) == Valid("test")
    * }}}
    * @param sub the constraint to apply on the subdata
    * @param l a lookup function. This function finds data in a structure of type I, and coerce it to type O
    * @return A Rule validating the existence and validity of data at `path`
    */
  def read[J, O](sub: => RuleLike[J, O])(implicit r: Path => RuleLike[I, J]): Rule[I, O] =
    Rule.toRule(r(path)).andThen(path)(sub)

  /**
    * Try to convert the data at `Path` to type `O`
    * {{{
    *   val json = Json.parse("""{
    *      "informations": {
    *        "label": "test"
    *      }
    *   }""")
    *   implicit val infoValidated = From[JsValue]{ __ => (__ \ "label").read[String] }
    *   val v = From[JsValue]{ __ => (__ \ "informations").read[Informations]) }
    *   v.validate(json) == Valid("test")
    * }}}
    * @param r a lookup function. This function finds data in a structure of type I, and coerce it to type O
    * @return A Rule validating the existence and validity of data at `path`.
    */
  def read[O](implicit r: Path => RuleLike[I, O]): Rule[I, O] =
    Rule(i => read(Rule.zero[O])(r).validate(i)) // Makes it lazy evaluated. Allows recursive writes

  def \(key: String): Reader[I] = Reader(path \ key)
  def \(idx: Int): Reader[I] = Reader(path \ idx)
  def \(child: PathNode): Reader[I] = Reader(path \ child)
}

case class Writer[I](path: Path = Path(Nil)) {

  /**
    * Create a Write that convert data to type `I`, and put it at Path `path`
    * {{{
    *   val w = To[JsObject] { __ =>
    *      (__ \ "informations").write[Seq[String]])
    *   }
    *   w.writes(Seq("foo", "bar")) == Json.obj("informations" -> Seq("foo", "bar"))
    * }}}
    * @note This method works fine with recursive writes
    */
  def write[O](implicit w: Path => WriteLike[O, I]): Write[O, I] =
    Write(x => w(path).writes(x)) // Makes it lazy evaluated. Allows recursive writes

  /**
    * Create a Write that convert data to type `I`, and put it at Path `path`
    * {{{
    *   val w = To[JsObject] { __ =>
    *      (__ \ "date").write(date("yyyy-MM-dd""))
    *   }
    *   w.writes(new Date()) == Json.obj("date" -> "2013-10-3")
    * }}}
    * @note This method works fine with recursive writes
    */
  def write[O, J](format: => WriteLike[O, J])(
      implicit w: Path => WriteLike[J, I]): Write[O, I] =
    Write.toWrite(w(path)).contramap(x => format.writes(x))

  def \(key: String): Writer[I] = Writer(path \ key)
  def \(idx: Int): Writer[I] = Writer(path \ idx)
  def \(child: PathNode): Writer[I] = Writer(path \ child)
}


object Build {
  def apply[F1[_]: At] = new Build1[F1] {}
  def apply[F1[_]: At, F2[_]: At] = new Build2[F1, F2] {}

  def apply[F1[_], F2[_], O](as: As2[F1, F2] => F1[O] with F2[O])
    (implicit a1: At[F1], a2: At[F2], M: Mixer[F1, F2]): F1[O] with F2[O] =
      as(As2[F1, F2]())
  // def apply[F1[_]: At, F2[_]: At, F3[_]: At] = new Build3[F1, F2, F3] {}
  // ...
}


// ------------------------------------------------------------------------------------------
trait Build1[F1[_]] {
  def using[O](as: As1[F1] => F1[O])(implicit a1: At[F1]): F1[O] =
    as(As1[F1]())
}

case class As1[F1[_]: At](path: Path = Path(Nil)) {
  def as[O](m1: F1[O]): F1[O]= At[F1].at(path, m1)
  def as[O](implicit m1: Path => F1[O]): F1[O]= m1(path)
  def \(key: String): As1[F1] = As1(path \ key)
  def \(idx: Int): As1[F1] = As1(path \ idx)
  def \(child: PathNode): As1[F1] = As1(path \ child)
}

// ------------------------------------------------------------------------------------------
trait Build2[F1[_], F2[_]] {
  def using[O](as: As2[F1, F2] => F1[O] with F2[O])
    (implicit a1: At[F1], a2: At[F2], M: Mixer[F1, F2]): F1[O] with F2[O] =
      as(As2[F1, F2]())
}

case class As2[F1[_]: At, F2[_]: At](path: Path = Path(Nil))(implicit M: Mixer[F1, F2]) {
  // TODO: Rename as
  def format[O](m1: F1[O], m2: F2[O]): F1[O] with F2[O] =
    M.mix(At[F1].at(path, m1), At[F2].at(path, m2))

  // TODO: Rename as
  def format[O](implicit m1: Path => F1[O], m2: Path => F2[O]): F1[O] with F2[O] =
    M.mix(m1(path), m2(path))


  // def format[O]
  //   (implicit
  //     r: Path => RuleLike[IR, O],
  //     w: Path => WriteLike[O, IW]
  //   ): Format[IR, IW, O] =
  //     Format[IR, IW, O](Reader(path).read(Rule.zero[O]), Writer(path).write(Write.zero[O]))

  // def format[JJ, J, O](subR: => RuleLike[J, O], subW: => WriteLike[O, JJ])
  //   (implicit
  //     r: Path => RuleLike[IR, J],
  //     w: Path => WriteLike[JJ, IW]
  //   ): Format[IR, IW, O] =
  //     Format[IR, IW, O](Reader(path).read(subR), Writer(path).write(subW))

  def \(key: String): As2[F1, F2] = As2(path \ key)
  def \(idx: Int): As2[F1, F2] = As2(path \ idx)
  def \(child: PathNode): As2[F1, F2] = As2(path \ child)
}

// ------------------------------------------------------------------------------------------
// trait Build3[F1[_], F2[_], F3[_]] {}
// case class As3[F1[_]: At, F2[_]: At, F3[_]: At](path: Path)(implicit M: Mixer3[F1, F2, F3]) {}
// ...
