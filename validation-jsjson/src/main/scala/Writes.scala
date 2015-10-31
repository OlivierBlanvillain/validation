package jto.validation
package jsjson

import cats.Monoid
import scala.scalajs.js

trait DefaultMonoids {
  implicit def jsonMonoid = new Monoid[js.Dictionary[js.Dynamic]] {
    // TODO: Should this be a deepMerge?
    def combine(a1: js.Dictionary[js.Dynamic], a2: js.Dictionary[js.Dynamic]) = js.Dictionary((a1 ++ a2).toSeq: _*)
    def empty = js.Dictionary[js.Dynamic]()
  }
}

object Writes extends DefaultWrites with DefaultMonoids with GenericWrites[js.Dynamic] {
  private def writeObj(j: js.Dynamic, n: PathNode): js.Dynamic = n match {
    case IdxPathNode(_) => js.Array(j).asInstanceOf[js.Dynamic]
    case KeyPathNode(key) => js.Dynamic.literal(key -> j)
  }

  implicit val validationErrorW = Write[ValidationError, js.Dynamic] { err =>
    js.Dynamic.literal(
      "msg" -> err.message,
      "args" -> err.args.foldLeft(js.Array(js.Array[Object]())) { (arr, arg) =>
        js.Array(arr :+ arg.toString)
      })
  }

  implicit def errorsW(implicit wErrs: WriteLike[Seq[ValidationError], js.Dynamic]) =
    Write[(Path, Seq[ValidationError]), js.Dictionary[js.Dynamic]] {
      case (p, errs) =>
        js.Dictionary[js.Dynamic](p.toString -> wErrs.writes(errs))
    }

  implicit def failureW(implicit w: WriteLike[(Path, Seq[ValidationError]), js.Dictionary[js.Dynamic]]) =
    Write[Invalid[Seq[(Path, Seq[ValidationError])]], js.Dictionary[js.Dynamic]] {
      case Invalid(errs) =>
        errs.map(w.writes).reduce(jsonMonoid.combine)
    }

  implicit val stringW = Write[String, js.Dynamic](_.asInstanceOf[js.Dynamic])

  implicit val intW = Write[Int, js.Dynamic](_.asInstanceOf[js.Dynamic])
  implicit val shortW = Write[Short, js.Dynamic](_.asInstanceOf[js.Dynamic])
  implicit val longW = Write[Long, js.Dynamic](_.asInstanceOf[js.Dynamic])
  implicit val floatW = Write[Float, js.Dynamic](_.asInstanceOf[js.Dynamic])
  implicit val doubleW = Write[Double, js.Dynamic](_.asInstanceOf[js.Dynamic])
  implicit val bigDecimalW = Write[BigDecimal, js.Dynamic](_.toString.asInstanceOf[js.Dynamic])

  implicit def booleanW = Write[Boolean, js.Dynamic](_.asInstanceOf[js.Dynamic])

  implicit def seqToJsArray[I](implicit w: WriteLike[I, js.Dynamic]): Write[Seq[I], js.Dynamic] =
    Write(ss => js.Array(ss.map(w.writes _): _*).asInstanceOf[js.Dynamic])

  def optionW[I, J](r: => WriteLike[I, J])(implicit w: Path => WriteLike[J, js.Dictionary[js.Dynamic]]): Path => Write[Option[I], js.Dictionary[js.Dynamic]] =
    super.optionW[I, J, js.Dictionary[js.Dynamic]](r, js.Dictionary[js.Dynamic]())

  implicit def optionW[I](implicit w: Path => WriteLike[I, js.Dictionary[js.Dynamic]]): Path => Write[Option[I], js.Dictionary[js.Dynamic]] =
    optionW(Write.zero[I])

  implicit def mapW[I](implicit w: WriteLike[I, js.Dynamic]) = Write[Map[String, I], js.Dictionary[js.Dynamic]] { m =>
    js.Dictionary[js.Dynamic](m.mapValues(w.writes).toSeq: _*)
  }

  implicit def writeJson[I](path: Path)(implicit w: WriteLike[I, js.Dynamic]): Write[I, js.Dictionary[js.Dynamic]] = Write { i =>
    path match {
      case Path(KeyPathNode(x) :: _) \: _ =>
        val ps = path.path.reverse
        val h = ps.head
        val o = writeObj(w.writes(i), h)
        ps.tail.foldLeft(o)(writeObj).asInstanceOf[js.Dictionary[js.Dynamic]]
      case Path(Nil) =>
        w.writes(i).asInstanceOf[js.Dictionary[js.Dynamic]]
      case _ => throw new RuntimeException(s"path $path is not a path of JsObject") // XXX: should be a compile time error
    }
  }
}
