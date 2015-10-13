package jto.validation
package jsjson

import cats.Monoid
import scala.scalajs.js

trait DefaultMonoids {
  implicit def jsonMonoid = new Monoid[js.Dictionary[js.Any]] {
    // TODO: Should this be a deepMerge?
    def combine(a1: js.Dictionary[js.Any], a2: js.Dictionary[js.Any]) = js.Dictionary[js.Any]()
    def empty = js.Dictionary[js.Any]()
  }
}

object Writes extends DefaultWrites with DefaultMonoids with GenericWrites[js.Any] {
  private def writeObj(j: js.Any, n: PathNode) = n match {
    case IdxPathNode(_) => js.Array(j)
    case KeyPathNode(key) => js.Dictionary[js.Any](key -> j)
  }

  implicit val validationErrorW = Write[ValidationError, js.Any] { err =>
    js.Dictionary[js.Any](
      "msg" -> err.message,
      "args" -> err.args.foldLeft(js.Array(js.Array[Object]())) { (arr, arg) =>
        js.Array(arr :+ arg.toString)
      })
  }

  implicit def errorsW(implicit wErrs: WriteLike[Seq[ValidationError], js.Any]) =
    Write[(Path, Seq[ValidationError]), js.Dictionary[js.Any]] {
      case (p, errs) =>
        js.Dictionary[js.Any](p.toString -> wErrs.writes(errs))
    }

  implicit def failureW(implicit w: WriteLike[(Path, Seq[ValidationError]), js.Dictionary[js.Any]]) =
    Write[Invalid[Seq[(Path, Seq[ValidationError])]], js.Dictionary[js.Any]] {
      case Invalid(errs) =>
        errs.map(w.writes).reduce(jsonMonoid.combine)
    }

  implicit val stringW = Write[String, js.Any](identity)

  implicit val intW = Write[Int, js.Any](identity)
  implicit val shortW = Write[Short, js.Any](identity)
  implicit val longW = Write[Long, js.Any](identity)
  implicit val floatW = Write[Float, js.Any](identity)
  implicit val doubleW = Write[Double, js.Any](identity)
  implicit val bigDecimalW = Write[BigDecimal, js.Any](_.toString)

  implicit def booleanW = Write[Boolean, js.Any](identity)

  implicit def seqToJsArray[I](implicit w: WriteLike[I, js.Any]): Write[Seq[I], js.Any] =
    Write(ss => js.Array(ss.map(w.writes _)))

  def optionW[I, J](r: => WriteLike[I, J])(implicit w: Path => WriteLike[J, js.Dictionary[js.Any]]): Path => Write[Option[I], js.Dictionary[js.Any]] =
    super.optionW[I, J, js.Dictionary[js.Any]](r, js.Dictionary[js.Any]())

  implicit def optionW[I](implicit w: Path => WriteLike[I, js.Dictionary[js.Any]]): Path => Write[Option[I], js.Dictionary[js.Any]] =
    optionW(Write.zero[I])

  implicit def mapW[I](implicit w: WriteLike[I, js.Any]) = Write[Map[String, I], js.Dictionary[js.Any]] { m =>
    js.Dictionary[js.Any](m.mapValues(w.writes).toSeq: _*)
  }

  implicit def writeJson[I](path: Path)(implicit w: WriteLike[I, js.Any]): Write[I, js.Dictionary[js.Any]] = Write { i =>
    path match {
      case Path(KeyPathNode(x) :: _) \: _ =>
        val ps = path.path.reverse
        val h = ps.head
        val o = writeObj(w.writes(i), h)
        ps.tail.foldLeft(o)(writeObj).asInstanceOf[js.Dictionary[js.Any]]
      case Path(Nil) =>
        w.writes(i).asInstanceOf[js.Dictionary[js.Any]]
      case _ => throw new RuntimeException(s"path $path is not a path of JsObject") // XXX: should be a compile time error
    }
  }
}
