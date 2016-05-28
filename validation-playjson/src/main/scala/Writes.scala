package jto.validation
package playjson

import play.api.libs.json.{JsValue, JsObject, Json, JsString, JsNumber, JsBoolean, JsArray}
import cats.Monoid

trait DefaultMonoids {
  // It's a lie, only Monoid[JsValue] exists.
  implicit def jsonMonoid = new Monoid[JsValue] {
    def combine(a1: JsValue, a2: JsValue): JsValue =
      a1.as[JsObject].deepMerge(a2.as[JsObject])

    def empty: JsValue = Json.obj()
  }
}

object Writes
    extends DefaultWrites
    with DefaultMonoids
    with GenericWrites[JsValue] {

  private def writeObj(j: JsValue, n: PathNode) = n match {
    case IdxPathNode(_) => Json.arr(j)
    case KeyPathNode(key) => Json.obj(key -> j)
  }

  implicit val validationErrorW = Write[ValidationError, JsValue] { err =>
    Json.obj(
      "msg" -> JsString(err.message),
      "args" -> err.args.foldLeft(Json.arr()) { (arr, arg) =>
        arr :+
        (arg match {
              case s: String => JsString(s)
              case nb: Int => JsNumber(nb)
              case nb: Short => JsNumber(nb)
              case nb: Long => JsNumber(nb)
              case nb: Double => JsNumber(nb)
              case nb: Float => JsNumber(nb)
              case b: Boolean => JsBoolean(b)
              case js: JsValue => js
              case x => JsString(x.toString)
            })
      })
  }

  implicit def errorsW(
      implicit wErrs: Write[Seq[ValidationError], JsValue]) =
    Write[(Path, Seq[ValidationError]), JsValue] {
      case (p, errs) =>
        Json.obj(p.toString -> wErrs.writes(errs))
    }

  implicit def failureW(
      implicit w: Write[(Path, Seq[ValidationError]), JsValue]) =
    Write[Invalid[Seq[(Path, Seq[ValidationError])]], JsValue] {
      case Invalid(errs) =>
        errs.map(w.writes).reduce(jsonMonoid.combine)
    }

  implicit val stringW: Write[String, JsValue] = Write(s => JsString(s))

  private def tToJs[T] =
    Write[T, JsValue]((i: T) => JsNumber(BigDecimal(i.toString)))
  implicit def javanumber[T <: java.lang.Number] = tToJs[T]

  implicit val intW = tToJs[Int]
  implicit val shortW = tToJs[Short]
  implicit val longW = tToJs[Long]
  implicit val floatW = tToJs[Float]
  implicit val doubleW = tToJs[Double]

  implicit val bigDecimalW = Write[BigDecimal, JsValue](JsNumber.apply)

  implicit def booleanW = Write[Boolean, JsValue](JsBoolean.apply)

  implicit def seqToJsArray[I](
      implicit w: Write[I, JsValue]): Write[Seq[I], JsValue] =
    Write(ss => JsArray(ss.map(w.writes _)))

  implicit def mapW[I](implicit w: Write[I, JsValue]) =
    Write[Map[String, I], JsValue] { m =>
      JsObject(m.mapValues(w.writes).toSeq)
    }

  implicit val writeAtJsValue: At[Write[?, JsValue]] = new At[Write[?, JsValue]] {
    def at[A](path: Path, w: Write[A, JsValue]): Write[A, JsValue] =
      Write {
        case None    => ??? // UG
        case Some(i) => ??? // LY
        case i =>
          path match {
            case Path(KeyPathNode(x) :: _) \: _ =>
              val ps = path.path.reverse
              val h = ps.head
              val o = writeObj(w.writes(i), h)
              ps.tail.foldLeft(o)(writeObj).asInstanceOf[JsValue]
            case _ =>
              throw new RuntimeException(s"path $path is not a path of JsValue")
          }
      }
  }
}
