package jto.validation
package jsjson

import scala.scalajs.js
import scala.util.Try

object Rules extends DefaultRules[js.Dynamic] {
  private def jsonAs[T](f: PartialFunction[js.Dynamic, Validated[Seq[ValidationError], T]])(msg: String, args: Any*) =
    Rule.fromMapping[js.Dynamic, T](
      f.orElse {
        case j => Invalid(Seq(ValidationError(msg, args: _*)))
      })

  implicit def stringR = jsonAs[String] {
    case v if js.typeOf(v) == "string" => Valid(v.asInstanceOf[String])
  }("error.invalid", "String")

  implicit def booleanR = jsonAs[Boolean] {
    case nope if {println(s"nope: ${nope.toSeq}"); println(s"nope.isInstanceOf[Boolean] ${nope.isInstanceOf[Boolean]}"); false} => ???
    case v if v.isInstanceOf[Boolean] => Valid(v.asInstanceOf[Boolean])
  }("error.invalid", "Boolean")

  // Note: Mappings of JsNumber to Number are validating that the JsNumber is indeed valid
  // in the target type. i.e: JsNumber(4.5) is not considered parseable as an Int.
  implicit def intR = jsonAs[Int] {
    case v if js.typeOf(v) == "number" && Try(v.toString.toInt).isSuccess => Valid(v.asInstanceOf[Int])
  }("error.number", "Int")

  implicit def shortR = jsonAs[Short] {
    case v if js.typeOf(v) == "number" && Try(v.toString.toShort).isSuccess => Valid(v.asInstanceOf[Short])
  }("error.number", "Short")

  implicit def longR = jsonAs[Long] {
    // v.asInstanceOf[Long] fails with a scala.scalajs.runtime.UndefinedBehaviorError: An undefined behavior was detected: 4 is not an instance of scala.scalajs.runtime.RuntimeLong.
    case v if js.typeOf(v) == "number" && Try(v.toString.toLong).isSuccess => Valid(v.toString.toLong)
  }("error.number", "Long")

  implicit def jsObjectR = jsonAs[js.Dynamic] {
    case v => Valid(v)
  }("error.invalid", "Object")

  implicit def jsArrayR[A] = jsonAs[js.Array[A]] {
    case v if js.Array.isArray(v) => Valid(v.asInstanceOf[js.Array[A]])
  }("error.invalid", "Array")

  implicit def floatR = jsonAs[Float] {
    case v if js.typeOf(v) == "number" && Try(v.toString.toFloat).isSuccess => Valid(v.asInstanceOf[Float])
  }("error.number", "Float")

  implicit def doubleR = jsonAs[Double] {
    case v if js.typeOf(v) == "number" && Try(v.toString.toDouble).isSuccess => Valid(v.asInstanceOf[Double])
  }("error.number", "Double")

  // implicit def bigDecimal = jsonAs[BigDecimal] {
  //   case JNumber(v) => Valid(v)
  // }("error.number", "BigDecimal")

  // import java.{math => jm}
  // implicit def javaBigDecimal = jsonAs[jm.BigDecimal] {
  //   case JNumber(v) => Valid(v.bigDecimal)
  // }("error.number", "BigDecimal")

  implicit val jsNullR = jsonAs[Null] {
    case v if v == null => Valid(null)
  }("error.invalid", "null")

  implicit def ooo[O](p: Path)(implicit pick: Path => RuleLike[js.Dynamic, js.Dynamic], coerce: RuleLike[js.Dynamic, O]): Rule[js.Dynamic, Option[O]] =
    optionR(Rule.zero[O])(pick, coerce)(p)

  def optionR[J, O](r: => RuleLike[J, O], noneValues: RuleLike[js.Dynamic, js.Dynamic]*)(implicit pick: Path => RuleLike[js.Dynamic, js.Dynamic], coerce: RuleLike[js.Dynamic, J]): Path => Rule[js.Dynamic, Option[O]] =
    super.opt[J, O](r, (jsNullR.map(n => n: js.Dynamic) +: noneValues): _*)

  // implicit def mapR[O](implicit r: RuleLike[js.Dynamic, O]): Rule[js.Dynamic, Map[String, O]] =
  //   super.mapR[js.Dynamic, O](r, jsObjectR.map(_.asInstanceOf[js.Dictionary[js.Dynamic]].toSeq))

  // implicit def JsValue[O](implicit r: RuleLike[JObject, O]): Rule[js.Dynamic, O] =
  //   jsObjectR.compose(r)

  implicit def pickInJson[II <: js.Dynamic, O](p: Path)(implicit r: RuleLike[js.Dynamic, O]): Rule[II, O] = {
    def search(path: Path, json: js.Dynamic): Option[js.Dynamic] = path.path match {
      case KeyPathNode(k) :: t =>
        println("lol")
        json match {
          case v if js.typeOf(v) == "object" && !js.Array.isArray(v) =>
            v.asInstanceOf[js.Dictionary[js.Dynamic]].find(_._1 == k).flatMap(kv => search(Path(t), kv._2))
          case _ => None
        }
      
      case IdxPathNode(i) :: t =>
        json match {
          case v if js.Array.isArray(v) => v.asInstanceOf[js.Array[js.Dynamic]].lift(i).flatMap(j => search(Path(t), j))
          case _ => None
        }
      
      case Nil => Some(json)
    }

    Rule[II, js.Dynamic] { json =>
      search(p, json) match {
        case None => Invalid(Seq(Path -> Seq(ValidationError("error.required"))))
        case Some(js) => Valid(js)
      }
    }.compose(r)
  }

  // XXX: a bit of boilerplate
  private def pickInS[T](implicit r: RuleLike[Seq[js.Dynamic], T]): Rule[js.Dynamic, T] =
    jsArrayR[js.Dynamic].map(fs => Seq(fs: _*)).compose(r)
  implicit def pickSeq[O](implicit r: RuleLike[js.Dynamic, O]) = pickInS(seqR[js.Dynamic, O])
  implicit def pickSet[O](implicit r: RuleLike[js.Dynamic, O]) = pickInS(setR[js.Dynamic, O])
  implicit def pickList[O](implicit r: RuleLike[js.Dynamic, O]) = pickInS(listR[js.Dynamic, O])
  implicit def pickArray[O: scala.reflect.ClassTag](implicit r: RuleLike[js.Dynamic, O]) = pickInS(arrayR[js.Dynamic, O])
  implicit def pickTraversable[O](implicit r: RuleLike[js.Dynamic, O]) = pickInS(traversableR[js.Dynamic, O])
}
