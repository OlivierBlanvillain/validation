package jto.validation
package jsjson

import scala.scalajs.js

object Rules extends DefaultRules[js.Any] {
  private def jsonAs[T](f: PartialFunction[js.Any, Validated[Seq[ValidationError], T]])(msg: String, args: Any*) =
    Rule.fromMapping[js.Any, T](
      f.orElse {
        case j => Invalid(Seq(ValidationError(msg, args: _*)))
      })

  // implicit def stringR = jsonAs[String] {
  //   case JString(v) => Valid(v)
  // }("error.invalid", "String")

  // implicit def booleanR = jsonAs[Boolean] {
  //   case v: Boolean => Valid(v)
  // }("error.invalid", "Boolean")

  // // Note: Mappings of JsNumber to Number are validating that the JsNumber is indeed valid
  // // in the target type. i.e: JsNumber(4.5) is not considered parseable as an Int.
  // implicit def intR = jsonAs[Int] {
  //   case JNumber(v) if v.isValidInt => Valid(v.toInt)
  // }("error.number", "Int")

  // implicit def shortR = jsonAs[Short] {
  //   case JNumber(v) if v.isValidShort => Valid(v.toShort)
  // }("error.number", "Short")

  // implicit def longR = jsonAs[Long] {
  //   case JNumber(v) if v.isValidLong => Valid(v.toLong)
  // }("error.number", "Long")

  // implicit def jsNumber = jsonAs[JNumber] {
  //   case v @ JNumber(_) => Valid(v)
  // }("error.number", "Number")

  // implicit def jsBooleanR = jsonAs[JBoolean] {
  //   case v @ JBoolean(_) => Valid(v)
  // }("error.invalid", "Boolean")

  // implicit def jsStringR = jsonAs[JString] {
  //   case v @ JString(_) => Valid(v)
  // }("error.invalid", "String")

  // implicit def jsObjectR = jsonAs[JObject] {
  //   case v @ JObject(_) => Valid(v)
  // }("error.invalid", "Object")

  // implicit def jsArrayR = jsonAs[JArray] {
  //   case v @ JArray(_) => Valid(v)
  // }("error.invalid", "Array")

  // implicit def floatR = jsonAs[Float] {
  //   case JNumber(v) if v.isDecimalFloat => Valid(v.toFloat)
  // }("error.number", "Float")

  // implicit def doubleR = jsonAs[Double] {
  //   case JNumber(v) if v.isDecimalDouble => Valid(v.toDouble)
  // }("error.number", "Double")

  // implicit def bigDecimal = jsonAs[BigDecimal] {
  //   case JNumber(v) => Valid(v)
  // }("error.number", "BigDecimal")

  // import java.{math => jm}
  // implicit def javaBigDecimal = jsonAs[jm.BigDecimal] {
  //   case JNumber(v) => Valid(v.bigDecimal)
  // }("error.number", "BigDecimal")

  // implicit val jsNullR = jsonAs[JNull.type] {
  //   case JNull => Valid(JNull)
  // }("error.invalid", "null")

  // implicit def ooo[O](p: Path)(implicit pick: Path => RuleLike[js.Any, js.Any], coerce: RuleLike[js.Any, O]): Rule[js.Any, Option[O]] =
  //   optionR(Rule.zero[O])(pick, coerce)(p)

  // def optionR[J, O](r: => RuleLike[J, O], noneValues: RuleLike[js.Any, js.Any]*)(implicit pick: Path => RuleLike[js.Any, js.Any], coerce: RuleLike[js.Any, J]): Path => Rule[js.Any, Option[O]] =
  //   super.opt[J, O](r, (jsNullR.map(n => n: js.Any) +: noneValues): _*)

  // implicit def mapR[O](implicit r: RuleLike[js.Any, O]): Rule[js.Any, Map[String, O]] =
  //   super.mapR[js.Any, O](r, jsObjectR.map { case JObject(fs) => fs.toSeq })

  // implicit def JsValue[O](implicit r: RuleLike[JObject, O]): Rule[js.Any, O] =
  //   jsObjectR.compose(r)

  // implicit def pickInJson[II <: js.Any, O](p: Path)(implicit r: RuleLike[js.Any, O]): Rule[II, O] = {

  //   def search(path: Path, json: js.Any): Option[js.Any] = path.path match {
  //     case KeyPathNode(k) :: t =>
  //       json match {
  //         case JObject(js) =>
  //           js.find(_._1 == k).flatMap(kv => search(Path(t), kv._2))
  //         case _ => None
  //       }
  //     case IdxPathNode(i) :: t =>
  //       json match {
  //         case JArray(js) => js.lift(i).flatMap(j => search(Path(t), j))
  //         case _ => None
  //       }
  //     case Nil => Some(json)
  //   }

  //   Rule[II, js.Any] { json =>
  //     search(p, json) match {
  //       case None => Invalid(Seq(Path -> Seq(ValidationError("error.required"))))
  //       case Some(js) => Valid(js)
  //     }
  //   }.compose(r)
  // }

  // // // XXX: a bit of boilerplate
  // private def pickInS[T](implicit r: RuleLike[Seq[js.Any], T]): Rule[js.Any, T] =
  //   jsArrayR.map { case JArray(fs) => Seq(fs:_*) }.compose(r)
  // implicit def pickSeq[O](implicit r: RuleLike[js.Any, O]) = pickInS(seqR[js.Any, O])
  // implicit def pickSet[O](implicit r: RuleLike[js.Any, O]) = pickInS(setR[js.Any, O])
  // implicit def pickList[O](implicit r: RuleLike[js.Any, O]) = pickInS(listR[js.Any, O])
  // implicit def pickArray[O: scala.reflect.ClassTag](implicit r: RuleLike[js.Any, O]) = pickInS(arrayR[js.Any, O])
  // implicit def pickTraversable[O](implicit r: RuleLike[js.Any, O]) = pickInS(traversableR[js.Any, O])
}
