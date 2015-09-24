package jto.validation

import play.api.libs.json.{JsValue, JsObject}

package object json extends DerivationInduction {
  type Output = JsObject
  
  val typePath = Path \ "$type"
  
  implicit def ambigiousOptionWorkaround[T](implicit r: RuleLike[JsValue, T]) = {
    import Rules._
    implicitly[Path => RuleLike[JsValue, Option[T]]]
  }
}
