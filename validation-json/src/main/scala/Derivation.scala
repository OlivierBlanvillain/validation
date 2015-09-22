package jto.validation
package json

import play.api.libs.json.{JsValue, JsObject}

trait Derivation extends WriteProduct with WriteCoproduct with RuleProduct with RuleCoproduct {
  type Output = JsObject
  
  val typePath = Path \ "$type"
  
  implicit def ambigiousWorkaround[T](implicit coerce: RuleLike[JsValue, T]) = {
    import jto.validation.json.Rules._
    implicitly[Path => RuleLike[JsValue, Option[T]]]
  }
}

object Derivation extends Derivation
  
object DerivationReq extends Derivation with WriteGeneric with RuleGeneric
