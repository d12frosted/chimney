package io.scalaland.chimney

import io.scalaland.chimney.dsl.{DefaultTransformerConfig, TransformerDefinition, TransformerFDefinition}
import io.scalaland.chimney.internal.TransformerCfg
import io.scalaland.chimney.internal.macros.ChimneyBlackboxMacros

import scala.language.experimental.macros

/** Type class expressing total transformation between
  * source type `From` and target type `To`.
  *
  * @tparam From type of input value
  * @tparam To   type of output value
  */
trait Transformer[From, To] {
  def transform(src: From): To
}

object Transformer {

  /** Provides [[io.scalaland.chimney.Transformer]] derived with the default settings.
    *
    * When transformation can't be derived, it results with compilation error.
    *
    * @tparam From type of input value
    * @tparam To type of output value
    * @return [[io.scalaland.chimney.Transformer]] type class definition
    */
  implicit def derive[From, To, Config]( // TODO: add constraint
      implicit config: Evidence[Config]
  ): Transformer[From, To] =
    macro ChimneyBlackboxMacros.deriveTransformerImpl[From, To, Config]

  /** Creates an empty [[io.scalaland.chimney.dsl.TransformerDefinition]] that
    * you can customize to derive [[io.scalaland.chimney.Transformer]].
    *
    * @see [[io.scalaland.chimney.dsl.TransformerDefinition]] for available settings
    *
    * @tparam From type of input value
    * @tparam To type of output value
    * @return [[io.scalaland.chimney.dsl.TransformerDefinition]] with defaults
    */
  def define[From, To]: TransformerDefinition[From, To, DefaultTransformerConfig, TransformerCfg.Empty] =
    new TransformerDefinition[From, To, DefaultTransformerConfig, TransformerCfg.Empty](Map.empty, Map.empty)

  /** Creates an empty [[io.scalaland.chimney.dsl.TransformerFDefinition]] that
    * you can customize to derive [[io.scalaland.chimney.TransformerF]].
    *
    * @see [[io.scalaland.chimney.dsl.TransformerFDefinition]] for available settings
    *
    * @tparam F    wrapper type constructor
    * @tparam From type of input value
    * @tparam To   type of output value
    * @return [[io.scalaland.chimney.dsl.TransformerFDefinition]] with defaults
    */
  def defineF[F[+_], From, To]: TransformerFDefinition[
    F,
    From,
    To,
    DefaultTransformerConfig,
    TransformerCfg.WrapperType[F, TransformerCfg.Empty]
  ] =
    TransformerF.define[F, From, To]
}
