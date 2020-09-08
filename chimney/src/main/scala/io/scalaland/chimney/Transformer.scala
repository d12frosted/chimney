package io.scalaland.chimney

import io.scalaland.chimney.dsl.{DefaultTransformerFlags, TransformerDefinition, TransformerFDefinition}
import io.scalaland.chimney.internal.TransformerCfg
import io.scalaland.chimney.internal.macros.ChimneyBlackboxMacros

import scala.language.experimental.macros
import scala.reflect.runtime.universe.TypeTag

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
    * @tparam Flags configuration flags of derived transformer
    * @return [[io.scalaland.chimney.Transformer]] type class definition
    */
  implicit def derive[From, To, Flags <: TransformerFlags.Type: TypeTag](
      implicit flags: Flags
  ): Transformer[From, To] =
    macro ChimneyBlackboxMacros.deriveTransformerImpl[From, To, Flags]

  /** Creates an empty [[io.scalaland.chimney.dsl.TransformerDefinition]] that
    * you can customize to derive [[io.scalaland.chimney.Transformer]].
    *
    * @see [[io.scalaland.chimney.dsl.TransformerDefinition]] for available settings
    *
    * @tparam From type of input value
    * @tparam To type of output value
    * @return [[io.scalaland.chimney.dsl.TransformerDefinition]] with defaults
    */
  def define[From, To]: TransformerDefinition[From, To, DefaultTransformerFlags, TransformerCfg.Empty] =
    new TransformerDefinition[From, To, DefaultTransformerFlags, TransformerCfg.Empty](Map.empty, Map.empty)

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
    DefaultTransformerFlags,
    TransformerCfg.WrapperType[F, TransformerCfg.Empty]
  ] =
    TransformerF.define[F, From, To]
}
