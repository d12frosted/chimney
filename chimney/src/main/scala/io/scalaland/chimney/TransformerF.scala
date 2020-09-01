package io.scalaland.chimney

import io.scalaland.chimney.dsl.{DefaultTransformerConfig, TransformerFDefinition}
import io.scalaland.chimney.internal.TransformerCfg
import io.scalaland.chimney.internal.macros.ChimneyBlackboxMacros

import scala.language.experimental.macros

/** Type class expressing partial transformation between
  * source type `From` and target type `To`, wrapping
  * transformation result in type constructor `F`.
  *
  * Useful for validated transformations, where result
  * type is wrapped in Option, Either, Validated, etc...
  *
  * @see [[io.scalaland.chimney.TransformerFSupport]]
  *
  * @tparam F    wrapper type constructor
  * @tparam From type of input value
  * @tparam To   type of output value
  */
trait TransformerF[F[+_], From, To] {
  def transform(src: From): F[To]
}

object TransformerF {

  /** Provides [[io.scalaland.chimney.TransformerF]] derived with the default settings.
    *
    * When transformation can't be derived, it results with compilation error.
    *
    * @tparam F      wrapper type constructor
    * @tparam From   type of input value
    * @tparam To     type of output value
    * @tparam Config configuration of derived transformer
    * @return [[io.scalaland.chimney.TransformerF]] type class definition
    */
  implicit def derive[F[+_], From, To, Config <: TransformerConfig.Type](
      implicit tfs: TransformerFSupport[F],
      config: Evidence[Config]
  ): TransformerF[F, From, To] =
    macro ChimneyBlackboxMacros.deriveTransformerFImpl[F, From, To, Config]

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
  def define[F[+_], From, To]: TransformerFDefinition[F, From, To, DefaultTransformerConfig, TransformerCfg.WrapperType[
    F,
    TransformerCfg.Empty
  ]] =
    new TransformerFDefinition(Map.empty, Map.empty)

}
