package io.scalaland.chimney

import io.scalaland.chimney.internal.utils.MacroUtils

import scala.language.existentials
import scala.reflect.macros.blackbox

trait Evidence[C]

sealed trait DefaultValues
final class EnableDefaultValues extends DefaultValues
final class DisableDefaultValues extends DefaultValues

sealed trait UnsafeOption
final class EnableUnsafeOption extends UnsafeOption
final class DisableUnsafeOption extends UnsafeOption

sealed trait MethodAccessors
final class EnableMethodAccessors extends MethodAccessors
final class DisableMethodAccessors extends MethodAccessors

final class TransformerConfig[DefaultValuesC <: DefaultValues, UnsafeOptionC <: UnsafeOption] {
  def enableDefaultValues: TransformerConfig[EnableDefaultValues, UnsafeOptionC] =
    new TransformerConfig[EnableDefaultValues, UnsafeOptionC]

  def disableDefaultValues: TransformerConfig[DisableDefaultValues, UnsafeOptionC] =
    new TransformerConfig[DisableDefaultValues, UnsafeOptionC]

  def enableUnsafeOption: TransformerConfig[DefaultValuesC, EnableUnsafeOption] =
    new TransformerConfig[DefaultValuesC, EnableUnsafeOption]

  def disableUnsafeOption: TransformerConfig[DefaultValuesC, DisableUnsafeOption] =
    new TransformerConfig[DefaultValuesC, DisableUnsafeOption]
}

object TransformerConfig {
  type Type = TransformerConfig[_, _]
  type Builder[DefaultValuesC <: DefaultValues, UnsafeOptionC <: UnsafeOption] =
    TransformerConfig[DefaultValuesC, UnsafeOptionC]

  type DefaultType = TransformerConfig[EnableDefaultValues, DisableUnsafeOption]
  val default = new TransformerConfig[EnableDefaultValues, DisableUnsafeOption]

  def provide[C <: Type](config: C): Evidence[C] =
    new Evidence[C] {}
}

case class TransformerCfg(processDefaultValues: DefaultValues)

trait TransformerConfiguration extends MacroUtils {
  val c: blackbox.Context

  import c.universe._

  def materialize(tpe: Type): TransformerCfg = {
    val args = tpe.typeArgs
    TransformerCfg(
      processDefaultValues = materializeDefaultVales(
        args.headOption.getOrElse(
          // $COVERAGE-OFF$
          c.abort(c.enclosingPosition, s"Bad transformer config type shape!: $tpe")
          // $COVERAGE-ON$
        )
      )
    )
  }

  object ConfigTpeConstructors {
    val enableDefaultValuesT = typeOf[EnableDefaultValues].typeConstructor
    val disableDefaultValuesT = typeOf[DisableDefaultValues].typeConstructor
  }

  private def materializeDefaultVales(tpe: Type): DefaultValues = {
    import ConfigTpeConstructors._
    if (tpe =:= enableDefaultValuesT) {
      return new EnableDefaultValues
    }
    if (tpe =:= disableDefaultValuesT) {
      return new DisableDefaultValues
    }
    // $COVERAGE-OFF$
    c.abort(c.enclosingPosition, s"Bad default values type shape!: ${tpe.resultType}")
    // $COVERAGE-ON$
  }
}
