package io.scalaland.chimney

import io.scalaland.chimney.internal.TransformerOptions
import io.scalaland.chimney.internal.utils.MacroUtils

import scala.language.existentials
import scala.reflect.macros.blackbox

trait Extractor[C] {
  type V
}

object Extractor {
  type Aux[C, V0] = Extractor[C] {
    type V = V0
  }

  private def extractor[C, V0]: Aux[C, V0] =
    new Extractor[C] {
      type V = V0
    }

  implicit def defaultValuesE[DefaultValuesC <: DefaultValues, UnsafeOptionC <: UnsafeOption]
      : Extractor.Aux[TransformerFlags[DefaultValuesC, UnsafeOptionC], DefaultValuesC] = Extractor.extractor

  implicit def unsafeOptionE[DefaultValuesC <: DefaultValues, UnsafeOptionC <: UnsafeOption]
      : Extractor.Aux[TransformerFlags[DefaultValuesC, UnsafeOptionC], UnsafeOptionC] = Extractor.extractor
}

sealed trait DefaultValues
final class EnableDefaultValues extends DefaultValues
final class DisableDefaultValues extends DefaultValues

sealed trait UnsafeOption
final class EnableUnsafeOption extends UnsafeOption
final class DisableUnsafeOption extends UnsafeOption

sealed trait MethodAccessors
final class EnableMethodAccessors extends MethodAccessors
final class DisableMethodAccessors extends MethodAccessors

final class TransformerFlags[DefaultValuesC <: DefaultValues, UnsafeOptionC <: UnsafeOption] {
  def enableDefaultValues: TransformerFlags[EnableDefaultValues, UnsafeOptionC] =
    new TransformerFlags[EnableDefaultValues, UnsafeOptionC]

  def disableDefaultValues: TransformerFlags[DisableDefaultValues, UnsafeOptionC] =
    new TransformerFlags[DisableDefaultValues, UnsafeOptionC]

  def enableUnsafeOption: TransformerFlags[DefaultValuesC, EnableUnsafeOption] =
    new TransformerFlags[DefaultValuesC, EnableUnsafeOption]

  def disableUnsafeOption: TransformerFlags[DefaultValuesC, DisableUnsafeOption] =
    new TransformerFlags[DefaultValuesC, DisableUnsafeOption]
}

object TransformerFlags {
  type Type = TransformerFlags[_ <: DefaultValues, _ <: UnsafeOption]
  type Builder[DefaultValuesC <: DefaultValues, UnsafeOptionC <: UnsafeOption] =
    TransformerFlags[DefaultValuesC, UnsafeOptionC]

  type DefaultType = TransformerFlags[EnableDefaultValues, DisableUnsafeOption]
  val default = new TransformerFlags[EnableDefaultValues, DisableUnsafeOption]
}

trait TransformerFlagsMaterialization extends MacroUtils {
  val c: blackbox.Context

  import c.universe._

  def materialize(tpe: Type): TransformerOptions = {
    val args = tpe.typeArgs
    TransformerOptions(
      processDefaultValues = materializeDefaultVales(
        extractTypeArg(tpe, args, 0)
      ) match {
        case _: EnableDefaultValues  => true
        case _: DisableDefaultValues => false
      },
      enableUnsafeOption = materializeUnsafeOption(
        extractTypeArg(tpe, args, 1)
      ) match {
        case _: EnableUnsafeOption  => true
        case _: DisableUnsafeOption => false
      }
    )
  }

  private def extractTypeArg(tpe: Type, args: List[Type], pos: Int): Type = {
    args
      .lift(pos)
      .getOrElse(
        // $COVERAGE-OFF$
        c.abort(c.enclosingPosition, s"Bad transformer config type shape!: $tpe")
        // $COVERAGE-ON$
      )

  }

  object ConfigTpeConstructors {
    val enableDefaultValuesT = typeOf[EnableDefaultValues].typeConstructor
    val disableDefaultValuesT = typeOf[DisableDefaultValues].typeConstructor

    val enableUnsafeOptionT = typeOf[EnableUnsafeOption].typeConstructor
    val disableUnsafeOptionT = typeOf[DisableUnsafeOption].typeConstructor
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
    c.abort(c.enclosingPosition, s"Bad DefaultValues type shape!: ${tpe.resultType}")
    // $COVERAGE-ON$
  }

  private def materializeUnsafeOption(tpe: Type): UnsafeOption = {
    import ConfigTpeConstructors._
    if (tpe =:= enableUnsafeOptionT) {
      return new EnableUnsafeOption
    }
    if (tpe =:= disableUnsafeOptionT) {
      return new DisableUnsafeOption
    }
    // $COVERAGE-OFF$
    c.abort(c.enclosingPosition, s"Bad UnsafeOption type shape!: ${tpe.resultType}")
    // $COVERAGE-ON$
  }

}