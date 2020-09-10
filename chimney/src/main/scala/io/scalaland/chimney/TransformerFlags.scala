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

  implicit def defaultValuesE[DefaultValues, UnsafeOption]
      : Extractor.Aux[TransformerFlags[DefaultValues, UnsafeOption], DefaultValues] = Extractor.extractor

  implicit def unsafeOptionE[DefaultValues, UnsafeOption]
      : Extractor.Aux[TransformerFlags[DefaultValues, UnsafeOption], UnsafeOption] = Extractor.extractor
}

sealed trait DefaultValuesFlag
final class EnableDefaultValues extends DefaultValuesFlag
final class DisableDefaultValues extends DefaultValuesFlag

sealed trait UnsafeOptionFlag
final class EnableUnsafeOption extends UnsafeOptionFlag
final class DisableUnsafeOption extends UnsafeOptionFlag

sealed trait MethodAccessorsFlag
final class EnableMethodAccessors extends MethodAccessorsFlag
final class DisableMethodAccessors extends MethodAccessorsFlag

final class TransformerFlags[DefaultValues, UnsafeOption] {
  def enableDefaultValues: TransformerFlags[EnableDefaultValues, UnsafeOption] =
    new TransformerFlags[EnableDefaultValues, UnsafeOption]

  def disableDefaultValues: TransformerFlags[DisableDefaultValues, UnsafeOption] =
    new TransformerFlags[DisableDefaultValues, UnsafeOption]

  def enableUnsafeOption: TransformerFlags[DefaultValues, EnableUnsafeOption] =
    new TransformerFlags[DefaultValues, EnableUnsafeOption]

  def disableUnsafeOption: TransformerFlags[DefaultValues, DisableUnsafeOption] =
    new TransformerFlags[DefaultValues, DisableUnsafeOption]
}

object TransformerFlags {
  type Type = TransformerFlags[_, _]

  type DefaultValues = EnableDefaultValues
  type UnsafeOption = DisableUnsafeOption
  val default = new TransformerFlags[DefaultValues, UnsafeOption]
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

  private def materializeDefaultVales(tpe: Type): DefaultValuesFlag = {
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

  private def materializeUnsafeOption(tpe: Type): UnsafeOptionFlag = {
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
