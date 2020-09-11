package io.scalaland.chimney

import io.scalaland.chimney.internal.TransformerOptions
import io.scalaland.chimney.internal.utils.MacroUtils

import scala.language.existentials
import scala.reflect.macros.blackbox

object Flags {
  final class Enabled
  final class Disabled
}

final class TransformerFlags {
  type DefaultValues = Flags.Enabled
  type UnsafeOption = Flags.Disabled
}

trait TransformerFlagsMaterialization extends MacroUtils {
  val c: blackbox.Context

  import c.universe._

  def materialize(tpe: Type): TransformerOptions = {
    TransformerOptions(
      processDefaultValues = materializeFlag(extractMember(tpe, "DefaultValues")),
      enableUnsafeOption = materializeFlag(extractMember(tpe, "UnsafeOption"))
    )
  }

  private def extractMember(tpe: Type, name: String): Type = {
    tpe.member(TypeName(name)).asType.toTypeConstructor
  }

  object ConfigTpeConstructors {
    val enabledT = typeOf[Flags.Enabled].typeConstructor
    val disabledT = typeOf[Flags.Disabled].typeConstructor
  }

  private def materializeFlag(tpe: Type): Boolean = {
    import ConfigTpeConstructors._
    if (tpe =:= enabledT) {
      return true
    }
    if (tpe =:= disabledT) {
      return false
    }
    // $COVERAGE-OFF$
    c.abort(c.enclosingPosition, s"Bad Flag type shape!: ${tpe.resultType}")
    // $COVERAGE-ON$
  }
}
