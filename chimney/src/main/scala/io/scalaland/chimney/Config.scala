package io.scalaland.chimney

import io.scalaland.chimney.internal.utils.MacroUtils

import scala.language.existentials
import scala.reflect.macros.blackbox

sealed abstract class Config

object Config {
  final class Empty extends Config

  final class EnableDefaultValues[C <: Config] extends Config
  final class DisableDefaultValues[C <: Config] extends Config
}

trait Configuration extends MacroUtils {
  val c: blackbox.Context

  import c.universe._

  object ConfigTpeConstructors {
    import Config._

    val emptyT = typeOf[Empty]
    val enableDefaultValuesT = typeOf[EnableDefaultValues[_]].typeConstructor
    val disableDefaultValuesT = typeOf[DisableDefaultValues[_]].typeConstructor
  }

  def materialize(configTpe: Type): MConfig = {
    import ConfigTpeConstructors._

    if (configTpe =:= emptyT) {
      MConfig(
        processDefaultValues = true
      )
    } else if (configTpe.typeConstructor =:= enableDefaultValuesT) {
      materialize(configTpe.typeArgs.head).copy(processDefaultValues = true)
    } else if (configTpe.typeConstructor =:= disableDefaultValuesT) {
      materialize(configTpe.typeArgs.head).copy(processDefaultValues = false)
    } else {
      // $COVERAGE-OFF$
      c.abort(c.enclosingPosition, s"Bad internal config type shape!: $configTpe")
      // $COVERAGE-ON$
    }
  }
}

case class MConfig(processDefaultValues: Boolean)
