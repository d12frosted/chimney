package io.scalaland.chimney

import io.scalaland.chimney.internal.TransformerOptions
import io.scalaland.chimney.internal.utils.MacroUtils

import scala.reflect.macros.blackbox

class TransformerFlags[Flags] {
  import TransformerFlags._

  def enableDefaultValues: TransformerFlags[Flags with EnableDefaultValues] = new TransformerFlags
  def disableDefaultValues: TransformerFlags[Flags with DisableDefaultValues] = new TransformerFlags

  def enableUnsafeOption: TransformerFlags[Flags with EnableUnsafeOption] = new TransformerFlags
  def disableUnsafeOption: TransformerFlags[Flags with DisableUnsafeOption] = new TransformerFlags
}

object TransformerFlags {
  trait Empty
  trait EnableDefaultValues
  trait DisableDefaultValues
  trait EnableUnsafeOption
  trait DisableUnsafeOption

  type Default = Empty with EnableDefaultValues

  val empty = new TransformerFlags[Empty]
}

trait TransformerFlagsSupport extends MacroUtils {
  val c: blackbox.Context

  import c.universe._

  object FlagTpeConstructors {
    import TransformerFlags._

    val emptyT = typeOf[Empty]
    val enableDefaultValuesT = typeOf[EnableDefaultValues]
    val disableDefaultValuesT = typeOf[DisableDefaultValues]
    val enableUnsafeOptionT = typeOf[EnableUnsafeOption]
    val disableUnsafeOptionT = typeOf[DisableUnsafeOption]
  }

  def materialize(tpe: Type): TransformerOptions = {
    import FlagTpeConstructors._

    val res = TransformerOptions(
      processDefaultValues = if (tpe <:< disableDefaultValuesT) false else tpe <:< enableDefaultValuesT,
      enableUnsafeOption = if (tpe <:< disableUnsafeOptionT) false else tpe <:< enableUnsafeOptionT
    )

    c.info(c.enclosingPosition, s"$tpe -->>>> $res", force = true)

    res
  }
}
