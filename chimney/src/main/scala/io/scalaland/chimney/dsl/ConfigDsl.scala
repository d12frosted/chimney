package io.scalaland.chimney.dsl

import io.scalaland.chimney.TransformerFlags._
import io.scalaland.chimney.internal.TransformerCfg
import io.scalaland.chimney.internal.TransformerCfg._

trait AConfigDsl[FlagsContainer[_], Flags] {
  def enableDefaultValues: FlagsContainer[Flags with EnableDefaultValues] =
    this.asInstanceOf[FlagsContainer[Flags with EnableDefaultValues]]

  def disableDefaultValues: FlagsContainer[Flags with DisableDefaultValues] =
    this.asInstanceOf[FlagsContainer[Flags with DisableDefaultValues]]

  def enableUnsafeOption: FlagsContainer[Flags with EnableUnsafeOption] =
    this.asInstanceOf[FlagsContainer[Flags with EnableUnsafeOption]]

  def disableUnsafeOption: FlagsContainer[Flags with DisableUnsafeOption] =
    this.asInstanceOf[FlagsContainer[Flags with DisableUnsafeOption]]

}

trait ConfigDsl[CC[_ <: TransformerCfg], C <: TransformerCfg] {

  /** Enable values to be supplied from method calls. Source method must be public and have no parameter list.
    *
    * By default this is disabled because method calls may perform side effects (e.g. mutations)
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#using-method-accessors]] for more details
    */
  def enableMethodAccessors: CC[EnableMethodAccessors[C]] =
    this.asInstanceOf[CC[EnableMethodAccessors[C]]]

  /** Enable Java Beans naming convention (`.getName`, `.isName`) on `From`.
    *
    * By default only Scala conversions (`.name`) are allowed.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/java-beans.html#reading-from-java-beans]] for more details
    */
  def enableBeanGetters: CC[EnableBeanGetters[C]] =
    this.asInstanceOf[CC[EnableBeanGetters[C]]]

  /** Enable Java Beans naming convention (`.setName(value)`) on `To`.
    *
    * By default only Scala conversions (`.copy(name = value)`) are allowed.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/java-beans.html#writing-to-java-beans]] for more details
    */
  def enableBeanSetters: CC[EnableBeanSetters[C]] =
    this.asInstanceOf[CC[EnableBeanSetters[C]]]

  /** Sets target value of optional field to None if field is missing from source type From.
    *
    * By default in such case compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/default-values.html#default-values-for-option-fields]] for more details
    */
  def enableOptionDefaultsToNone: CC[EnableOptionDefaultsToNone[C]] =
    this.asInstanceOf[CC[EnableOptionDefaultsToNone[C]]]

}
