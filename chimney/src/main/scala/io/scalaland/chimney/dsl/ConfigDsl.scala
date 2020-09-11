package io.scalaland.chimney.dsl

import io.scalaland.chimney.internal.TransformerCfg
import io.scalaland.chimney.internal.TransformerCfg.{
  EnableBeanGetters,
  EnableBeanSetters,
  EnableMethodAccessors,
  EnableOptionDefaultsToNone
}
import io.scalaland.chimney._

trait ConfigDsl[R[_ <: TransformerCfg, _ <: TransformerFlags], Cfg <: TransformerCfg, Flags <: TransformerFlags] {

  /** Fail derivation if `From` type is missing field even if `To` has default value for it.
    *
    * By default in such case derivation will fallback to default values.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/default-values.html#disabling-default-values-in-generated-transformer]] for more details
    */
  def disableDefaultValues: R[Cfg, Flags { type DefaultValues = Flags.Disabled }] =
    this.asInstanceOf[R[Cfg, Flags { type DefaultValues = Flags.Disabled }]]

  /** Use default value of field in `To` if field is missing in `From`.
    *
    * By default in such case derivation will fallback to default values.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/default-values.html#disabling-default-values-in-generated-transformer]] for more details
    */
  def enableDefaultValues: R[Cfg, Flags { type DefaultValues = Flags.Enabled }] =
    this.asInstanceOf[R[Cfg, Flags { type DefaultValues = Flags.Enabled }]]

  /** Disable unsafe call to `.get` when source type From contains field of type `Option[A]`,
    * but target type To defines this fields as `A`.
    *
    * By default in such case compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/unsafe-options.html]] for more details
    */
  def disableUnsafeOption: R[Cfg, Flags { type UnsafeOption = Flags.Disabled }] =
    this.asInstanceOf[R[Cfg, Flags { type UnsafeOption = Flags.Disabled }]]

  /** Enable unsafe call to `.get` when source type From contains field of type `Option[A]`,
    * but target type To defines this fields as `A`.
    *
    * It's unsafe as code generated this way may throw at runtime.
    *
    * By default in such case compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/unsafe-options.html]] for more details
    */
  def enableUnsafeOption: R[Cfg, Flags { type UnsafeOption = Flags.Enabled }] =
    this.asInstanceOf[R[Cfg, Flags { type UnsafeOption = Flags.Enabled }]]

  /** Enable values to be supplied from method calls. Source method must be public and have no parameter list.
    *
    * By default this is disabled because method calls may perform side effects (e.g. mutations)
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#using-method-accessors]] for more details
    */
  def enableMethodAccessors: R[EnableMethodAccessors[Cfg], Flags] =
    this.asInstanceOf[R[EnableMethodAccessors[Cfg], Flags]]

  /** Enable Java Beans naming convention (`.getName`, `.isName`) on `From`.
    *
    * By default only Scala conversions (`.name`) are allowed.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/java-beans.html#reading-from-java-beans]] for more details
    */
  def enableBeanGetters: R[EnableBeanGetters[Cfg], Flags] =
    this.asInstanceOf[R[EnableBeanGetters[Cfg], Flags]]

  /** Enable Java Beans naming convention (`.setName(value)`) on `To`.
    *
    * By default only Scala conversions (`.copy(name = value)`) are allowed.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/java-beans.html#writing-to-java-beans]] for more details
    */
  def enableBeanSetters: R[EnableBeanSetters[Cfg], Flags] =
    this.asInstanceOf[R[EnableBeanSetters[Cfg], Flags]]

  /** Sets target value of optional field to None if field is missing from source type From.
    *
    * By default in such case compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/default-values.html#default-values-for-option-fields]] for more details
    */
  def enableOptionDefaultsToNone: R[EnableOptionDefaultsToNone[Cfg], Flags] =
    this.asInstanceOf[R[EnableOptionDefaultsToNone[Cfg], Flags]]
}
