package io.scalaland.chimney.dsl

import io.scalaland.chimney.internal.TransformerCfg
import io.scalaland.chimney.internal.TransformerCfg.{
  EnableBeanGetters,
  EnableBeanSetters,
  EnableMethodAccessors,
  EnableOptionDefaultsToNone
}
import io.scalaland.chimney._

trait AConfigDsl[CC[_ <: TransformerConfig.Type], C <: TransformerConfig.Type] {

  /** Fail derivation if `From` type is missing field even if `To` has default value for it.
    *
    * By default in such case derivation will fallback to default values.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/default-values.html#disabling-default-values-in-generated-transformer]] for more details
    */
  def disableDefaultValues[UnsafeOptionC <: UnsafeOption](
      implicit f: UnsafeOptionExtractor.Aux[C, UnsafeOptionC]
  ): CC[TransformerConfig[DisableDefaultValues, UnsafeOptionC]] =
    this.asInstanceOf[CC[TransformerConfig[DisableDefaultValues, UnsafeOptionC]]]

  /** Use default value of field in `To` if field is missing in `From`.
    *
    * By default in such case derivation will fallback to default values.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/default-values.html#disabling-default-values-in-generated-transformer]] for more details
    */
  def enableDefaultValues[UnsafeOptionC <: UnsafeOption](
      implicit f: UnsafeOptionExtractor.Aux[C, UnsafeOptionC]
  ): CC[TransformerConfig[EnableDefaultValues, UnsafeOptionC]] =
    this.asInstanceOf[CC[TransformerConfig[EnableDefaultValues, UnsafeOptionC]]]

  /** Disable unsafe call to `.get` when source type From contains field of type `Option[A]`,
    * but target type To defines this fields as `A`.
    *
    * By default in such case compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/unsafe-options.html]] for more details
    */
  def disableUnsafeOption[DefaultValuesC <: DefaultValues](
      implicit f: DefaultValuesExtractor.Aux[C, DefaultValuesC]
  ): CC[TransformerConfig[DefaultValuesC, DisableUnsafeOption]] =
    this.asInstanceOf[CC[TransformerConfig[DefaultValuesC, DisableUnsafeOption]]]

  /** Enable unsafe call to `.get` when source type From contains field of type `Option[A]`,
    * but target type To defines this fields as `A`.
    *
    * It's unsafe as code generated this way may throw at runtime.
    *
    * By default in such case compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/unsafe-options.html]] for more details
    */
  def enableUnsafeOption[DefaultValuesC <: DefaultValues](
      implicit f: DefaultValuesExtractor.Aux[C, DefaultValuesC]
  ): CC[TransformerConfig[DefaultValuesC, EnableUnsafeOption]] =
    this.asInstanceOf[CC[TransformerConfig[DefaultValuesC, EnableUnsafeOption]]]
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
