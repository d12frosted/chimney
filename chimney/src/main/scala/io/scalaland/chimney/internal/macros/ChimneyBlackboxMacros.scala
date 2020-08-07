package io.scalaland.chimney.internal.macros

import io.scalaland.chimney
import io.scalaland.chimney.internal.utils.{DerivationGuards, EitherUtils, MacroUtils}
import io.scalaland.chimney.{Config, Patcher, TransformerF, TransformerFSupport}

import scala.reflect.macros.blackbox

class ChimneyBlackboxMacros(val c: blackbox.Context)
    extends PatcherMacros
    with TransformerMacros
    with DerivationGuards
    with MacroUtils
    with EitherUtils {

  import c.universe._

  def buildTransformerImpl[From: WeakTypeTag, To: WeakTypeTag, C: WeakTypeTag](
      config: c.Expr[Config]
  ): c.Expr[chimney.Transformer[From, To]] = {
    c.Expr[chimney.Transformer[From, To]] {
      reify {
        buildDefinedTransformer[From, To, C](config.splice)
      }.tree // this doesn't work
    }
  }

  def buildTransformerFImpl[F[+_], From: WeakTypeTag, To: WeakTypeTag, C: WeakTypeTag](
      config: c.Expr[Config],
      tfs: c.Expr[TransformerFSupport[F]]
  ): c.Expr[TransformerF[F, From, To]] = {
    c.Expr[TransformerF[F, From, To]] {
      reify {
        buildDefinedTransformer[From, To, C](config.splice, tfs.tree)
      }.tree // this doesn't work
    }
  }

  def transformImpl[From: WeakTypeTag, To: WeakTypeTag, C: WeakTypeTag](config: c.Expr[Config]): c.Expr[To] = {
    c.Expr[To] {
      reify {
        expandTransform[From, To, C](config.splice)
      }.tree // this doesn't work
    }
  }

  def transformFImpl[F[+_], From: WeakTypeTag, To: WeakTypeTag, C: WeakTypeTag](
      config: c.Expr[Config],
      tfs: c.Expr[TransformerFSupport[F]]
  ): c.Expr[F[To]] = {
    c.Expr[F[To]] {
      reify {
        expandTransform[From, To, C](config.splice, tfs.tree)
      }.tree // this doesn't work
    }
  }

  def deriveTransformerImpl[From: WeakTypeTag, To: WeakTypeTag](
      config: c.Expr[Config]
  ): c.Expr[chimney.Transformer[From, To]] = {
    c.Expr[chimney.Transformer[From, To]](
      reify {
        val aConfig = config.splice
        genTransformer[From, To](
          TransformerConfig(
            processDefaultValues = aConfig.useDefaultValues,
            definitionScope = Some((weakTypeOf[From], weakTypeOf[To]))
          )
        )
      }.tree // this doesn't work
    )
  }

  def deriveTransformerFImpl[F[+_], From: WeakTypeTag, To: WeakTypeTag](
      tfs: c.Expr[TransformerFSupport[F]]
  )(
      implicit F: WeakTypeTag[F[_]]
  ): c.Expr[TransformerF[F, From, To]] = {
    c.Expr[TransformerF[F, From, To]](
      genTransformer[From, To](
        TransformerConfig(
          definitionScope = Some((weakTypeOf[From], weakTypeOf[To])),
          wrapperType = Some(F.tpe),
          wrapperSupportInstance = tfs.tree
        )
      )
    )
  }

  def patchImpl[T: WeakTypeTag, Patch: WeakTypeTag, C: WeakTypeTag]: c.Expr[T] = {
    c.Expr[T](expandPatch[T, Patch, C])
  }

  def derivePatcherImpl[T: WeakTypeTag, Patch: WeakTypeTag]: c.Expr[Patcher[T, Patch]] = {
    genPatcher[T, Patch](PatcherConfig())
  }
}
