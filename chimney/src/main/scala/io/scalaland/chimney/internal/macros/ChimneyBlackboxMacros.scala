package io.scalaland.chimney.internal.macros

import io.scalaland.chimney
import io.scalaland.chimney.internal.utils.{DerivationGuards, EitherUtils, MacroUtils}
import io.scalaland.chimney.{Patcher, TransformerF, TransformerFSupport}

import scala.reflect.macros.blackbox

class ChimneyBlackboxMacros(val c: blackbox.Context)
    extends PatcherMacros
    with TransformerMacros
    with DerivationGuards
    with MacroUtils
    with EitherUtils {

  import c.universe._

  def buildTransformerImpl[From: WeakTypeTag, To: WeakTypeTag, C0: WeakTypeTag, C: WeakTypeTag]
      : c.Expr[chimney.Transformer[From, To]] = {
    c.Expr[chimney.Transformer[From, To]] {
      buildDefinedTransformer[From, To, C0, C]()
    }
  }

  def buildTransformerFImpl[F[+_], From: WeakTypeTag, To: WeakTypeTag, Config: WeakTypeTag, C: WeakTypeTag](
      tfs: c.Expr[TransformerFSupport[F]]
  ): c.Expr[TransformerF[F, From, To]] = {
    c.Expr[TransformerF[F, From, To]] {
      buildDefinedTransformer[From, To, Config, C](tfs.tree)
    }
  }

  def transformImpl[From: WeakTypeTag, To: WeakTypeTag, C0: WeakTypeTag, C: WeakTypeTag]: c.Expr[To] = {
    c.Expr[To] {
      expandTransform[From, To, C0, C]()
    }
  }

  def transformFImpl[F[+_], From: WeakTypeTag, To: WeakTypeTag, C0: WeakTypeTag, C: WeakTypeTag](
      tfs: c.Expr[TransformerFSupport[F]]
  ): c.Expr[F[To]] = {
    c.Expr[F[To]] {
      expandTransform[From, To, C0, C](tfs.tree)
    }
  }

  def deriveTransformerImpl[From: WeakTypeTag, To: WeakTypeTag, Flags: WeakTypeTag](
      config: c.Expr[Flags]
  ): c.Expr[chimney.Transformer[From, To]] = {
    val flagsTpe = weakTypeOf[Flags].dealias
    val options = materialize(flagsTpe)
    c.Expr[chimney.Transformer[From, To]] {
      genTransformer[From, To](
        TransformerConfig(
          options = options,
          definitionScope = Some((weakTypeOf[From], weakTypeOf[To]))
        )
      )
    }
  }

  def deriveTransformerFImpl[F[+_], From: WeakTypeTag, To: WeakTypeTag, O: WeakTypeTag](
      tfs: c.Expr[TransformerFSupport[F]],
      config: c.Expr[O]
  )(
      implicit F: WeakTypeTag[F[_]]
  ): c.Expr[TransformerF[F, From, To]] = {
    val optionsTpe = weakTypeOf[O].dealias
    val options = materialize(optionsTpe)
    c.Expr[TransformerF[F, From, To]](
      genTransformer[From, To](
        TransformerConfig(
          options = options,
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
