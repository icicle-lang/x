{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP #-}
module X.Data.Vector.Primitive (
    module Primitive


  -- * Accessors

  -- ** Destructors
#if !MIN_VERSION_vector(0,12,2)
  , uncons
#endif

  -- * Elementwise operations

  -- ** Mapping
#if !MIN_VERSION_vector(0,12,0)
  , mapMaybe
  , imapMaybe
#endif
  , mapAccumulate

  -- ** Monadic mapping
#if !MIN_VERSION_vector(0,12,2)
  , mapMaybeM
  , imapMaybeM
#endif
  , mapAccumulateM

  -- * Modifying vectors

  -- ** Transposition
  , transpose

  -- ** Merging
  , merge
  ) where

import qualified Data.Vector as Boxed
import           Data.Vector.Primitive as Primitive

import           P hiding (mapMaybe)

import qualified X.Data.Vector.Generic as Generic


transpose :: Prim a => Boxed.Vector (Vector a) -> Boxed.Vector (Vector a)
transpose =
  Generic.transpose
{-# INLINE transpose #-}

#if !MIN_VERSION_vector(0,12,0)
mapMaybe :: (Prim a, Prim b) => (a -> Maybe b) -> Vector a -> Vector b
mapMaybe =
  Generic.mapMaybe
{-# INLINE mapMaybe #-}

imapMaybe :: (Prim a, Prim b) => (Int -> a -> Maybe b) -> Vector a -> Vector b
imapMaybe =
  Generic.imapMaybe
{-# INLINE imapMaybe #-}
#endif

#if !MIN_VERSION_vector(0,12,2)
mapMaybeM :: (Monad m, Prim a, Prim b) => (a -> m (Maybe b)) -> Vector a -> m (Vector b)
mapMaybeM =
  Generic.mapMaybeM
{-# INLINE mapMaybeM #-}

imapMaybeM :: (Monad m, Prim a, Prim b) => (Int -> a -> m (Maybe b)) -> Vector a -> m (Vector b)
imapMaybeM =
  Generic.imapMaybeM
{-# INLINE imapMaybeM #-}
#endif

mapAccumulate :: (Prim elt, Prim elt') => (acc -> elt -> (acc, elt')) -> acc -> Vector elt -> Vector elt'
mapAccumulate =
  Generic.mapAccumulate
{-# INLINE mapAccumulate #-}

mapAccumulateM :: (Monad m, Prim elt, Prim elt') => (acc -> elt -> m (acc, elt')) -> acc -> Vector elt -> m (Vector elt')
mapAccumulateM =
  Generic.mapAccumulateM
{-# INLINE mapAccumulateM #-}

merge :: Prim a => (a -> a -> Generic.MergePullFrom a) -> Vector a -> Vector a -> Vector a
merge =
  Generic.merge
{-# INLINE merge #-}

#if !MIN_VERSION_vector(0,12,2)
uncons :: Prim a => Vector a -> Maybe (a, Vector a)
uncons =
  Generic.uncons
{-# INLINE uncons #-}
#endif
