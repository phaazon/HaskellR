-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- Vectors that can be passed to and from R with no copying at all. These
-- vectors are wrappers over SEXP vectors used by R. Memory for vectors is
-- allocated from the R heap, and in such way that they can be converted to
-- a 'SEXP' by simple pointer arithmetic (see 'toSEXP').
--
-- The main difference between "Data.Vector.SEXP.Mutable" and
-- "Data.Vector.Storable" is that the former uses a header-prefixed data layout
-- (the header immediately precedes the payload of the vector). This means that
-- no additional pointer dereferencing is needed to reach the vector data. The
-- trade-off is, for mutable vectors, slicing is not supported. The reason is
-- that slicing header-prefixed vectors is generally not possible without
-- copying, which breaks the semantics of the API for 'MVector'.
--
-- To perform slicing, it is necessary to convert to a "Data.Vector.Storable"
-- vector first, using 'unsafeToStorable'.

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Vector.SEXP.Mutable
  ( -- * Mutable vectors of 'SEXP' types
    MVector(..)
  , IOVector
  , STVector
    -- * Accessors
    -- ** Length information
  , length
  , null
    -- * Construction
    -- ** Initialisation
  , new
  , unsafeNew
  , replicate
  , replicateM
  , clone
    -- ** Restricting memory usage
  , clear
    -- * Accessing individual elements
  , read
  , write
  , swap
  , unsafeRead
  , unsafeWrite
  , unsafeSwap
    -- * Modifying vectors
    -- ** Filling and copying
  , set
  , copy
  , move
  , unsafeCopy
  , unsafeMove
    -- * SEXP specific.
  , fromSEXP
  , toSEXP
  , unsafeToStorable
  , fromStorable
  ) where

import Data.Vector.SEXP.Base
import qualified Foreign.R as R
import Foreign.R (SEXP, SEXPTYPE)
import Foreign.R.Type (SSEXPTYPE, IsVector)
import Internal.Error

import Control.Applicative
import Control.Monad (liftM)
import Control.Monad.Primitive
  (PrimMonad, PrimState, RealWorld, unsafePrimToPrim, unsafeInlineIO)
import qualified Data.Vector.Generic.Mutable as G
import qualified Data.Vector.Storable.Mutable as Storable
import Data.Singletons (fromSing, sing)
import Data.Int

import Foreign (castPtr, Ptr, withForeignPtr)
import Foreign.Concurrent (newForeignPtr)
import Foreign.C
import Foreign.Storable
import Foreign.Marshal.Array (copyArray, moveArray)

import Prelude hiding (length, null, replicate, read)
import System.IO.Unsafe (unsafePerformIO)

#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>

-- | Mutable R vector. They are represented in memory with the same header as
-- 'SEXP' nodes. The second type paramater is a phantom parameter reflecting at
-- the type level the tag of the vector when viewed as a 'SEXP'. The tag of the
-- vector and the representation type are related via 'ElemRep'.
newtype MVector s (ty :: SEXPTYPE) r a = MVector { unMVector :: SEXP s ty }

type IOVector s ty   = MVector s ty RealWorld
type STVector s ty r = MVector s ty s

instance (VECTOR s ty a)
         => G.MVector (MVector s ty) a where
  basicLength (MVector s) = unsafeInlineIO $
    fromIntegral <$> {# get VECSEXP->vecsxp.length #} (R.unsexp s)
-- N.B. slicing can't be supported properly by vectors prefixed by header,
-- this means that we can support only a reducing size (required for
-- vectors algorithms), and slicing that is noop
  basicUnsafeSlice j m v
    | j == 0 && m == G.basicLength v = v
    | j == 0 = unsafePerformIO $ do
        let s = castPtr $ R.unsexp $ unMVector v
        {# set VECSEXP->vecsxp.length #} s (fromIntegral m :: CInt)
        return v
    | otherwise =
      failure "Data.Vector.SEXP.Mutable.slice"
              "unsafeSlice is not supported for SEXP vectors, to perform slicing convert vector to Storable."
  basicOverlaps mv1 mv2   = unMVector mv1 == unMVector mv2
  basicUnsafeNew n
    -- R calls using allocVector() for CHARSXP "defunct"...
    | fromSing (sing :: SSEXPTYPE ty) == R.Char =
      failure "Data.Vector.SEXP.Mutable.new"
              "R character vectors are immutable and globally cached. Use 'mkChar' instead."
    | otherwise =
      -- No functor instance available here in GHC < 7.10 (pre AMP).
      liftM fromSEXP $ unsafePrimToPrim (R.allocVectorProtected (sing :: SSEXPTYPE ty) n)
  basicUnsafeRead mv i     = unsafePrimToPrim
                           $ peekElemOff (toVecPtr mv) i
  basicUnsafeWrite mv i x  = unsafePrimToPrim
                           $ pokeElemOff (toVecPtr mv) i x
  basicSet mv x            = Prelude.mapM_ (\i -> G.basicUnsafeWrite mv i x) [0..G.basicLength mv]
  basicUnsafeCopy mv1 mv2  = unsafePrimToPrim $ do
      copyArray (toVecPtr mv1)
                (toVecPtr mv2)
                (G.basicLength mv1)
  basicUnsafeMove mv1 mv2  = unsafePrimToPrim $ do
      moveArray (toVecPtr mv1)
                (toVecPtr mv2)
                (G.basicLength mv1)

toVecPtr :: MVector s ty r a -> Ptr a
toVecPtr mv = castPtr (R.unsafeSEXPToVectorPtr $ unMVector mv)

-- | /O(1)/ Create a vector from a 'SEXP'.
fromSEXP :: (E s ty a, Storable a, IsVector ty)
         => R.SEXP s ty
         -> MVector s ty r a
fromSEXP s = MVector s

-- | /O(1)/ Convert a mutable vector to a 'SEXP'. This can be done efficiently,
-- without copy, because vectors in this module always include a 'SEXP' header
-- immediately before the vector data in memory.
toSEXP :: forall s a r ty. (E s ty a, IsVector ty, Storable a)
       => MVector s ty r a
       -> R.SEXP s ty
toSEXP = unMVector

-- Length information
-- ------------------

-- | Length of the mutable vector.
length :: VECTOR s ty a => MVector s ty r a -> Int
{-# INLINE length #-}
length (MVector s) =
    unsafeInlineIO $
    fromIntegral <$> {# get VECSEXP->vecsxp.length #} (R.unsexp s)

-- | Check whether the vector is empty
null :: VECTOR s ty a => (MVector s ty) r a -> Bool
{-# INLINE null #-}
null (MVector s) =
    unsafeInlineIO $
    ((/= (0::Int)) . fromIntegral) <$>
    {# get VECSEXP->vecsxp.length #} (R.unsexp s)

-- Initialisation
-- --------------

-- | Create a mutable vector of the given length.
new :: (PrimMonad m, VECTOR s ty a) => Int -> m (MVector s ty (PrimState m) a)
{-# INLINE new #-}
new = G.new

-- | Create a mutable vector of the given length. The length is not checked.
unsafeNew :: (PrimMonad m, VECTOR s ty a) => Int -> m (MVector s ty (PrimState m) a)
{-# INLINE unsafeNew #-}
unsafeNew = G.unsafeNew

-- | Create a mutable vector of the given length (0 if the length is negative)
-- and fill it with an initial value.
replicate :: (PrimMonad m, VECTOR s ty a) => Int -> a -> m (MVector s ty (PrimState m) a)
{-# INLINE replicate #-}
replicate = G.replicate

-- | Create a mutable vector of the given length (0 if the length is negative)
-- and fill it with values produced by repeatedly executing the monadic action.
replicateM :: (PrimMonad m, VECTOR s ty a) => Int -> m a -> m (MVector s ty (PrimState m) a)
{-# INLINE replicateM #-}
replicateM = G.replicateM

-- | Create a copy of a mutable vector.
clone :: (PrimMonad m, VECTOR s ty a)
      => MVector s ty (PrimState m) a -> m (MVector s ty (PrimState m) a)
{-# INLINE clone #-}
clone = G.clone

-- Restricting memory usage
-- ------------------------

-- | Reset all elements of the vector to some undefined value, clearing all
-- references to external objects. This is usually a noop for unboxed vectors.
clear :: (PrimMonad m, VECTOR s ty a) => MVector s ty (PrimState m) a -> m ()
{-# INLINE clear #-}
clear = G.clear

-- Accessing individual elements
-- -----------------------------

-- | Yield the element at the given position.
read :: (PrimMonad m, VECTOR s ty a)
     => MVector s ty (PrimState m) a -> Int -> m a
{-# INLINE read #-}
read = G.read

-- | Replace the element at the given position.
write :: (PrimMonad m, VECTOR s ty a)
      => MVector s ty (PrimState m) a -> Int -> a -> m ()
{-# INLINE write #-}
write = G.write

-- | Swap the elements at the given positions.
swap :: (PrimMonad m, VECTOR s ty a)
     => MVector s ty (PrimState m) a -> Int -> Int -> m ()
{-# INLINE swap #-}
swap = G.swap

-- | Yield the element at the given position. No bounds checks are performed.
unsafeRead :: (PrimMonad m, VECTOR s ty a)
           => MVector s ty (PrimState m) a -> Int -> m a
{-# INLINE unsafeRead #-}
unsafeRead = G.unsafeRead

-- | Replace the element at the given position. No bounds checks are performed.
unsafeWrite :: (PrimMonad m, VECTOR s ty a)
            => MVector s ty (PrimState m) a -> Int -> a -> m ()
{-# INLINE unsafeWrite #-}
unsafeWrite = G.unsafeWrite

-- | Swap the elements at the given positions. No bounds checks are performed.
unsafeSwap :: (PrimMonad m, VECTOR s ty a)
           => MVector s ty (PrimState m) a -> Int -> Int -> m ()
{-# INLINE unsafeSwap #-}
unsafeSwap = G.unsafeSwap

-- Filling and copying
-- -------------------

-- | Set all elements of the vector to the given value.
set :: (PrimMonad m, VECTOR s ty a) => MVector s ty (PrimState m) a -> a -> m ()
{-# INLINE set #-}
set = G.set

-- | Copy a vector. The two vectors must have the same length and may not
-- overlap.
copy :: (PrimMonad m, VECTOR s ty a)
     => MVector s ty (PrimState m) a
     -> MVector s ty (PrimState m) a
     -> m ()
{-# INLINE copy #-}
copy = G.copy

-- | Copy a vector. The two vectors must have the same length and may not
-- overlap. This is not checked.
unsafeCopy :: (PrimMonad m, VECTOR s ty a)
           => MVector s ty (PrimState m) a   -- ^ target
           -> MVector s ty (PrimState m) a   -- ^ source
           -> m ()
{-# INLINE unsafeCopy #-}
unsafeCopy = G.unsafeCopy

-- | Move the contents of a vector. The two vectors must have the same
-- length.
--
-- If the vectors do not overlap, then this is equivalent to 'copy'.
-- Otherwise, the copying is performed as if the source vector were
-- copied to a temporary vector and then the temporary vector was copied
-- to the target vector.
move :: (PrimMonad m, VECTOR s ty a)
     => MVector s ty (PrimState m) a
     -> MVector s ty (PrimState m) a
     -> m ()
{-# INLINE move #-}
move = G.move

-- | Move the contents of a vector. The two vectors must have the same
-- length, but this is not checked.
--
-- If the vectors do not overlap, then this is equivalent to 'unsafeCopy'.
-- Otherwise, the copying is performed as if the source vector were
-- copied to a temporary vector and then the temporary vector was copied
-- to the target vector.
unsafeMove :: (PrimMonad m, VECTOR s ty a)
           => MVector s ty (PrimState m) a          -- ^ target
           -> MVector s ty (PrimState m) a          -- ^ source
           -> m ()
{-# INLINE unsafeMove #-}
unsafeMove = G.unsafeMove

-- | O(1) Inplace convertion to Storable vector.
unsafeToStorable :: (PrimMonad m, VECTOR s ty a)
                 => MVector s ty (PrimState m) a           -- ^ target
                 -> m (Storable.MVector (PrimState m) a) -- ^ source
{-# INLINE unsafeToStorable #-}
unsafeToStorable v@(MVector p) = unsafePrimToPrim $ do
  R.preserveObject p
  ptr <- newForeignPtr (toVecPtr v) (R.releaseObject (R.sexp $ castPtr $ toVecPtr v))
  return $ Storable.unsafeFromForeignPtr0 ptr (length v)

-- | O(N) Convertion from storable vector to SEXP vector.
fromStorable :: (PrimMonad m, VECTOR s ty a)
             => Storable.MVector (PrimState m) a
             -> m (MVector s ty (PrimState m) a)
{-# INLINE fromStorable #-}
fromStorable v = do
  let (fptr, l) = Storable.unsafeToForeignPtr0 v
  mv <- new l
  unsafePrimToPrim $ withForeignPtr fptr $ \p -> do
    copyArray (toVecPtr mv) p (Storable.length v)
  return mv
