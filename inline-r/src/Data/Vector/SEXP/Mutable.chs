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
import Control.Monad.R.Class
import Internal.Error

import Control.Applicative
import Control.Monad.Primitive
  (PrimMonad, PrimState, unsafePrimToPrim, unsafeInlineIO)
import qualified Data.Vector.Storable.Mutable as Storable
import Data.Foldable (for_)
import Data.Singletons (fromSing, sing)
import Data.Int

import Foreign (castPtr, Ptr, withForeignPtr)
import Foreign.Concurrent (newForeignPtr)
import Foreign.Storable
import Foreign.Marshal.Array (copyArray, moveArray)

import Prelude hiding (length, null, replicate, read)

#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>

-- | Mutable R vector. They are represented in memory with the same header as
-- 'SEXP' nodes. The second type paramater is a phantom parameter reflecting at
-- the type level the tag of the vector when viewed as a 'SEXP'. The tag of the
-- vector and the representation type are related via 'ElemRep'.
newtype MVector s (ty :: SEXPTYPE) a = MVector { unMVector :: SEXP s ty }

toVecPtr :: MVector s ty a -> Ptr a
toVecPtr mv = castPtr (R.unsafeSEXPToVectorPtr $ unMVector mv)

-- Length information
-- ------------------

-- | Length of the mutable vector.
length :: MVector s ty a -> Int
{-# INLINE length #-}
length (MVector s) =
    unsafeInlineIO $
    fromIntegral <$> {# get VECSEXP->vecsxp.length #} (R.unsexp s)

-- | Check whether the vector is empty
null :: MVector s ty a -> Bool
{-# INLINE null #-}
null (MVector s) =
    unsafeInlineIO $
    ((/= (0::Int)) . fromIntegral) <$>
    {# get VECSEXP->vecsxp.length #} (R.unsexp s)

-- Initialisation
-- --------------

-- | Create a mutable vector of the given length.
new :: forall m ty a . (VECTOR (Region m) ty a, MonadR m) => Int -> m (MVector (Region m) ty a)
{-# INLINE new #-}
new n
  | n < fromIntegral (maxBound :: Int32) = unsafeNew n
  | otherwise    = failure "Data.Vector.SEXP.Mutable.new"
                           "Vector size is too big"

-- | Create a mutable vector of the given length. The length is not checked.
unsafeNew :: forall m ty a . (VECTOR (Region m) ty a, MonadR m) => Int -> m (MVector (Region m) ty a)
{-# INLINE unsafeNew #-}
unsafeNew n
  -- R calls using allocVector() for CHARSXP "defunct"...
  | fromSing (sing :: SSEXPTYPE ty) == R.Char =
     failure "Data.Vector.SEXP.Mutable.new"
             "R character vectors are immutable and globally cached. Use 'mkChar' instead."
  | otherwise =
    MVector <$> (io (R.allocVector (sing :: SSEXPTYPE ty) n) >>= acquire)

-- | Create a mutable vector of the given length (0 if the length is negative)
-- and fill it with an initial value.
replicate :: forall m ty a . (VECTOR (Region m) ty a, MonadR m) => Int -> a -> m (MVector (Region m) ty a)
{-# INLINE replicate #-}
replicate n a = do
  v <- new n
  for_ [0..n] $ \i -> unsafeWrite v i a
  return v

-- | Create a mutable vector of the given length (0 if the length is negative)
-- and fill it with values produced by repeatedly executing the monadic action.
replicateM :: (MonadR m, VECTOR (Region m) ty a) => Int -> m a -> m (MVector (Region m) ty a)
{-# INLINE replicateM #-}
replicateM n m = do
  v <- new n
  for_ [0..n] $ \i -> m >>= unsafeWrite v i
  return v

-- Accessing individual elements
-- -----------------------------

-- | Replace the element at the given position. No bounds checks are performed.
unsafeWrite :: (MonadR m, VECTOR (Region m) ty a)
            => MVector (Region m) ty a -> Int -> a -> m ()
{-# INLINE unsafeWrite #-}
unsafeWrite mv i x = io $ pokeElemOff (toVecPtr mv) i x

-- | Yield the element at the given position. No bounds checks are performed.
unsafeRead :: (MonadR m, VECTOR (Region m) ty a)
           => MVector (Region m) ty a -> Int -> m a
{-# INLINE unsafeRead #-}
unsafeRead mv i = io $ peekElemOff (toVecPtr mv) i

-- | Swap the elements at the given positions. No bounds checks are performed.
unsafeSwap :: (MonadR m, VECTOR (Region m) ty a)
           => MVector (Region m) ty a -> Int -> Int -> m ()
{-# INLINE unsafeSwap #-}
unsafeSwap mv i j = do
  x <- unsafeRead mv i
  y <- unsafeRead mv j
  unsafeWrite mv i y
  unsafeWrite mv j x

-- | Swap the elements at the given positions.
swap :: (MonadR m, VECTOR (Region m) ty a)
     => MVector (Region m) ty a -> Int -> Int -> m ()
{-# INLINE swap #-}
swap mv i j = do
  x <- read mv i
  y <- read mv j
  write mv i y
  write mv j x

-- | Replace the element at the given position.
write :: (MonadR m, VECTOR (Region m) ty a)
      => MVector (Region m) ty a -> Int -> a -> m ()
{-# INLINE write #-}
write mv i v
  | 0 < i && i < length mv = unsafeWrite mv i v
  | otherwise = failure "Data.Vector.SEXP.Mutable.write"
                        "index out of bounds"

-- | Yield the element at the given position.
read :: (MonadR m, VECTOR (Region m) ty a)
     => MVector (Region m) ty a -> Int -> m a
{-# INLINE read #-}
read m i
  | 0 < i && i < length m = unsafeRead m i
  | otherwise = failure "Data.Vector.SEXP.Mutable.read"
                        "index out of bounds"

-- | Create a copy of a mutable vector.
clone :: (MonadR m, VECTOR (Region m) ty a)
      => MVector (Region m) ty a -> m (MVector (Region m) ty a)
{-# INLINE clone #-}
clone mv1 = do
  mv2 <- new (length mv1)
  for_ [0..length mv1] $ \i -> unsafeRead mv1 i >>= unsafeWrite mv2 i
  return mv2

-- Filling and copying
-- -------------------

-- | Set all elements of the vector to the given value.
set :: (MonadR m, VECTOR (Region m) ty a)
    => MVector (Region m) ty a -> a -> m ()
{-# INLINE set #-}
set mv a = for_ [0..length mv] $ \i -> unsafeWrite mv i a

-- | Copy a vector. The two vectors must have the same length and may not
-- overlap.
copy :: (MonadR m, VECTOR (Region m) ty a)
     => MVector (Region m) ty a
     -> MVector (Region m) ty a
     -> m ()
{-# INLINE copy #-}
copy mv1 mv2
  | length mv1 == length mv2 = unsafeCopy mv1 mv2
  | otherwise = failure "Data.Vector.SEXP.Mutable.copy"
                        "vectors should have same size"

-- | Copy a vector. The two vectors must have the same length and may not
-- overlap. This is not checked.
unsafeCopy :: (MonadR m, VECTOR (Region m) ty a)
           => MVector (Region m) ty a   -- ^ target
           -> MVector (Region m) ty a   -- ^ source
           -> m ()
{-# INLINE unsafeCopy #-}
unsafeCopy mv1 mv2 = io $
  copyArray (toVecPtr mv1)
            (toVecPtr mv2)
            (length mv1)

-- | Move the contents of a vector. The two vectors must have the same
-- length.
--
-- If the vectors do not overlap, then this is equivalent to 'copy'.
-- Otherwise, the copying is performed as if the source vector were
-- copied to a temporary vector and then the temporary vector was copied
-- to the target vector.
move :: (MonadR m, VECTOR (Region m) ty a)
     => MVector (Region m) ty a
     -> MVector (Region m) ty a
     -> m ()
{-# INLINE move #-}
move mv1 mv2
  | length mv1 == length mv2 = unsafeMove mv1 mv2
  | otherwise = failure "Data.Vector.SEXP.Mutable.move"
                        "vectors should have same size"

-- | Move the contents of a vector. The two vectors must have the same
-- length, but this is not checked.
--
-- If the vectors do not overlap, then this is equivalent to 'unsafeCopy'.
-- Otherwise, the copying is performed as if the source vector were
-- copied to a temporary vector and then the temporary vector was copied
-- to the target vector.
unsafeMove :: (MonadR m, VECTOR (Region m) ty a)
           => MVector (Region m) ty a          -- ^ target
           -> MVector (Region m) ty a          -- ^ source
           -> m ()
{-# INLINE unsafeMove #-}
unsafeMove mv1 mv2 = io $
      moveArray (toVecPtr mv1)
                (toVecPtr mv2)
                (length mv1)

-- | /O(1)/ Create a vector from a 'SEXP'.
fromSEXP :: (E m ty a, Storable a, IsVector ty)
         => R.SEXP m ty
         -> MVector m ty a
fromSEXP = MVector

-- | /O(1)/ Convert a mutable vector to a 'SEXP'. This can be done efficiently,
-- without copy, because vectors in this module always include a 'SEXP' header
-- immediately before the vector data in memory.
toSEXP :: forall m a ty. (E m ty a, IsVector ty, Storable a)
       => MVector m ty a
       -> R.SEXP m ty
toSEXP = unMVector

-- | /O(1)/ Inplace convertion to Storable vector.
-- This method do not honor *NAMED* attribute of 'SEXP', this means that
-- this vector share data with other vectors, and each modification operation
-- should copy vector. If you need to do modifications and you are not sure
-- if vector is share data or not concider using 'toStorable'.
--
-- Contents of the vector will are guaranteed to be protected while vector
-- itself is alive.
unsafeToStorable :: (PrimMonad m, VECTOR r ty a)
                 => MVector r ty a                       -- ^ source
                 -> m (Storable.MVector (PrimState m) a) -- ^ target
{-# INLINE unsafeToStorable #-}
unsafeToStorable v@(MVector p) = unsafePrimToPrim $ do
  R.preserveObject p
  ptr <- newForeignPtr (toVecPtr v) (R.releaseObject (R.sexp $ castPtr $ toVecPtr v))
  return $ Storable.unsafeFromForeignPtr0 ptr (length v)


{-
overStorable :: (PrimMonad m, VECTOR r ty a)
             => MVector r ty a
             -> (forall s . ST s 
-}

-- | O(N) Convertion from storable vector to SEXP vector.
fromStorable :: (MonadR r, PrimMonad r, VECTOR (Region r) ty a)
             => Storable.MVector m a
             -> r (MVector (Region r) ty a)
{-# INLINE fromStorable #-}
fromStorable v = do
  let (fptr, l) = Storable.unsafeToForeignPtr0 v
  mv <- new l
  io $ withForeignPtr fptr $ \p -> do
    copyArray (toVecPtr mv) p (Storable.length v)
  return mv
