-- |
-- Copyright: (C) 2013 Amgen, Inc.
--

{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Vector.SEXP.Base where

#if MIN_VERSION_vector(0,11,0)
import qualified Data.Vector.Fusion.Bundle.Monadic as Bundle
import           Data.Vector.Fusion.Bundle.Monadic (sSize, sElems)
import           Data.Vector.Fusion.Bundle.Size (Size, smaller)
import qualified Data.Vector.Fusion.Stream.Monadic as Stream
import           Data.Vector.Fusion.Bundle (lift)
import           Data.Vector.Fusion.Util (Id)
#else
import qualified Data.Vector.Fusion.Stream as Stream
#endif

import qualified Data.Vector.Generic as G

import Foreign.R.Type
import Foreign.R (SEXP, SomeSEXP)

import Data.Singletons (SingI)

import Data.Complex (Complex)
import Data.Word (Word8)
import Data.Int (Int32)
import Foreign.Storable (Storable)

-- | Function from R types to the types of the representations of each element
-- in the vector.
type family ElemRep s (a :: SEXPTYPE)
type instance ElemRep s 'Char    = Word8
type instance ElemRep s 'Logical = Logical
type instance ElemRep s 'Int     = Int32
type instance ElemRep s 'Real    = Double
type instance ElemRep s 'Complex = Complex Double
type instance ElemRep s 'String  = SEXP s 'Char
type instance ElemRep s 'Vector  = SomeSEXP s
type instance ElemRep s 'Expr    = SomeSEXP s
type instance ElemRep s 'Raw     = Word8

-- | 'ElemRep' in the form of a relation, for convenience.
type E s a b = ElemRep s a ~ b

-- | Constraint synonym for all operations on vectors.
type VECTOR s ty a = (Storable a, IsVector ty, SingI ty, ElemRep s ty ~ a)

zipWith :: (G.Vector v0 a, G.Vector v1 b, G.Vector v2 c)
        => (a -> b -> c) -> v0 a -> v1 b -> v2 c
{-# INLINE zipWith #-}
zipWith f xs ys = unstream' (Stream.zipWith f (stream' xs) (stream' ys)) [size' xs, size' ys]

zipWith3 :: (G.Vector v0 a, G.Vector v1 b, G.Vector v2 c, G.Vector v3 d)
         => (a -> b -> c -> d) -> v0 a -> v1 b -> v2 c -> v3 d
{-# INLINE zipWith3 #-}
zipWith3 f as bs cs = unstream' (Stream.zipWith3 f (stream' as) (stream' bs) (stream' cs)) [size' as, size' bs, size' cs]

zipWith4 :: (G.Vector v0 a, G.Vector v1 b, G.Vector v2 c, G.Vector v3 d,
             G.Vector v4 e)
         => (a -> b -> c -> d -> e) -> v0 a -> v1 b -> v2 c -> v3 d -> v4 e
{-# INLINE zipWith4 #-}
zipWith4 f as bs cs ds = unstream' (Stream.zipWith4 f (stream' as) (stream' bs) (stream' cs) (stream' ds)) [size' as, size' bs, size' cs, size' ds]

zipWith5 :: (G.Vector v0 a, G.Vector v1 b, G.Vector v2 c, G.Vector v3 d,
             G.Vector v4 e, G.Vector v5 f)
         => (a -> b -> c -> d -> e -> f) -> v0 a -> v1 b -> v2 c -> v3 d -> v4 e
         -> v5 f
{-# INLINE zipWith5 #-}
zipWith5 f as bs cs ds es = unstream' (Stream.zipWith5 f (stream' as) (stream' bs) (stream' cs) (stream' ds) (stream' es)) [size' as, size' bs, size' cs, size' ds, size' es]

zipWith6 :: (G.Vector v0 a, G.Vector v1 b, G.Vector v2 c, G.Vector v3 d,
             G.Vector v4 e, G.Vector v5 f, G.Vector v6 g)
         => (a -> b -> c -> d -> e -> f -> g) -> v0 a -> v1 b -> v2 c -> v3 d
         -> v4 e -> v5 f -> v6 g
{-# INLINE zipWith6 #-}
zipWith6 f as bs cs ds es fs = unstream' (Stream.zipWith6 f (stream' as) (stream' bs) (stream' cs) (stream' ds) (stream' es) (stream' fs)) [size' as, size' bs, size' cs, size' ds, size' es, size' fs]

izipWith :: (G.Vector v0 a, G.Vector v1 b, G.Vector v2 c)
         => (Int -> a -> b -> c) -> v0 a -> v1 b -> v2 c
{-# INLINE izipWith #-}
izipWith f xs ys = unstream' (Stream.zipWith (uncurry f) (Stream.indexed (stream' xs)) (stream' ys)) [size' xs, size' ys]

izipWith3 :: (G.Vector v0 a, G.Vector v1 b, G.Vector v2 c, G.Vector v3 d)
          => (Int -> a -> b -> c -> d) -> v0 a -> v1 b -> v2 c -> v3 d
{-# INLINE izipWith3 #-}
izipWith3 f as bs cs = unstream' (Stream.zipWith3 (uncurry f) (Stream.indexed (stream' as)) (stream' bs) (stream' cs)) [size' as, size' bs, size' cs]

izipWith4 :: (G.Vector v0 a, G.Vector v1 b, G.Vector v2 c, G.Vector v3 d,
              G.Vector v4 e)
          => (Int -> a -> b -> c -> d -> e) -> v0 a -> v1 b -> v2 c -> v3 d
          -> v4 e
{-# INLINE izipWith4 #-}
izipWith4 f as bs cs ds = unstream' (Stream.zipWith4 (uncurry f) (Stream.indexed (stream' as)) (stream' bs) (stream' cs) (stream' ds)) [size' as, size' bs, size' cs, size' ds]

izipWith5 :: (G.Vector v0 a, G.Vector v1 b, G.Vector v2 c, G.Vector v3 d,
              G.Vector v4 e, G.Vector v5 f)
          => (Int -> a -> b -> c -> d -> e -> f) -> v0 a -> v1 b -> v2 c -> v3 d
          -> v4 e -> v5 f
{-# INLINE izipWith5 #-}
izipWith5 f as bs cs ds es = unstream' (Stream.zipWith5 (uncurry f) (Stream.indexed (stream' as)) (stream' bs) (stream' cs) (stream' ds) (stream' es)) [size' as, size' bs, size' cs, size' ds, size' es]


izipWith6 :: (G.Vector v0 a, G.Vector v1 b, G.Vector v2 c, G.Vector v3 d,
              G.Vector v4 e, G.Vector v5 f, G.Vector v6 g)
          => (Int -> a -> b -> c -> d -> e -> f -> g) -> v0 a -> v1 b -> v2 c
          -> v3 d -> v4 e -> v5 f -> v6 g
{-# INLINE izipWith6 #-}
izipWith6 f as bs cs ds es fs = unstream' (Stream.zipWith6 (uncurry f) (Stream.indexed (stream' as)) (stream' bs) (stream' cs) (stream' ds) (stream' es) (stream' fs)) [size' as, size' bs, size' cs, size' ds, size' es, size' fs]

zipWithM_ :: (G.Vector v0 a, G.Vector v1 b, Monad m)
          => (a -> b -> m c)
          -> v0 a -> v1 b -> m ()
{-# INLINE zipWithM_ #-}
#if MIN_VERSION_vector(0,11,0)
zipWithM_ f as bs = Stream.zipWithM_ f (sElems . lift $ G.stream as) (sElems . lift $ G.stream bs)
#else
zipWithM_ f as bs = Stream.zipWithM_ f (G.stream as) (G.stream bs)
#endif

-- * Helpers

#if MIN_VERSION_vector(0,11,0)
stream' :: G.Vector v a => v a -> Stream.Stream Id a
stream' = sElems . G.stream
#else
stream' :: G.Vector v a => v a -> Stream.Stream a
stream' = G.stream
#endif

#if MIN_VERSION_vector(0,11,0)
unstream' :: G.Vector v a => Stream.Stream Id a -> [Size] -> v a
unstream' str sz = G.unstream . Bundle.fromStream str $ smallest sz
#else
unstream' :: G.Vector v a => Stream.Stream a -> t -> v a
unstream' str _sz = G.unstream str
#endif

#if MIN_VERSION_vector(0,11,0)
size' :: G.Vector v a => v a -> Size
size' = sSize . G.stream
#else
size' :: a -> ()
size' = const ()
#endif

#if MIN_VERSION_vector(0,11,0)
smallest :: [Size] -> Size
smallest = foldl1 smaller
#endif
