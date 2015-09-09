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
#if MIN_VERSION_vector(0,11,0)
zipWith f xs ys = G.unstream $ Bundle.fromStream (Stream.zipWith f (sElems xs') (sElems ys')) sz
  where
    xs' = G.stream xs
    ys' = G.stream ys
    sz  = smallest [sSize xs', sSize ys']
#else
zipWith f xs ys = G.unstream (Stream.zipWith f (G.stream xs) (G.stream ys))
#endif

zipWith3 :: (G.Vector v0 a, G.Vector v1 b, G.Vector v2 c, G.Vector v3 d)
         => (a -> b -> c -> d) -> v0 a -> v1 b -> v2 c -> v3 d
{-# INLINE zipWith3 #-}
#if MIN_VERSION_vector(0,11,0)
zipWith3 f as bs cs = G.unstream $ Bundle.fromStream (Stream.zipWith3 f (sElems as') (sElems bs') (sElems cs')) sz
  where
    as' = G.stream as
    bs' = G.stream bs
    cs' = G.stream cs
    sz  = smallest [sSize as', sSize bs', sSize cs']
#else
zipWith3 f as bs cs = G.unstream (Stream.zipWith3 f (G.stream as) (G.stream bs) (G.stream cs))
#endif

zipWith4 :: (G.Vector v0 a, G.Vector v1 b, G.Vector v2 c, G.Vector v3 d,
             G.Vector v4 e)
         => (a -> b -> c -> d -> e) -> v0 a -> v1 b -> v2 c -> v3 d -> v4 e
{-# INLINE zipWith4 #-}
#if MIN_VERSION_vector(0,11,0)
zipWith4 f as bs cs ds = G.unstream $ Bundle.fromStream (Stream.zipWith4 f (sElems as') (sElems bs') (sElems cs') (sElems ds')) sz
  where
    as' = G.stream as
    bs' = G.stream bs
    cs' = G.stream cs
    ds' = G.stream ds
    sz  = smallest [sSize as', sSize bs', sSize cs', sSize ds']
#else
zipWith4 f as bs cs ds = G.unstream (Stream.zipWith4 f (G.stream as) (G.stream bs) (G.stream cs) (G.stream ds))
#endif

zipWith5 :: (G.Vector v0 a, G.Vector v1 b, G.Vector v2 c, G.Vector v3 d,
             G.Vector v4 e, G.Vector v5 f)
         => (a -> b -> c -> d -> e -> f) -> v0 a -> v1 b -> v2 c -> v3 d -> v4 e
         -> v5 f
{-# INLINE zipWith5 #-}
#if MIN_VERSION_vector(0,11,0)
zipWith5 f as bs cs ds es = G.unstream $ Bundle.fromStream (Stream.zipWith5 f (sElems as') (sElems bs') (sElems cs') (sElems ds') (sElems es')) sz
  where
    as' = G.stream as
    bs' = G.stream bs
    cs' = G.stream cs
    ds' = G.stream ds
    es' = G.stream es
    sz  = smallest [sSize as', sSize bs', sSize cs', sSize ds', sSize es']
#else
zipWith5 f as bs cs ds es = G.unstream (Stream.zipWith5 f (G.stream as) (G.stream bs) (G.stream cs) (G.stream ds) (G.stream es))
#endif

zipWith6 :: (G.Vector v0 a, G.Vector v1 b, G.Vector v2 c, G.Vector v3 d,
             G.Vector v4 e, G.Vector v5 f, G.Vector v6 g)
         => (a -> b -> c -> d -> e -> f -> g) -> v0 a -> v1 b -> v2 c -> v3 d
         -> v4 e -> v5 f -> v6 g
{-# INLINE zipWith6 #-}
#if MIN_VERSION_vector(0,11,0)
zipWith6 f as bs cs ds es fs = G.unstream $ Bundle.fromStream (Stream.zipWith6 f (sElems as') (sElems bs') (sElems cs') (sElems ds') (sElems es') (sElems fs')) sz
  where
    as' = G.stream as
    bs' = G.stream bs
    cs' = G.stream cs
    ds' = G.stream ds
    es' = G.stream es
    fs' = G.stream fs
    sz  = smallest [sSize as', sSize bs', sSize cs', sSize ds', sSize es', sSize fs']
#else
zipWith6 f as bs cs ds es fs = G.unstream (Stream.zipWith6 f (G.stream as) (G.stream bs) (G.stream cs) (G.stream ds) (G.stream es) (G.stream fs))
#endif

izipWith :: (G.Vector v0 a, G.Vector v1 b, G.Vector v2 c)
         => (Int -> a -> b -> c) -> v0 a -> v1 b -> v2 c
{-# INLINE izipWith #-}
#if MIN_VERSION_vector(0,11,0)
izipWith f xs ys = G.unstream $ Bundle.fromStream (Stream.zipWith (uncurry f) (Stream.indexed (sElems xs')) (sElems ys')) sz
  where
    xs' = G.stream xs
    ys' = G.stream ys
    sz  = smallest [sSize xs', sSize ys']
#else
izipWith f as bs = G.unstream (Stream.zipWith (uncurry f) (Stream.indexed (G.stream as)) (G.stream bs))
#endif

izipWith3 :: (G.Vector v0 a, G.Vector v1 b, G.Vector v2 c, G.Vector v3 d)
          => (Int -> a -> b -> c -> d) -> v0 a -> v1 b -> v2 c -> v3 d
{-# INLINE izipWith3 #-}
#if MIN_VERSION_vector(0,11,0)
izipWith3 f as bs cs = G.unstream $ Bundle.fromStream (Stream.zipWith3 (uncurry f) (Stream.indexed (sElems as')) (sElems bs') (sElems cs')) sz
  where
    as' = G.stream as
    bs' = G.stream bs
    cs' = G.stream cs
    sz  = smallest [sSize as', sSize bs', sSize cs']
#else
izipWith3 f as bs cs = G.unstream (Stream.zipWith3 (uncurry f) (Stream.indexed (G.stream as)) (G.stream bs) (G.stream cs))
#endif

izipWith4 :: (G.Vector v0 a, G.Vector v1 b, G.Vector v2 c, G.Vector v3 d,
              G.Vector v4 e)
          => (Int -> a -> b -> c -> d -> e) -> v0 a -> v1 b -> v2 c -> v3 d
          -> v4 e
{-# INLINE izipWith4 #-}
#if MIN_VERSION_vector(0,11,0)
izipWith4 f as bs cs ds = G.unstream $ Bundle.fromStream (Stream.zipWith4 (uncurry f) (Stream.indexed (sElems as')) (sElems bs') (sElems cs') (sElems ds')) sz
  where
    as' = G.stream as
    bs' = G.stream bs
    cs' = G.stream cs
    ds' = G.stream ds
    sz  = smallest [sSize as', sSize bs', sSize cs', sSize ds']
#else
izipWith4 f as bs cs ds = G.unstream (Stream.zipWith4 (uncurry f) (Stream.indexed (G.stream as)) (G.stream bs) (G.stream cs) (G.stream ds))
#endif

izipWith5 :: (G.Vector v0 a, G.Vector v1 b, G.Vector v2 c, G.Vector v3 d,
              G.Vector v4 e, G.Vector v5 f)
          => (Int -> a -> b -> c -> d -> e -> f) -> v0 a -> v1 b -> v2 c -> v3 d
          -> v4 e -> v5 f
{-# INLINE izipWith5 #-}
#if MIN_VERSION_vector(0,11,0)
izipWith5 f as bs cs ds es = G.unstream $ Bundle.fromStream (Stream.zipWith5 (uncurry f) (Stream.indexed (sElems as')) (sElems bs') (sElems cs') (sElems ds') (sElems es')) sz
  where
    as' = G.stream as
    bs' = G.stream bs
    cs' = G.stream cs
    ds' = G.stream ds
    es' = G.stream es
    sz  = smallest [sSize as', sSize bs', sSize cs', sSize ds', sSize es']
#else
izipWith5 f as bs cs ds es = G.unstream (Stream.zipWith5 (uncurry f) (Stream.indexed (G.stream as)) (G.stream bs) (G.stream cs) (G.stream ds) (G.stream es))
#endif

izipWith6 :: (G.Vector v0 a, G.Vector v1 b, G.Vector v2 c, G.Vector v3 d,
              G.Vector v4 e, G.Vector v5 f, G.Vector v6 g)
          => (Int -> a -> b -> c -> d -> e -> f -> g) -> v0 a -> v1 b -> v2 c
          -> v3 d -> v4 e -> v5 f -> v6 g
{-# INLINE izipWith6 #-}
#if MIN_VERSION_vector(0,11,0)
izipWith6 f as bs cs ds es fs = G.unstream $ Bundle.fromStream (Stream.zipWith6 (uncurry f) (Stream.indexed (sElems as')) (sElems bs') (sElems cs') (sElems ds') (sElems es') (sElems fs')) sz
  where
    as' = G.stream as
    bs' = G.stream bs
    cs' = G.stream cs
    ds' = G.stream ds
    es' = G.stream es
    fs' = G.stream fs
    sz  = smallest [sSize as', sSize bs', sSize cs', sSize ds', sSize es', sSize fs']
#else
izipWith6 f as bs cs ds es fs = G.unstream (Stream.zipWith6 (uncurry f) (Stream.indexed (G.stream as)) (G.stream bs) (G.stream cs) (G.stream ds) (G.stream es) (G.stream fs))
#endif

{-# INLINE zipWithM_ #-}
#if MIN_VERSION_vector(0,11,0)
zipWithM_ f as bs = Stream.zipWithM_ f (sElems $ lift as') (sElems $ lift bs')
  where
    as' = G.stream as
    bs' = G.stream bs
#else
zipWithM_ f as bs = Stream.zipWithM_ f (G.stream as) (G.stream bs)
#endif

-- * Helpers
#if MIN_VERSION_vector(0,11,0)
smallest :: [Size] -> Size
smallest = foldl1 smaller
#endif
