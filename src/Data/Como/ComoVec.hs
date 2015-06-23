{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Como.ComoVec where

import Data.MonoTraversable
import qualified Data.Vector.Generic as V
import Data.Ix
import Data.MonoComonadStore
import Control.Applicative


data Boundary a = Dirichlet !a
                | Neumann0
                | BNearest !(a -> a)
                | Periodic

data IxVec i v a = IV { ivVec   :: !(v a)
                      , ivRange :: !(i, i)
                      } deriving Functor

data ComoVec i v a b = CV { cvVec      :: !(IxVec i v a)
                          , cvBoundary :: i -> Boundary a
                          , cvOut      :: !(i -> a -> b)
                          , cvFocus    :: !i
                          }

type instance Element (Boundary a) = a
type instance Element (IxVec i v a) = a
type instance Element (ComoVec i v a b) = b

instance Functor v => Functor (ComoVec i v a) where
    fmap f (CV v b o i) = CV v b (\i' -> f . o i') i

instance MonoFunctor (Boundary a) where
    omap f b = case b of
                 Dirichlet x -> let !y = f x   in Dirichlet y
                 Neumann0    -> b
                 BNearest g  -> let !h = f . g in BNearest h
                 Periodic    -> b
    {-# INLINE omap #-}

instance (MonoFunctor (v a), Element (v a) ~ a) => MonoFunctor (IxVec i v a) where
    omap f (IV v r) = IV (omap f v) r
    {-# INLINE omap #-}

instance (MonoFunctor (v b), Element (v b) ~ b) => MonoFunctor (ComoVec i v a b) where
    omap f (CV v b o i) = CV v b (\i -> f . o i) i
    {-# INLINE omap #-}

instance (V.Vector v a, Ix (i k), Integral k, Applicative i, MonoFunctor (v b), Element (v b) ~ b) => MonoComonad (ComoVec (i k) v a b) where
    oextract (CV (IV v r@(mn, mx)) b o i)
      | inRange r i = o i $ v V.! index r i
      | otherwise   = o i $ case b i of
                              Dirichlet d -> d
                              Neumann0    -> v V.! index r (clamper mn mx i)
                              Periodic    -> v V.! index r (wrapper mn mx i)
                              BNearest f  -> f (v V.! index r (clamper mn mx i))
    {-# INLINE oextract #-}
    oextend f (CV iv b o i) = CV iv b (\i' _ -> f (CV iv b o i')) i
    {-# INLINE oextend #-}

instance (V.Vector v a, Ix (i k), MonoFunctor (v b), Element (v b) ~ b, Applicative i, Integral k) => MonoComonadStore (i k) (ComoVec (i k) v a b) where
    opos (CV _ _ _ i) = i
    {-# INLINE opos #-}
    opeek = peekCV
    {-# INLINE opeek #-}
    opeeks f cv     = peekCV (f (cvFocus cv)) cv
    {-# INLINE opeeks #-}
    oseek i cv      = cv { cvFocus = i              }
    {-# INLINE oseek #-}
    oseeks f cv     = cv { cvFocus = f (cvFocus cv) }
    {-# INLINE oseeks #-}

instance (V.Vector v a, Ix (i k), Applicative i, MonoFunctor (v b), Element (v b) ~ b, Num (i k), Integral k) => MonoComonadRelaStore (i k) (ComoVec (i k) v a b) where
    orelapeek i = opeeks (+ i)
    {-# INLINE orelapeek #-}
    orelaseek i = oseeks (+ i)
    {-# INLINE orelaseek #-}

peekCV :: (V.Vector v a, Ix (i k), Applicative i, Integral k) => i k -> ComoVec (i k) v a b -> b
peekCV i (CV (IV v r@(mn, mx)) b o _)
    | inRange r i = o i $ v V.! index r i
    | otherwise   = o i $ case b i of
                            Dirichlet d -> d
                            Neumann0    -> v V.! index r (clamper mn mx i)
                            Periodic    -> v V.! index r (wrapper mn mx i)
                            BNearest f  -> f (v V.! index r (clamper mn mx i))
{-# INLINE peekCV #-}

clamper rmn rmx = go
  where
    go = liftA3 clamp rmn rmx
    clamp mn mx x | x > mx    = mx
                  | x < mn    = mn
                  | otherwise = x
    -- {-# INLINE clamp #-}
{-# INLINE clamper #-}

wrapper rmn rmx = go
  where
    go = liftA3 wrap rmn rmx
    wrap mn mx x = (x - mn) `mod` range + mn
      where
        range = mx - mn + 1
    -- {-# INLINE wrap #-}
{-# INLINE wrapper #-}

concretizeV :: (V.Vector v a, Ix (i k), Integral k, Applicative i) => ComoVec (i k) v a a -> ComoVec (i k) v a a
concretizeV vf@(CV iv@(IV _ r) b o i) = CV (IV v' r) b (\_ x -> x) i
  where
    v' = V.fromListN (rangeSize r)
       . map (`peekCV` vf)
       $ range r
{-# INLINE concretizeV #-}

concretizeV' :: (V.Vector v a, V.Vector v b, Ix (i k), Integral k, Applicative i) => (a -> b) -> ((a -> a) -> b -> b) -> ComoVec (i k) v a b -> ComoVec (i k) v b b
concretizeV' df nf vf@(CV iv@(IV _ r) b o i) = CV (IV v' r) b' (\_ x -> x) i
  where
    b' i' = case b i' of
              Dirichlet d -> Dirichlet $ df d
              Neumann0    -> Neumann0
              Periodic    -> Periodic
              BNearest f  -> BNearest $ nf f
    v' = V.fromListN (rangeSize r)
       . map (`peekCV` vf)
       $ range r
{-# INLINE concretizeV' #-}

resizeCV :: (V.Vector v a, Ix (i k), Integral k, Applicative i) => (i k, i k) -> ComoVec (i k) v a a -> ComoVec (i k) v a a
resizeCV r' vf@(CV iv b o i) = CV (IV v' r') b (\_ x -> x) i
  where
    v' = V.fromListN (rangeSize r')
       . map (`peekCV` vf)
       $ range r'
{-# INLINE resizeCV #-}

resizeCV' :: (V.Vector v a, V.Vector v b, Ix (i k), Integral k, Applicative i) => (i k, i k) -> (a -> b) -> ((a -> a) -> b -> b) -> ComoVec (i k) v a b -> ComoVec (i k) v b b
resizeCV' r' df nf vf@(CV iv b o i) = CV (IV v' r') b' (\_ x -> x) i
  where
    b' i' = case b i' of
              Dirichlet d -> Dirichlet $ df d
              Neumann0    -> Neumann0
              Periodic    -> Periodic
              BNearest f  -> BNearest $ nf f
    v' = V.fromListN (rangeSize r')
       . map (`peekCV` vf)
       $ range r'
{-# INLINE resizeCV' #-}

