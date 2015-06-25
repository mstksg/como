{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Como.ComoVec where

import Data.MonoTraversable
import qualified Data.Vector.Generic as V
import Data.Ix
import Data.MonoComonadStore
import Control.Applicative


data Boundary a = BConstant !a
                | BClamp
                | BNearestF !(a -> a)
                | BWrap

data IxVec i v a = IV { ivVec   :: !(v a)
                      , ivRange :: !(i, i)
                      } deriving (Functor, Show, Eq)

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
                 BConstant x -> let !y = f x   in BConstant y
                 BClamp      -> b
                 BNearestF g -> let !h = f . g in BNearestF h
                 BWrap       -> b
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
                              BConstant d -> d
                              BClamp      -> v V.! index r (clamper mn mx i)
                              BWrap       -> v V.! index r (wrapper mn mx i)
                              BNearestF f -> f (v V.! index r (clamper mn mx i))
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

peekCV
    :: V.Vector v a
    => Ix (i k)
    => Applicative i
    => Integral k
    => i k
    -> ComoVec (i k) v a b
    -> b
peekCV i (CV (IV v r@(mn, mx)) b o _)
    | inRange r i = o i $ v V.! index r i
    | otherwise   = o i $ case b i of
                            BConstant d -> d
                            BClamp      -> v V.! index r (clamper mn mx i)
                            BWrap       -> v V.! index r (wrapper mn mx i)
                            BNearestF f -> f (v V.! index r (clamper mn mx i))
{-# INLINE peekCV #-}

clamper
    :: Applicative i
      => Ord k
    => i k
    -> i k
    -> i k
    -> i k
clamper rmn rmx = go
  where
    go = liftA3 clamp rmn rmx
    clamp mn mx x | x > mx    = mx
                  | x < mn    = mn
                  | otherwise = x
    -- {-# INLINE clamp #-}
{-# INLINE clamper #-}

wrapper
    :: Applicative i
      => Integral k
    => i k
    -> i k
    -> i k
    -> i k
wrapper rmn rmx = go
  where
    go = liftA3 wrap rmn rmx
    wrap mn mx x = (x - mn) `mod` range + mn
      where
        range = mx - mn + 1
    -- {-# INLINE wrap #-}
{-# INLINE wrapper #-}

-- concretizeV = concretizeV' id id
concretizeV
    :: V.Vector v a
      => Ix (i k)
      => Integral k
      => Applicative i
    => ComoVec (i k) v a a
    -> ComoVec (i k) v a a
concretizeV vf = resizeCV (ivRange (cvVec vf)) vf
{-# INLINE concretizeV #-}

-- concretizeV' df nf vf = resizeCV' (ivRange (cvVec vf)) df nf vf
concretizeV'
    :: V.Vector v a
      => V.Vector v b
      => Ix (i k)
      => Integral k
      => Applicative i
    => (a -> b)
    -> ((a -> a) -> b -> b)
    -> ComoVec (i k) v a b
    -> ComoVec (i k) v b b
concretizeV' df nf vf = resizeCV' df nf (ivRange (cvVec vf)) vf
{-# INLINE concretizeV' #-}

-- resizeCV = resizeCV' id id
resizeCV
    :: V.Vector v a
      => Ix (i k)
      => Integral k
      => Applicative i
    => (i k, i k)
    -> ComoVec (i k) v a a
    -> ComoVec (i k) v a a
resizeCV r' vf@(CV iv b o i) = CV (IV v' r') b (\_ x -> x) i
  where
    v' = V.fromListN (rangeSize r')
       . map (`peekCV` vf)
       $ range r'
{-# INLINE resizeCV #-}

resizeCV'
    :: V.Vector v a
      => V.Vector v b
      => Ix (i k)
      => Integral k
      => Applicative i
    => (a -> b)
    -> ((a -> a) -> b -> b)
    -> (i k, i k)
    -> ComoVec (i k) v a b
    -> ComoVec (i k) v b b
resizeCV' df nf r' vf@(CV iv b o i) = CV (IV v' r') b' (\_ x -> x) i
  where
    b' i' = case b i' of
              BConstant d -> BConstant $ df d
              BClamp      -> BClamp
              BWrap       -> BWrap
              BNearestF f -> BNearestF $ nf f
    v' = V.fromListN (rangeSize r')
       . map (`peekCV` vf)
       $ range r'
{-# INLINE resizeCV' #-}

getIV
    :: V.Vector v a
      => V.Vector v b
      => Ix (i k)
      => Integral k
      => Applicative i
    => ComoVec (i k) v a b
    -> IxVec (i k) v b
getIV vf@(CV iv@(IV _ r) b o i) = IV v' r
  where
    v' = V.fromListN (rangeSize r)
       . map (`peekCV` vf)
       $ range r
{-# INLINE getIV #-}

getVec
    :: V.Vector v a
      => V.Vector v b
      => Ix (i k)
      => Integral k
      => Applicative i
    => ComoVec (i k) v a b
    -> v b
getVec = ivVec . getIV
{-# INLINE getVec #-}

extendC
    :: V.Vector v a
      => Ix (i k)
      => Integral k
      => Applicative i
      => MonoFunctor (v a)
      => Element (v a) ~ a
    => (ComoVec (i k) v a a -> a)
    -> ComoVec (i k) v a a
    -> ComoVec (i k) v a a
extendC f = concretizeV . oextend f
{-# INLINE extendC #-}

ckC
    :: V.Vector v a
      => Ix (i k)
      => Integral k
      => Applicative i
      => MonoFunctor (v a)
      => Element (v a) ~ a
    => (ComoVec (i k) v a a -> a)
    -> (ComoVec (i k) v a a -> a)
    -> ComoVec (i k) v a a
    -> a
ckC g f = g . extendC f
{-# INLINE ckC #-}

replicateCV :: V.Vector v a
              => Ix i
              => Num i
            => (i, i)
            -> a
            -> ComoVec i v a a
replicateCV r x = CV (IV (V.replicate (rangeSize r) x) r)
                     (const (BConstant x))
                     (\_ y -> y)
                     0

generateCV :: V.Vector v a
             => Ix i
             => Num i
            => (i, i)
            -> a
            -> (i -> a)
            -> ComoVec i v a a
generateCV r b f = CV (IV v r) (const (BConstant b)) (\_ y -> y) 0
  where
    v = V.fromListN (rangeSize r)
      . map f
      $ range r
