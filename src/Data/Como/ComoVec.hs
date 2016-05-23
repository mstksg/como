{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- TODO: offer custom/parameterizable boundary conditions?
-- can solve ugliness of concretizeV' and weird conretizing getters
-- requiring matching source and destiniation types.

module Data.Como.ComoVec where

import Data.MonoTraversable
import qualified Data.Vector.Generic as V
import Data.Functor.Contravariant
import Data.Ix
import Control.Comonad
import Control.Comonad.Store.Class
import Data.MonoComonadStore
import Data.ComonadRelaStore
import Control.Applicative

data Boundary a = BConstant !a
                | BClamp
                | BNearestF !(a -> a)
                | BWrap

data IxVec i v a = IV { _ivVec   :: !(v a)
                      , _ivRange :: !(i, i)
                      } deriving (Functor, Show, Eq)

data ComoVec i v a b = CV { _cvIxVec    :: !(IxVec i v a)
                          , _cvBoundary :: i -> Boundary a
                          , _cvOut      :: !(i -> a -> b)
                          , _cvFocus    :: !i
                          }

type instance Element (Boundary a) = a
type instance Element (IxVec i v a) = a
type instance Element (ComoVec i v a b) = b

-- instance (Applicative v, Num k, Applicative i) => Applicative (IxVec (i k) v) where
--     pure x = IV (pure x) (pure 0, pure 0)
--     IV vf rf <*> IV vx rx = IV undefined undefined

instance Functor (ComoVec i v a) where
    fmap f (CV v b o i) = CV v b (\i' -> f . o i') i

-- instance (Applicative v, Num k, Applicative i) => Applicative (ComoVec (i k) v a) where
--     pure x = CV (IV (pure undefined) (pure 0, pure 0))
--                 (\_ -> BConstant undefined)         -- undefined is no good, will fail on strict vectors.  maybe require monoid?
--                 (\_ _ -> x)
--                 (pure 0)
--     cvf@(CV (IV vf (mnf, mxf)) bf of_ if_) <*> cvx@(CV (IV vx (mnx, mxx)) bx ox ix) = CV iv' undefined undefined undefined
--       where
--         iv' = IV v' r'
--         r'  = (liftA2 min mnf mnx, liftA2 max mxf mxx)
--         v' = V.fromListN (rangeSize r')
--            . map (\i -> peekCV i cvf $ peekCV i cvx)
--            $ range r'

-- HEY consider maybe including indexing functions?  that would make this
-- much easier.  hm.

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
    omap f (CV v b o i) = CV v b (\i' -> f . o i') i
    {-# INLINE omap #-}

instance (V.Vector v a, Ix (i k), Integral k, Applicative i) => Comonad (ComoVec (i k) v a) where
    extract (CV (IV v r@(mn, mx)) b o i)
      | inRange r i = o i $ v V.! index r i
      | otherwise   = o i $ case b i of
                              BConstant d -> d
                              BClamp      -> v V.! index r (clamper mn mx i)
                              BWrap       -> v V.! index r (wrapper mn mx i)
                              BNearestF f -> f (v V.! index r (clamper mn mx i))
    {-# INLINE extract #-}
    extend f (CV iv b o i) = CV iv b (\i' _ -> f (CV iv b o i')) i
    {-# INLINE extend #-}

instance (V.Vector v a, Ix (i k), Integral k, Applicative i) => ComonadStore (i k) (ComoVec (i k) v a) where
    pos (CV _ _ _ i) = i
    {-# INLINE pos #-}
    peek = peekCV
    {-# INLINE peek #-}
    peeks f cv      = peekCV (f (_cvFocus cv)) cv
    {-# INLINE peeks #-}
    seek i cv       = cv { _cvFocus = i              }
    {-# INLINE seek #-}
    seeks f cv      = cv { _cvFocus = f (_cvFocus cv) }
    {-# INLINE seeks #-}

instance (V.Vector v a, Ix (i k), Integral k, Applicative i, Num (i k)) => ComonadRelaStore (i k) (ComoVec (i k) v a) where
    relapeek i = peeks (+ i)
    {-# INLINE relapeek #-}
    relaseek i = seeks (+ i)
    {-# INLINE relaseek #-}

instance (V.Vector v a, Ix (i k), Integral k, Applicative i, MonoFunctor (v b), Element (v b) ~ b) => MonoComonad (ComoVec (i k) v a b) where
    oextract = extract
    {-# INLINE oextract #-}
    oextend = extend
    {-# INLINE oextend #-}

instance (V.Vector v a, Ix (i k), MonoFunctor (v b), Element (v b) ~ b, Applicative i, Integral k) => MonoComonadStore (i k) (ComoVec (i k) v a b) where
    opos (CV _ _ _ i) = i
    {-# INLINE opos #-}
    opeek = peekCV
    {-# INLINE opeek #-}
    opeeks f cv     = peekCV (f (_cvFocus cv)) cv
    {-# INLINE opeeks #-}
    oseek i cv      = cv { _cvFocus = i              }
    {-# INLINE oseek #-}
    oseeks f cv     = cv { _cvFocus = f (_cvFocus cv) }
    {-# INLINE oseeks #-}

instance (V.Vector v a, Ix (i k), Applicative i, MonoFunctor (v b), Element (v b) ~ b, Num (i k), Integral k) => MonoComonadRelaStore (i k) (ComoVec (i k) v a b) where
    orelapeek = relapeek
    {-# INLINE orelapeek #-}
    orelaseek = relaseek
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
    wrap mn mx x = (x - mn) `mod` rng + mn
      where
        rng = mx - mn + 1
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
concretizeV vf = resizeCV (_ivRange (_cvIxVec vf)) vf
{-# INLINE concretizeV #-}

-- concretizeV' df nf vf = resizeCV' (_ivRange (_cvIxVec vf)) df nf vf
concretizeV'
    :: V.Vector v a
      => V.Vector v b
      => Ix (i k)
      => Integral k
      => Applicative i
    => (a -> b)                 -- ???      mapping dirichlet boundaries
    -> ((a -> a) -> b -> b)     -- ???      maps the neumann boundaries
    -> ComoVec (i k) v a b
    -> ComoVec (i k) v b b
concretizeV' df nf vf = resizeCV' df nf (_ivRange (_cvIxVec vf)) vf
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
resizeCV r' vf@(CV _ b _ i) = CV (IV v' r') b (\_ x -> x) i
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
resizeCV' df nf r' vf@(CV _ b _ i) = CV (IV v' r') b' (\_ x -> x) i
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

-- does concretize
getIV
    :: V.Vector v a
      => V.Vector v b
      => Ix (i k)
      => Integral k
      => Applicative i
    => ComoVec (i k) v a b
    -> IxVec (i k) v b
getIV vf@(CV (IV _ r) _ _ _) = IV v' r
  where
    v' = V.fromListN (rangeSize r)
       . map (`peekCV` vf)
       $ range r
{-# INLINE getIV #-}

-- does concretize
getVec
    :: V.Vector v a
      => V.Vector v b
      => Ix (i k)
      => Integral k
      => Applicative i
    => ComoVec (i k) v a b
    -> v b
getVec = _ivVec . getIV
{-# INLINE getVec #-}

concretizeGetV
    :: V.Vector v a
      => Ix (i k)
      => Integral k
      => Applicative i
    => ComoVec (i k) v a a
    -> (v a, ComoVec (i k) v a a)
concretizeGetV cv = (_ivVec (_cvIxVec conc), conc)
  where
    conc = concretizeV cv
{-# INLINE concretizeGetV #-}

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

replicateCV
    :: V.Vector v a
      => Ix i
      => Num i
    => (i, i)
    -> a
    -> ComoVec i v a a
replicateCV r x = CV (IV (V.replicate (rangeSize r) x) r)
                     (const (BConstant x))
                     (\_ y -> y)
                     0
{-# INLINE replicateCV #-}

generateCV
    :: V.Vector v a
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
{-# INLINE generateCV #-}

-- Lenses

-- ivVec :: Lens (IxVec i v a) (IxVec a v b) (v a) (v b)
ivVec
    :: Functor f
    => (v a -> f (v b))
    -> IxVec i v a
    -> f (IxVec i v b)
ivVec f iv@(IV v _) = (\v' -> iv { _ivVec = v' }) <$> f v
{-# INLINE ivVec #-}

-- ivRange :: Lens' (IxVec i v a) (i, i)
ivRange
    :: Functor f
    => ((i, i) -> f (i, i))
    -> IxVec i v a
    -> f (IxVec i v a)
ivRange f iv@(IV _ r) = (\r' -> iv { _ivRange = r' }) <$> f r
{-# INLINE ivRange #-}

-- cvIxVec :: Lens' (ComoVec i v a a) (IxVec i v a)
-- concretizes!
cvIxVec
    :: Functor f
      => V.Vector v a
      => Ix (i k)
      => Integral k
      => Applicative i
    => (IxVec (i k) v a -> f (IxVec (i k) v a))
    -> ComoVec (i k) v a a
    -> f (ComoVec (i k) v a a)
cvIxVec f cv = (\iv' -> cv { _cvIxVec = iv', _cvOut = \_ x -> x })
           <$> f (getIV cv)
{-# INLINE cvIxVec #-}

-- cvBoundary :: Lens' (ComoVec i v a b) (i -> Boundary a)
cvBoundary
    :: Functor f
    => ((i -> Boundary a) -> f (i -> Boundary a))
    -> ComoVec i v a b
    -> f (ComoVec i v a b)
cvBoundary f cv@(CV _ b _ _) = (\b' -> cv { _cvBoundary = b' }) <$> f b
{-# INLINE cvBoundary #-}

-- cvFocus :: Lens' (ComoVec i v a b) i
cvFocus
    :: Functor f
    => (i -> f i)
    -> ComoVec i v a b
    -> f (ComoVec i v a b)
cvFocus f cv@(CV _ _ _ i) = (\i' -> cv { _cvFocus = i' }) <$> f i
{-# INLINE cvFocus #-}

-- cvSourceIV :: Lens' (ComoVec i v a b) (IxVec i v a)
cvSourceIV
    :: Functor f
    => (IxVec i v a -> f (IxVec i v a))
    -> ComoVec i v a b
    -> f (ComoVec i v a b)
cvSourceIV f cv@(CV iv _ _ _) = (\iv' -> cv { _cvIxVec = iv' }) <$> f iv
{-# INLINE cvSourceIV #-}

-- cvSourceVec :: Lens' (ComoVec i v a b) (v a)
--
-- no concretization
cvSourceVec
    :: Functor f
    => (v a -> f (v a))
    -> ComoVec i v a b
    -> f (ComoVec i v a b)
cvSourceVec = cvSourceIV . ivVec

-- cvVec  :: Lens' (ComoVec i v a a) (v a)
-- concretizes!
cvVec
    :: Functor f
      => V.Vector v a
      => Ix (i k)
      => Integral k
      => Applicative i
    => (v a -> f (v a))
    -> ComoVec (i k) v a a
    -> f (ComoVec (i k) v a a)
cvVec = cvIxVec . ivVec
{-# INLINE cvVec #-}

-- TODO: purely a Getter for cvVec
-- x DONE
--
-- cvVec_ :: Getter (ComoVec i v a b) (v b)
-- concretizes!
--
-- We're allowed to get a 'v b' from a ComoVec of source 'v a' because we
-- concretize and forget about boundary conditions and other troublesome
-- aspects of concretizing to a different type.
--
-- TODO: typecheck with Getter from lens
-- x DONE
cvVec_
    :: Contravariant f
      => Functor f
      => V.Vector v a
      => V.Vector v b
      => Ix (i k)
      => Integral k
      => Applicative i
    => (v b -> f (v b))
    -> ComoVec (i k) v a b
    -> f (ComoVec (i k) v a b)
cvVec_ f cv = cv <$ f (getVec cv)

-- cvIxVec :: Getter (ComoVec i v a b) (IxVec i v b)
-- concretizes!
cvIxVec_
    :: Functor f
      => V.Vector v a
      => V.Vector v b
      => Ix (i k)
      => Integral k
      => Applicative i
    => (IxVec (i k) v b -> f (IxVec (i k) v b))
    -> ComoVec (i k) v a b
    -> f (ComoVec (i k) v a b)
cvIxVec_ f cv = cv <$ f (getIV cv)
{-# INLINE cvIxVec_ #-}
