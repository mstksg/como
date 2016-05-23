{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeFamilies      #-}

module Data.Vector.IxVec where

import           Data.MonoTraversable

data IxVec i v a = IV { _ivVec   :: !(v a)
                      , _ivRange :: !(i, i)
                      } deriving (Functor, Show, Eq, Foldable, Traversable)

type instance Element (IxVec i v a) = a

instance (MonoFunctor (v a), Element (v a) ~ a) => MonoFunctor (IxVec i v a) where
    omap f (IV v r) = IV (omap f v) r
    {-# INLINE omap #-}


-- instance (Applicative v, Num k, Applicative i) => Applicative (IxVec (i k) v) where
--     pure x = IV (pure x) (pure 0, pure 0)
--     IV vf rf <*> IV vx rx = IV undefined undefined

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
