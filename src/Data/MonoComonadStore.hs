{-# LANGUAGE DefaultSignatures #-}      -- TODO: 7.6 support
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Data.MonoComonadStore where

import Data.MonoTraversable
import Control.Comonad.Store
import Data.ComonadRelaStore


class MonoComonad mono => MonoComonadRelaStore s mono | mono -> s where
    orelapeek :: s -> mono -> Element mono
    orelaseek :: s -> mono -> mono

    default orelapeek :: (ComonadRelaStore s w, w a ~ mono, Element (w a) ~ a)
                 => s -> mono -> Element mono
    orelapeek = relapeek
    {-# INLINE orelapeek #-}
    default orelaseek :: (ComonadRelaStore s w, w a ~ mono)
                 => s -> mono -> mono
    orelaseek = relaseek
    {-# INLINE orelaseek #-}

class MonoComonad mono => MonoComonadStore s mono | mono -> s where
    opos        :: mono -> s

    opeek       :: s -> mono -> Element mono
    opeek      = opeeks . const
    opeeks      :: (s -> s) -> mono -> Element mono
    opeeks f w = opeek (f (opos w)) w

    oseek       :: s -> mono -> mono
    oseek      = oseeks . const
    oseeks      :: (s -> s) -> mono -> mono
    oseeks f w = oseek (f (opos w)) w

    oexperiment :: Functor f => (s -> f s) -> mono -> f (Element mono)
    oexperiment f w = fmap (`opeek` w) (f (opos w))

    {-# MINIMAL opos, (opeek | opeeks), (oseek | oseeks) #-}

    default opos :: (ComonadStore s w, w a ~ mono, Element (w a) ~ a)
                 => mono -> s
    opos = pos
    {-# INLINE opos #-}


