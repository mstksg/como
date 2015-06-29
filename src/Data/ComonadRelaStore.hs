{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.ComonadRelaStore where

import Control.Comonad

class Comonad w => ComonadRelaStore s w | w -> s where
    relapeek :: s -> w a -> a
    relaseek :: s -> w a -> w a

