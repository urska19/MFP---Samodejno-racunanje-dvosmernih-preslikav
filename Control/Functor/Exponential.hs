--{-# OPTIONS_GHC -fglasgow-exts #-}
-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Functor.Exponential
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: non-portable (class-associated types)
--
-- Exponential functors, see <http://comonad.com/reader/2008/rotten-bananas/>
-------------------------------------------------------------------------------------------

module Control.Functor.Exponential 
	( ExpFunctor(xmap)
	) where

import Control.Applicative (Const(..))

class ExpFunctor f where
	xmap :: (a -> b) -> (b -> a) -> f a -> f b

instance ExpFunctor (Const a) where
        xmap _ _ (Const a) = Const a
