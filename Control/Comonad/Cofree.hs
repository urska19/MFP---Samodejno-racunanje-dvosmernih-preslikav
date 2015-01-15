{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Comonad.Cofree
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  rank-2 types 
--
-- Examples: 
-- type LV = Cofree Maybe
-- type Stream = Cofree Identity

----------------------------------------------------------------------------
module Control.Comonad.Cofree 
	( Cofree
	, runCofree, cofree
	, ComonadCofree(outCofree)
	, RunComonadCofree(anaCofree)
	) where

import Control.Arrow ((&&&))
import Control.Comonad
import Control.Functor.Fix
import Control.Functor.Combinators.Biff
import Control.Monad.Identity
import Control.Comonad.Reader

type Cofree f = Fix (PCofree f)

runCofree :: Cofree f a -> (a, f (Cofree f a))
runCofree = runPCofree . outB

cofree :: a -> f (Cofree f a) -> Cofree f a 
cofree a as = InB $ Biff (Identity a,as)

class (Functor f, Comonad w) => ComonadCofree f w | w -> f where
        outCofree :: w a -> f (w a)

instance Functor f => ComonadCofree f (Cofree f) where
        outCofree = snd . runCofree

instance ComonadCofree f w => ComonadCofree f (CoreaderT w e) where
	outCofree = fmap CoreaderT . outCofree . runCoreaderT

class ComonadCofree f w => RunComonadCofree f w | w -> f where
	anaCofree :: Functor f => (a -> c) -> (a -> f a) -> a -> w c

instance Functor f => RunComonadCofree f (Cofree f) where
	anaCofree h t = InB . Biff . (Identity . h &&& fmap (anaCofree h t) . t)
