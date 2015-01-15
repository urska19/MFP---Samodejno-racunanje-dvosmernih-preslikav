{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Morphism.Chrono
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (rank-2 polymorphism)
-- 
-- Chronomorphisms from <http://comonad.com/reader/2008/time-for-chronomorphisms/>
----------------------------------------------------------------------------
module Control.Morphism.Chrono where

import Control.Comonad.Cofree
import Control.Functor.Algebra
import Control.Functor.Extras
import Control.Monad.Free
import Control.Morphism.Hylo
import Control.Morphism.Futu
import Control.Morphism.Histo

chrono :: (RunMonadFree f m, RunComonadCofree g w) => GAlgebra g w b -> (f :~> g) -> GCoalgebra f m a -> a -> b
chrono = g_hylo (distHisto id) (distFutu id)

g_chrono :: (Functor f, Functor g, RunComonadCofree h w, RunMonadFree j m) => 
	    Dist g h -> Dist j f -> GAlgebra g w b -> (f :~> g) -> GCoalgebra f m a -> a -> b
g_chrono h f = g_hylo (distHisto h) (distFutu f)
