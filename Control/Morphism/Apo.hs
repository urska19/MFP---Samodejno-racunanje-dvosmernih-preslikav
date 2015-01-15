{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Morphism.Apo
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (rank-2 polymorphism)
-- 
-- Traditional operators, shown here to show how to roll your own
----------------------------------------------------------------------------
module Control.Morphism.Apo 
	( apo, g_apo
	, postpro_apo, g_postpro_apo
	, Apo, ApoT
	, distApoT
	, GApo, GApoT
	, distGApo, distGApoT
	) where

import Control.Functor.Algebra
import Control.Functor.Extras
import Control.Functor.Fix
import Control.Monad
import Control.Monad.Either 
import Control.Morphism.Ana
import Control.Morphism.Postpro
import Control.Arrow ((|||))

-- * Unfold Sugar

apo :: Functor f => GCoalgebra f (Apo f) a -> a -> FixF f
apo = g_apo outF

g_apo :: Functor f => Coalgebra f b -> GCoalgebra f (GApo b) a -> a -> FixF f
g_apo g = g_ana (distGApo g)

postpro_apo :: Functor f => GCoalgebra f (Apo f) a -> (f :~> f) -> a -> FixF f
postpro_apo = g_postpro_apo outF

g_postpro_apo :: Functor f => Coalgebra f b -> GCoalgebra f (GApo b) a -> (f :~> f) -> a -> FixF f
g_postpro_apo g = g_postpro (distGApo g)

type Apo f a 		= Either (FixF f) a
type ApoT f m a 	= EitherT (FixF f) m a

type GApo b a 		= Either b a
type GApoT b m a 	= EitherT b m a 

-- * Distributive Law Combinators for apomorphisms
-- NB: we don't actually have simple recursion combinators for all of these 

distGApo :: Functor f => Coalgebra f b -> Dist (Either b) f
distGApo f = fmap Left . f  ||| fmap Right

distGApoT :: (Functor f, Monad m) => GCoalgebra f m b -> Dist m f -> Dist (EitherT b m) f
distGApoT g k = fmap (EitherT . join) . k  . liftM (fmap (liftM Left) . g ||| fmap (return . Right)) . runEitherT

distApoT :: (Functor f, Monad m) => Dist m f -> Dist (ApoT f m) f
distApoT = distGApoT (liftCoalgebra outF)
