{-# OPTIONS_GHC -O -ddump-simpl-stats #-}

{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.Free
import Control.Monad.Codensity
import Control.Monad (join)
  
data (+) f g a where
  Inl :: f a -> (f + g) a
  Inr :: g a -> (f + g) a
  
instance (Functor f, Functor g) => Functor (f + g) where
  fmap f (Inl s) = Inl (fmap f s)
  fmap f (Inr s) = Inr (fmap f s)
  
data State s k where
  Put :: s -> k -> State s k
  Get :: (s -> k) -> State s k
  
instance Functor (State s) where
  fmap f (Put s k) = Put s (f k)
  fmap f (Get k) = Get (f . k)
  
class Functor f => TermAlgebra h f | h -> f where
  var :: forall a. a -> h a
  con :: forall a. f (h a) -> h a
  
instance Functor f => TermAlgebra (Free f) f where
  var = Pure
  con = Free
  
instance TermAlgebra IO IO where
  var = return
  con m = join m
  
class (Monad m, TermAlgebra m f) => TermMonad m f | m -> f
instance (Monad m, TermAlgebra m f) => TermMonad m f

instance Functor f => TermMonad (Free f) f

instance TermAlgebra h f => TermAlgebra (Codensity h) f where
  var = return
  con = algCod con
  
algCod :: Functor f => (forall x. f (h x) -> h x) -> (f (Codensity h a) -> Codensity h a)
algCod alg op = Codensity (\k -> alg (fmap (\m -> runCodensity m k) op)) 

--instance TermAlgebra h f => TermMonad (Codensity h) f

newtype StateCarrier s m a = SC { unSC :: s -> m a }
  
instance Functor m => Functor (StateCarrier s m) where
  fmap f x = SC (fmap (fmap f) (unSC x))
  
conState :: TermAlgebra m f => f (s -> m a) -> (s -> m a)
conState op s = con (fmap (\m -> m s) op)

(∇) :: (f b -> b) -> (g b -> b) -> ((f + g) b -> b)
(∇) algf algg (Inl s) = algf s
(∇) algf algg (Inr s) = algg s

instance TermMonad m f => TermAlgebra (StateCarrier s m) (State s + f) where
  var = SC . genState
  con = SC . (algState ∇ conState) . fmap unSC

genState :: TermMonad m f => a -> (s -> m a)
genState x = const (var x)

algState :: TermMonad m f => State s (s -> m a) -> (s -> m a)
algState (Put s' k) s = k s'
algState (Get k) s = k s s

runCod :: (a -> f x) -> Codensity f a -> f x
runCod g m = runCodensity m g

runStateC :: TermMonad m f => Codensity (StateCarrier s m) a -> (s -> m a)
runStateC = unSC . runCod var



get :: State s s
get = undefined

put :: s -> State s (s -> m s)
put s = undefined

--instance Monad (State s) where
--  return x = Put s x
--  (State _ k) >>= f = f k
--
--count :: State Int Int
--count = do
--  i <- get
--  return i



--instance TermAlgebra IO ()

--newtype IOCarrier m a = IOC { unIO :: m a }
--
--conIO :: TermAlgebra m f => f (m a) -> m a
--conIO op = con op
--
--genIO :: TermMonad m f => a -> m a
--genIO = var
--
----instance TermMonad m f => TermAlgebra IO a where
----  var = return
--
--algIO :: TermMonad m f => IO (m a) -> m a
--algIO op = undefined
--
--instance TermMonad m f => TermAlgebra (IOCarrier m) (IO + f) where
--  var = IOC . genIO
--  con = IOC . (algIO ∇ conIO) . fmap unIO
--
--runIOC :: TermMonad m f => Codensity (IOCarrier m) a -> m a
--runIOC = unIO . runCod var



















