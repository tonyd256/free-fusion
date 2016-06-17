{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Freer.Exec
  ( Exec(..)
  , exec
  )where
  
import Control.Monad.Free

import Freer.Operators

foldF :: Functor f => (a -> b) -> (f b -> b) -> Free f a -> b
foldF pure imp (Pure x) = pure x
foldF pure imp (Free t) = imp (fmap (foldF pure imp) t)

exec :: Exec f m => Free f a -> m a
exec = foldF return execAlgebra

class (Functor f, Monad m) => Exec f m where
  execAlgebra :: f (m a) -> m a
  
instance (Exec f m, Exec g m) => Exec (f :+: g) m where
  execAlgebra (Inl e) = execAlgebra e
  execAlgebra (Inr e) = execAlgebra e