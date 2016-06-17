{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Freer.Internal
  ( inject
  ) where
  
import Control.Monad.Free
  
import Freer.Operators

inject :: (g :<: f) => g (Free f a) -> Free f a
inject = Free . inj

instance Functor f => f :<: f where
  inj = id
  
instance (Functor f, Functor g) => f :<: (f :+: g) where
  inj = Inl
  
instance (Functor f, Functor g, Functor h) => f :<: (f :+: g :+: h) where
  inj = Inl . inj
  
instance (Functor f, Functor g, Functor h) => g :<: (f :+: g :+: h) where
  inj = Inl . inj
  
instance (Functor f, Functor g, Functor h, Functor i) => f :<: (f :+: g :+: h :+: i) where
  inj = Inl . inj

instance (Functor f, Functor g, Functor h, Functor i) => g :<: (f :+: g :+: h :+: i) where
  inj = Inl . inj

--instance (Functor f, Functor g, Functor h, Functor i) => h :<: (f :+: g :+: h :+: i) where
--  inj = Inl . inj

instance {-# OVERLAPS #-} (Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
  inj = Inr . inj
  
--instance {-# OVERLAPS #-} (Functor f, Functor g, Functor h, Functor i, f :<: i) => f :<: (h :+: g :+: i) where
--  inj = Inr . inj
  
--instance {-# OVERLAPS #-} (Functor f, Functor g, Functor h, Functor i, Functor j, f :<: j) => f :<: (h :+: g :+: i :+: j) where
--  inj = Inr . inj