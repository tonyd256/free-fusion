{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}

import Control.Monad.Free
import Data.Monoid
import Freer

data AppDSL a
  = GetState (String -> a)
  | SaveState String a
  | LogString String a
  deriving Functor

data DBDSL a
  = SelectList String ([String] -> a)
  | SaveList String [String] a
  deriving Functor
  
data AnotherDSL a
  = Something String ([String] -> a)
  | SomethingElse String [String] a
  deriving Functor
  
data OneMoreDSL a
  = Anything a
  deriving Functor
 
--getState :: (AppDSL :<: f) => Free f String
--saveState :: (AppDSL :<: f) => String -> Free f ()
getState = inject $ GetState Pure
saveState s = inject $ SaveState s $ Pure ()
logString s = inject $ LogString s $ Pure ()

selectList s = inject $ SelectList s Pure
saveList s list = inject $ SaveList s list $ Pure ()

something s = inject $ Something s Pure
somethingElse s xs = inject $ SomethingElse s xs $ Pure ()

anything = inject $ Anything $ Pure ()

type App = Free (AppDSL :+: DBDSL :+: AnotherDSL :+: OneMoreDSL)

runApp :: App ()
runApp = do
  anything
  saveList "" []
  somethingElse "" []
  s <- getState
  saveList "" []
  somethingElse s []
  saveList "" []
  anything
  saveList "" []
  somethingElse s []
  anything
  xs <- selectList "hello"
  anything
  logString $ s <> unlines xs
  
instance Exec AppDSL IO where
  execAlgebra (GetState f) = f "state"
  execAlgebra (SaveState _ a) = a
  execAlgebra (LogString s a) = print s >> a

instance Exec DBDSL IO where
  execAlgebra (SelectList s f) = f [s]
  execAlgebra (SaveList s xs a) = a 
  
instance Exec AnotherDSL IO where
  execAlgebra (Something s f) = f [s]
  execAlgebra (SomethingElse s xs a) = a
  
instance Exec OneMoreDSL IO where
  execAlgebra (Anything a) = a