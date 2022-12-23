module Program 
  ( Program(..)
  , CallSubroutineK'
  , makeSubroutine
)where

import Prelude

import Data.Exists (Exists, mkExists)

data Program :: (Type -> Type) -> Type -> Type
data Program inst a =
  Finish a
  | RunInstruction (inst a)
  | CallSubroutineK (Exists (CallSubroutineK' inst a))

newtype CallSubroutineK' :: (Type -> Type) -> Type -> Type -> Type
newtype CallSubroutineK' inst a b = CallSubroutineK' { subroutine :: Program inst b, callback :: (b -> Program inst a) }

makeSubroutine :: forall inst a b. { subroutine :: Program inst b, callback :: (b -> Program inst a) } -> Program inst a
makeSubroutine record = CallSubroutineK $ mkExists $ CallSubroutineK' record
