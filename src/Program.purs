module Program 
  ( Program(..)
  , SingleMemoryInstruction(..)
)where

import Prelude

import Control.Bind (bindFlipped)

data SingleMemoryInstruction next =
  Read (Int -> Program next)
  | Write Int (Unit -> Program next)

instance functorSingleMemoryInstruction :: Functor SingleMemoryInstruction where
  map :: forall a b. (a -> b) -> SingleMemoryInstruction a -> SingleMemoryInstruction b
  map f inst = case inst of
    Read k -> Read $ k >>> (map f)
    Write v k -> Write v (k >>> (map f))

mapF :: forall a b. (Program a -> Program b) -> SingleMemoryInstruction a -> SingleMemoryInstruction b
mapF f inst = case inst of
  Read k -> Read $ k >>> f
  Write v k -> Write v (k >>> f)

-- data Program :: (Type -> Type) -> Type -> Type
data Program next =
  Finish next
  | Operation (SingleMemoryInstruction next)

instance functorProgram :: Functor Program where
  map f program = case program of
    Finish next -> Finish $ f next
    Operation op -> Operation $ map f op

instance applyProgram :: Apply Program where
  apply = ap

instance bindProgram :: Bind Program where
  bind program f = case program of
    Finish next -> f next
    Operation op -> Operation $ mapF (bindFlipped f) op
    -- bindFlipped f :: forall a b. Program a -> Program b
    -- Read k -> Read $ \v -> k >>> (bindFlipped f) $ v
    --           Read $ k >>> (bindFlipped f)
    -- Write v k -> Write v (\_ -> bind (k unit) f)

instance applicativeProgram :: Applicative Program where
  pure = Finish

instance monadProgram :: Monad Program

-- newtype CallSubroutineK' :: (Type -> Type) -> Type -> Type -> Type
-- newtype CallSubroutineK' inst a b = CallSubroutineK' 

-- makeSubroutine :: forall inst a b. { subroutine :: Program inst b, callback :: (b -> Program inst a) } -> Program inst a
-- makeSubroutine record = CallSubroutineK $ mkExists $ CallSubroutineK' record
