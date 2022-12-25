module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Program (Program(..), SingleMemoryInstruction(..))

doubleMemoryValue :: Program Int
doubleMemoryValue = do
  value <- Operation $ Read pure
  _ <- Operation $ Write (value * 2) pure
  value2 <- Operation $ Read pure
  pure value2

d =
  Operation (Read pure) >>= \value2 ->
  Operation (Write (value2 * 2) pure) >>= \_ ->
  pure unit

-- e1 = Operation (Read pure) >>= pure
-- e2 = Opeation $ mapF (bindFlipped pure) (Read pure)
-- e3 = Operation $ Read $ pure >>> (bindFlipped pure)

interpretSingleMemoryInstruction :: forall a. SingleMemoryInstruction a -> Program a
interpretSingleMemoryInstruction inst = case inst of
  Read k -> k 1
  Write v k -> k unit

interpret :: forall a. (SingleMemoryInstruction a -> Program a) -> Program a -> a
interpret interpretOp program = case program of
  Finish v -> v
  Operation op -> interpretOp >>> (interpret interpretOp) $ op

main :: Effect Unit
main = do
  let p = doubleMemoryValue
  let r = interpret interpretSingleMemoryInstruction p
  log $ show r
