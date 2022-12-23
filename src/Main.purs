module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Program (Program(..), makeSubroutine)

data SingleMemoryInstruction :: Type -> Type
data SingleMemoryInstruction r =
  ReadValue Unit
  | WriteValue Int

doubleMemoryValue :: Program SingleMemoryInstruction Unit
doubleMemoryValue = makeSubroutine {
  subroutine: RunInstruction $ ReadValue unit,
  callback: \initialValue -> makeSubroutine {
    subroutine: RunInstruction $ WriteValue initialValue,
    callback: \_writeResult -> Finish unit
  }
}

fourFoldMemoryValue :: Program SingleMemoryInstruction Int
fourFoldMemoryValue = makeSubroutine {
  subroutine: doubleMemoryValue,
  callback: \_res1 -> makeSubroutine {
    subroutine: doubleMemoryValue,
    callback: \_res2 -> makeSubroutine {
      subroutine: RunInstruction (ReadValue unit),
      callback: \finalValue -> Finish finalValue
    }
  }
}

useIf :: Program SingleMemoryInstruction Unit
useIf = makeSubroutine {
  subroutine: RunInstruction (ReadValue unit),
  callback: \value -> makeSubroutine {
    subroutine: (
      if value `mod` 2 == 0 then
        RunInstruction (WriteValue 0)
      else
        RunInstruction (WriteValue 1)
    ),
    callback: \_res -> Finish unit
  }
}

main :: Effect Unit
main = do
  let p = doubleMemoryValue
  log "🍝"
