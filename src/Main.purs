module Main where

import Prelude

import Data.Exists (Exists, mkExists)
import Effect (Effect)
import Effect.Console (log)

newtype CallSubroutineK' a b = CallSubroutineK' { subroutine :: Program1 b, callback :: (b -> Program1 a) }

data Program1 a = 
  Finish a
  | ReadValue Unit
  | WriteValue Int
  | CallSubroutineK (Exists (CallSubroutineK' a))

doubleMemoryValue :: Program1 Unit
doubleMemoryValue = CallSubroutineK (mkExists $ CallSubroutineK' {
  subroutine: ReadValue unit,
  callback: (\initialValue ->
    CallSubroutineK (mkExists $ CallSubroutineK' {
      subroutine: WriteValue initialValue,
      callback: (\_writeResult ->
        Finish unit
      )
    })
  )
})

fourFoldMemoryValue :: Program1 Int
fourFoldMemoryValue = CallSubroutineK (mkExists $ CallSubroutineK' {
  subroutine: doubleMemoryValue,
  callback: (\_res1 ->
    CallSubroutineK (mkExists $ CallSubroutineK' {
      subroutine: doubleMemoryValue,
      callback: (\_res2 ->
        CallSubroutineK (mkExists $ CallSubroutineK' {
          subroutine: ReadValue unit,
          callback: \finalValue -> Finish finalValue
        })
      )
    })
  )
})

main :: Effect Unit
main = do
  let p = doubleMemoryValue
  log "🍝"
