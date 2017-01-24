module GHC.Extensions.DynamicSolver

open System
open GHC.Extensions

//-------------------------------------------------------------------------------------------------
// 

let solve (initialState : 'State) (evalueState : 'State -> 'Value) (developState : 'State -> 'State list) =
  // let state = getState Queue
  // let states = developState state |> filter knownStates
  // for state in states :
    // addState state knownStates
    // if isFinal state then compareWithBest state
    // else addState (evaluation state) state queue
    ()




