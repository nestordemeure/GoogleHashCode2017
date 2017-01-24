module GHC.Extensions.DynamicSolver

open System
open GHC.Extensions
open GHC.Extensions.Common

//-------------------------------------------------------------------------------------------------
// SOLVER

/// tries to find the state which minimize the evaluation function
/// a state will be developed in children states using developState (final states should return a Seq.empty)
let inline solve (initialState : 'State) (initialValue : 'Value) evaluateState developState =
  let mutable bestState = initialState
  let mutable bestValue = initialValue
  /// previously tested states (memoization)
  let knownStates = HashSet.singleton initialState
  /// states and their evaluations
  let stateQueue = MPriorityQueue.empty |-> MPriorityQueue.push bestValue bestState
  /// explores all possible states
  let rec search () =
    match MPriorityQueue.popMin stateQueue with 
    | None -> () 
    | Some (value,state) -> 
      for newState in developState state do
        if not (HashSet.contains newState knownStates) then 
           HashSet.add newState knownStates |> ignore // TODO : is it truly mutable ?
           let newValue = evaluateState newState
           if newValue < bestValue then 
              bestState <- newState
              bestValue <- newValue
           MPriorityQueue.push newValue newState stateQueue
      search () // TODO : one could add a timeout here
  bestState, bestValue




