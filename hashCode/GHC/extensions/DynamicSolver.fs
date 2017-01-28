module GHC.Extensions.DynamicSolver

open System
open GHC.Extensions
open GHC.Extensions.Common
open System.Collections.Generic
open ExtCore.Collections

//-------------------------------------------------------------------------------------------------
// SOLVER

/// tries to find the state which minimize the evaluation function
/// a state will be developed in children states using developState (final states should return a Seq.empty)
let solve (initialState : 'State) (initialValue : 'Value) evaluateState developState =
  let mutable bestState = initialState
  let mutable bestValue = initialValue
  /// previously tested states (memoization)
  let knownStates = MutableSet.singleton initialState
  /// states and their evaluations
  let stateQueue = MPriorityQueue.singleton bestValue bestState
  /// explores all possible states
  let rec search () =
    match MPriorityQueue.popMin stateQueue with 
    | None -> () 
    | Some (value,state) -> 
      for newState in developState state do
        if not (MutableSet.contains newState knownStates) then 
           MutableSet.add newState knownStates
           let newValue = evaluateState newState
           if newValue < bestValue then 
              bestState <- newState
              bestValue <- newValue
           MPriorityQueue.push newValue newState stateQueue
      search () // TODO : one could add a timeout here
  bestState, bestValue

//-------------------------------------------------------------------------------------------------
// KNAPSACK

type Object = {weight : int ; price : int ; id : int}

type State = {weight : int ; price : int ; objects : Object list}

/// returns a list of object wich maximise the price while keeping sum(weight) <= maxWeight
/// solution using dynamic programming
/// might explode if maxWeight gets too big
let knapsackDyn maxWeight (objects : Object list) =
  let mutable solution = Array.create (maxWeight+1) {weight=0 ; price=0 ; objects=[]} 
  let mutable previousSolution = {weight=0 ; price=0 ; objects=[]}
  for ob in objects do 
    previousSolution <- solution.[ob.weight-1]
    for i = ob.weight to maxWeight do 
       let newWeight = previousSolution.weight + ob.weight
       let newPrice = previousSolution.price + ob.price
       if (newPrice > solution.[i].price) && (newWeight <= i) then 
          let newSolution = {weight = newWeight ; price = newPrice ; objects = ob :: previousSolution.objects}
          previousSolution <- solution.[i]
          solution.[i] <- newSolution
       else previousSolution <- solution.[i]
  solution.[maxWeight]
(*
/// objects : (weight*price) list
let knapsackDij maxWeight objects =
  let mutable bestState = {weight=0 ; price=0 ; objectNumber=0 ; objects=[] ; left=objects}
  /// previously visited states (memoization)
  let knownStates = Dictionary()
  /// states and their current evaluations
  let stateQueue = MaxMPriorityQueue.singleton 0 bestState
  /// explores all possible states
  let rec search () =
    match MaxMPriorityQueue.popMax stateQueue with 
    | None -> () 
    | Some (price,state) when Dict.containsKey (state.weight,state.objectNumber) knownStates -> ()
    | Some (price,state) -> 
      for newState in develop maxWeight state state.left do
        if not (MutableSet.contains newState knownStates) then 
           MutableSet.add newState knownStates
           if newState.price > bestState.price then 
              bestState <- newState
           MaxMPriorityQueue.push newState.price newState stateQueue
      search () // TODO : one could add a timeout here
  bestState
*)
// a state is a number of objects and a weight 
// the state with the best price is kept 
