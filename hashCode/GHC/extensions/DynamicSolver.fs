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

/// weight should be minimised and price maximised
type Object = {weight : int ; price : int ; id : int}
type State = {weight : int ; price : int ; objects : Object list}
let emptyState = {weight=0 ; price=0 ; objects=[]} 

/// returns a list of object wich maximise the price while keeping sum(weight) <= maxWeight
/// solution using dynamic programming
/// might explode if maxWeight gets too big
let knapsackDyn maxWeight (objects : Object list) =
  let mutable previousSolution = emptyState
  let solution = Array.create (maxWeight+1) emptyState 
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

/// variation using graphs, uses only the useful weights (but not all of them unlike the classical dynamic version)
/// similar because for each object, for each state, it tries to 1-add it to the state, 2-not add it
/// a state represent the best combinaison for a given weight
let knapsackDij maxWeight (objects : Object list) =
  let mutable states = Map.singleton 0 emptyState
  /// takes a state, add an object and update the states if it is an amelioration (better price for the same weight) + legal
  let addObject (ob:Object) oldStates w st = 
    let newSt = {weight=st.weight+ob.weight; price=st.price+ob.price; objects= ob :: st.objects}
    if newSt.weight > maxWeight then oldStates else 
      match Map.tryFind newSt.weight oldStates with 
      | Some oldSt when oldSt.price >= newSt.price -> oldStates
      | _ -> Map.add newSt.weight newSt oldStates
  /// update the states by trying to add the object to all the states, one by one
  for ob in objects do 
    states <- Map.fold (addObject ob) states states
  // maxBy price
  Map.fold (fun acc _ st -> if st.price>acc.price then st else acc) emptyState states
(*
/// variation using graphs, uses only the useful weights (and not all of them like the dynamic version)
let knapsackDij maxWeight (objects : Object list) =
  let states = dict [0, emptyState]
  /// takes a state, add an object and update the states if it is an amelioration (better price for the same weight) + legal
  let addObject (ob:Object) w st = 
    let newSt = {weight=st.weight+ob.weight; price=st.price+ob.price; objects= ob :: st.objects}
    if newSt.weight <= maxWeight then 
      match Dict.tryFind newSt.weight states with 
      | Some oldSt when oldSt.price >= newSt.price -> ()
      | _ -> states.[newSt.weight] <- newSt
  /// add the object to all the states, one by one (possible conflict : add object should not add before the end of iter ?)
  for ob in objects do Dict.iter (addObject ob) states
  // maxBy price
  Dict.fold (fun acc _ st -> if st.price>acc.price then st else acc) emptyState states
*)

