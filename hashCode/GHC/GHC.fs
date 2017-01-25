module GHC.Main

open ExtCore.Collections

open GHC.Extensions
open GHC.Extensions.Common
open GHC.Domain
open GHC.Import
open GHC.Solve
open GHC.Export

//-------------------------------------------------------------------------------------------------
// EVALUATION

let mutable score = 0

let evaluation solution = ()

//-------------------------------------------------------------------------------------------------
// MAIN

[<EntryPoint>]
let main argv =
    (*
    //printfn "%A" argv
    // import
    let inPath = "../input.in"
    let r = import inPath
    // solution

    // evaluation
    evaluation r
    printfn "score : %d" score
    //export 
    export "../output.txt" [||]
    *)

    let q1 = MPriorityQueue.empty

    MPriorityQueue.push 1 1 q1 
    MPriorityQueue.push 2 2 q1
    MPriorityQueue.push 3 3 q1
    MPriorityQueue.push 4 4 q1

    let mi = MPriorityQueue.popMin q1
    
    MPriorityQueue.toSeq q1 |> printfn "q1 : %A"
    printfn "min : %A" mi

    0 // return an integer exit code
