module GHC.Main

open ExtCore.Collections

open GHC.Extensions
open GHC.Extensions.Common
open GHC.Domain
open GHC.Import
open GHC.Solve
open GHC.Export
open System.Collections.Generic

//-------------------------------------------------------------------------------------------------
// EVALUATION
(*
let mutable score = 0

let evaluation solution = ()
*)
//-------------------------------------------------------------------------------------------------
// MAIN

[<EntryPoint>]
let main argv =
    // import
    let inPaths = ["me_at_the_zoo";"trending_today";"videos_worth_spreading";"kittens"]
    for inPath in inPaths do
       printfn "%s" inPath
       let videos, points, cacheNum, cacheSize = import (sprintf "../inputs/%s.in" inPath)
       // solution
       let sol = solution (videos, points, cacheNum, cacheSize)
       // evaluation
       (*
       evaluation sol
       printfn "score : %d" score
       score <- 0
       *)
       //export 
       export (sprintf "../outputs/%sOut.txt" inPath) sol
    0 // return an integer exit code
