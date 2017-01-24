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


//-------------------------------------------------------------------------------------------------
// MAIN

[<EntryPoint>]
let main argv =
    //printfn "%A" argv
    // import

    // solution

    // evaluation

    printfn "score : %d" score
    //export 

    0 // return an integer exit code
