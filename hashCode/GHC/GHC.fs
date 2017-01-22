module GHC

open FSharpx.Collections
open GHC.Extensions
open GHC.Domain
open GHC.Import
open GHC.Solve
open GHC.Export

//-------------------------------------------------------------------------------------------------



//-------------------------------------------------------------------------------------------------
// MAIN

[<EntryPoint>]
let main argv =
    printfn "%A" argv
    0 // return an integer exit code
