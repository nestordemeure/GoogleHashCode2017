namespace GHC.Extensions

open System
//open System.Collections.Generic // for Dictionary
//open FSharp.Collections.ParallelSeq // for PSeq
//open System.Threading.Tasks // for Parallel.ForEach(data, action) |> ignore

//-------------------------------------------------------------------------------------------------
// FUNCTIONS

module Common =
    /// unit pipe
    let inline (|->) x f = f x ; x

//-------------------------------------------------------------------------------------------------
// ARRAY

module Array =
   /// swap the values at the given indexes
   let inline swap (a : 'T array) i j =
      let temp = a.[i]
      a.[i] <- a.[j]
      a.[j] <- temp
