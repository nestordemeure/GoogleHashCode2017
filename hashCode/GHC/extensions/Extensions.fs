module GHC.Extensions

//open System.Collections.Generic
//open FSharp.Collections.ParallelSeq

//-------------------------------------------------------------------------------------------------
// FUNCTIONS

/// unit pipe
let inline (|->) x f = f x ; x

//-------------------------------------------------------------------------------------------------
// Array

module Array =
   /// swap the values at the given indexes
   let inline swap (a : 'T array) i j =
      let temp = a.[i]
      a.[i] <- a.[j]
      a.[j] <- temp

//-------------------------------------------------------------------------------------------------
