namespace GHC.Extensions

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

//-------------------------------------------------------------------------------------------------
// LIST

module List =
   /// output a list as a string separated by the given string
   let toString sep (l : string list) =
      match l with 
      | [] -> ""
      | _ -> List.reduce (fun acc s -> acc + sep + s ) l

//-------------------------------------------------------------------------------------------------
// MUTABLE SET 

/// modify in place set
module MSet =
    type MutableSet = int
    (*let contains e s =
        Set.co*)

