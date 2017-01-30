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

    /// let timeout = timeoutIn 1000
    /// if timeout () then (*something*) else (*something else*)
    let timeoutIn milliseconds =
        let timer = Diagnostics.Stopwatch.StartNew()
        fun () -> timer.ElapsedMilliseconds <= milliseconds

//-------------------------------------------------------------------------------------------------
// ARRAY

module Array =
   /// swap the values at the given indexes
   let inline swap (a : 'T array) i j =
      let temp = a.[i]
      a.[i] <- a.[j]
      a.[j] <- temp

   /// test if any element of the array satisfy the given predicate
   let existsi predicate (a : 'T array) =
      let mutable i = 0
      while i < a.Length && not (predicate i a.[i]) do 
         i <- i+1
      i = a.Length

   /// returns the index of the smallest element in the array
   let minIndex (a : 'T array) = 
      let mutable result = a.[0]
      let mutable resultIndex = 0
      for i = 1 to a.Length - 1 do 
         if a.[i] < result then 
            result <- a.[i]
            resultIndex <- i 
      resultIndex

   /// returns the index of the biggest element in the array
   let maxIndex (a : 'T array) = 
      let mutable result = a.[0]
      let mutable resultIndex = 0
      for i = 1 to a.Length - 1 do 
         if a.[i] > result then 
            result <- a.[i]
            resultIndex <- i 
      resultIndex

   /// returns the index of the smallest element in the array according to a given projection
   let minByIndex projection (a : 'T array) = 
      let mutable result = projection a.[0]
      let mutable resultIndex = 0
      for i = 1 to a.Length - 1 do 
         let pi = projection a.[i]
         if pi < result then 
            result <- pi
            resultIndex <- i 
      resultIndex

   /// returns the index of the biggest element in the array according to a given projection
   let maxByIndex projection (a : 'T array) = 
      let mutable result = projection a.[0]
      let mutable resultIndex = 0
      for i = 1 to a.Length - 1 do 
         let pi = projection a.[i]
         if pi > result then 
            result <- pi
            resultIndex <- i 
      resultIndex

   /// returns the smallest element in the array according to a given projection
   let minByi projection (a : 'T array) = 
      let mutable result = projection 0 a.[0]
      let mutable resultIndex = 0
      for i = 1 to a.Length - 1 do 
         let pi = projection i a.[i]
         if pi < result then 
            result <- pi
            resultIndex <- i 
      a.[resultIndex]

   /// returns the smallest element in the array according to a given projection
   let maxByi projection (a : 'T array) = 
      let mutable result = projection 0 a.[0]
      let mutable resultIndex = 0
      for i = 1 to a.Length - 1 do 
         let pi = projection i a.[i]
         if pi > result then 
            result <- pi
            resultIndex <- i 
      a.[resultIndex]

//-------------------------------------------------------------------------------------------------
// LIST

module List =
   /// change only the first element to satisfy the predicate
   let rec singleMap predicate mapping l =
      match l with 
      | [] -> l
      | t::q when predicate t -> (mapping t) :: q
      | t::q -> t :: (singleMap predicate mapping q)

   /// replace the first occurence of an element with a given new value
   let rec replace oldx newx l =
      match l with 
      | [] -> l
      | x::q when x = oldx -> newx :: q
      | x::q -> x :: (replace oldx newx q)