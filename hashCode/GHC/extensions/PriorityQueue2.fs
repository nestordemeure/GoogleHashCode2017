namespace GHC.Extensions

open System
open System.Collections.Generic
open GHC.Extensions
open GHC.Extensions.Common
 
//-------------------------------------------------------------------------------------------------
// MUTABLE PRIORITY QUEUE 

// adapted from : https://en.wikipedia.org/wiki/Binary_heap
/// change-in-place Min Priority Queue, quicker if you can use them
[<RequireQualifiedAccess>]
module MPriorityQueue =
  type HeapEntry<'K,'V> = struct val k:'K val v:'V new(k,v) = { k=k;v=v } end
  /// a priority queue that is changed in place, more efficient than its functionnal counterpart
  type MutablePriorityQueue<'K,'V> = ResizeArray<HeapEntry<'K,'V>>
 
  let empty<'K,'V> = MutablePriorityQueue<HeapEntry<'K,'V>>()
 
  let inline isEmpty (pq: MutablePriorityQueue<_,_>) = pq.Count = 0
 
  let inline size (pq: MutablePriorityQueue<_,_>) = pq.Count
 
  let peekMin (pq:MutablePriorityQueue<_,_>) = 
      if pq.Count < 1 then None else
         let kv = pq.[0]
         Some (kv.k, kv.v)
 
  // TODO
  let push k v (pq:MutablePriorityQueue<_,_>) =
    pq.Add(HeapEntry(k,v)) |> ignore
 
  // TODO
  let popMin (pq:MutablePriorityQueue<_,_>) = 
      if pq.Count <= 1 then None else
         let kv = pq.Min
         pq.Remove kv |> ignore
         Some (kv.k, kv.v)
  
  // TODO
  let fromSeq sq = 
    let result = empty 
    Seq.iter (fun (k,v) -> result.Add(HeapEntry(k,v)) |> ignore) sq 
    result

  let toSeq (pq:MutablePriorityQueue<_,_>) = pq |> Seq.map (fun kv -> kv.k, kv.v)
