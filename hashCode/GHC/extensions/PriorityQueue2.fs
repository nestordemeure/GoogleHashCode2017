namespace GHC.Extensions

open System
open System.Collections.Generic
open GHC.Extensions
open GHC.Extensions.Common
 
//-------------------------------------------------------------------------------------------------
// MUTABLE PRIORITY QUEUE 

// adapted from : http://rosettacode.org/wiki/Priority_queue#F.23
/// change-in-place Min Priority Queue, quicker if you can use them
[<RequireQualifiedAccess>]
module MPriorityQueue =
  type HeapEntry<'K,'V> = struct val k:'K val v:'V new(k,v) = { k=k;v=v } end
  /// a priority queue that is changed in place, more efficient than its functionnal counterpart
  type MutablePriorityQueue<'K,'V> = SortedSet<HeapEntry<'K,'V>>
 
  let empty<'K,'V> = MutablePriorityQueue<HeapEntry<'K,'V>>()
 
  let isEmpty (pq: MutablePriorityQueue<_,_>) = pq.Count = 0
 
  let size (pq: MutablePriorityQueue<_,_>) = pq.Count
 
  let peekMin (pq:MutablePriorityQueue<_,_>) = 
      if pq.Count <= 1 then None else
         let kv = pq.Min
         Some (kv.k, kv.v)
 
  let push k v (pq:MutablePriorityQueue<_,_>) =
    pq.Add(HeapEntry(k,v)) |> ignore
 
  let popMin (pq:MutablePriorityQueue<_,_>) = 
      if pq.Count <= 1 then None else
         let kv = pq.Min
         pq.Remove kv |> ignore
         Some (kv.k, kv.v)

  let fromSeq sq = 
    let result = empty 
    Seq.iter (fun (k,v) -> result.Add(HeapEntry(k,v)) |> ignore) sq 
    result

  let toSeq (pq:MutablePriorityQueue<_,_>) = pq |> Seq.map (fun kv -> kv.k, kv.v)
