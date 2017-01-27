namespace GHC.Extensions

open System
open ExtCore.Collections
open GHC.Extensions
open GHC.Extensions.Common

/// struct to store a value and its key (cache friendly)
type HeapEntry<'K,'V> = struct val k:'K val v:'V new(k,v) = {k=k;v=v} end

//-------------------------------------------------------------------------------------------------
// PRIORITY QUEUE 

[<CompilationRepresentation(CompilationRepresentationFlags.UseNullAsTrueValue)>]
type PairingHeap<'K,'V> =
    | EmptyHeap
    | Heap of HeapEntry<'K,'V> * (PairingHeap<'K,'V> list)

/// Min Priority Queue
[<RequireQualifiedAccess>]
module PairingHeap =
 
  let empty = EmptyHeap

  let inline singleton k v = Heap( HeapEntry(k,v) , [])
 
  let inline isEmpty pq = pq = EmptyHeap
 
  /// Return number of elements in the priority queue. 
  /// WARNING : 0(n)
  let rec size pq =
    match pq with 
    | EmptyHeap -> 0 
    | Heap(kv,l) -> 1 + (List.sumBy size l)

  let merge pq1 pq2 =
    match pq1, pq2 with 
    | EmptyHeap, _ -> pq2
    | _, EmptyHeap -> pq1
    | Heap(kv1,l1), Heap(kv2,l2) -> if kv1 < kv2 then Heap(kv1, pq2 :: l1) else Heap(kv2, pq1 :: l2)

  /// merge a list of heap using a pairing procedure
  let mergeList l =
    let rec mergeInPairs pairs l =
      match l with 
      | [] -> pairs
      | [pq] -> pq::pairs
      | pq1::pq2::q -> mergeInPairs ((merge pq1 pq2) :: pairs) q
    mergeInPairs [] l |> List.fold merge EmptyHeap
 
  let push k v pq = merge pq (singleton k v)
 
  let deleteMin pq = 
    match pq with 
    | EmptyHeap -> pq 
    | Heap(_,l) -> mergeList l

  let peekMin pq =
    match pq with 
    | EmptyHeap -> None
    | Heap(kv,_) -> Some(kv.k, kv.v)

  let popMin pq = 
    match pq with 
    | EmptyHeap -> None 
    | Heap(kv,l) -> Some( (kv.k,kv.v), mergeList l)
 
  let fromList l = l |> List.map (fun (k,v) -> singleton k v) |> mergeList

  let fromSeq sq = [for (k,v) in sq -> singleton k v] |> mergeList
  
  let toSeq pq = Seq.unfold popMin pq
