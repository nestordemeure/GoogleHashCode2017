namespace GHC.Extensions

open System
open ExtCore.Collections
open GHC.Extensions
open GHC.Extensions.Common

//-------------------------------------------------------------------------------------------------
// PRIORITY QUEUE 

[<CompilationRepresentation(CompilationRepresentationFlags.UseNullAsTrueValue)>]
/// good amortized Min Priority Queue
/// the price of a deleteMin/popMin can go up to O(n) but it is amortizd into O(ln(n)) other operations are O(1)
type PairingHeap<'K,'V> =
    | EmptyHeap
    | Heap of HeapEntry<'K,'V> * (PairingHeap<'K,'V> list)

/// good amortized Min Priority Queue
/// the price of a deleteMin/popMin can go up to O(n) but it is amortizd into O(ln(n)) other operations are O(1)
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

  /// merge a list of heaps using a pairing procedure
  (*
  let mergeMany l =
    let rec mergeInPairs pairs l =
      match l with 
      | [] -> pairs
      | [pq] -> pq::pairs
      | pq1::pq2::q -> mergeInPairs ((merge pq1 pq2) :: pairs) q
    mergeInPairs [] l |> List.fold merge EmptyHeap
 *)
  /// merge a list of heaps using a pairing procedure
  /// non tail-recursive, based on Okazaki's implementation
  let rec mergeMany l =
    match l with 
    | [] -> EmptyHeap
    | [pq] -> pq
    | pq1::pq2::q -> merge (merge pq1 pq2) (mergeMany q)

  let push k v pq = merge pq (singleton k v)
 
  let deleteMin pq = 
    match pq with 
    | EmptyHeap -> pq 
    | Heap(_,l) -> mergeMany l

  let peekMin pq =
    match pq with 
    | EmptyHeap -> None
    | Heap(kv,_) -> Some(kv.k, kv.v)

  let popMin pq = 
    match pq with 
    | EmptyHeap -> None 
    | Heap(kv,l) -> Some( (kv.k,kv.v), mergeMany l)
 
  let fromList l = l |> List.map (fun (k,v) -> singleton k v) |> mergeMany

  let fromSeq sq = [for (k,v) in sq -> singleton k v] |> mergeMany
  
  let toSeq pq = Seq.unfold popMin pq

//-------------------------------------------------------------------------------------------------
// LAZY PRIORITY QUEUE 

[<CompilationRepresentation(CompilationRepresentationFlags.UseNullAsTrueValue)>]
/// good amortized Min Priority Queue
/// the price of a deleteMin/popMin can go up to O(n) but it is amortizd into O(ln(n)) other operations are O(1)
type LazyPairingHeap<'K,'V> =
    | LEmptyHeap
    | LHeap of HeapEntry<'K,'V> * (LazyPairingHeap<'K,'V> list) * Lazy<LazyPairingHeap<'K,'V>>

/// good amortized Min Priority Queue
/// the price of a deleteMin/popMin can go up to O(n) but it is amortized into O(ln(n)) other operations are O(1)
/// Okazaki warns about this amortization when you use persistance (eah copy of the heap will duplicate the amortization) 
/// he has a lazy solution that makes mergeMany fully lazy (making it too slow to remain competitive)
/// here is a personnal solution, mergeMany is not lazy but deleteMin is (performances in real applications remain to be tested)
[<RequireQualifiedAccess>]
module LazyPairingHeap =
 
  let empty = LEmptyHeap

  let inline singleton k v = LHeap( HeapEntry(k,v) , [], Lazy.CreateFromValue LEmptyHeap )
 
  let inline isEmpty pq = pq = LEmptyHeap
 
  /// Return number of elements in the priority queue. 
  /// WARNING : 0(n)
  let rec size pq =
    match pq with 
    | LEmptyHeap -> 0 
    | LHeap(_,l,_) -> 1 + (List.sumBy size l)

  /// merge a list of heaps using a pairing procedure
  /// non tail-recursive, based on Okazaki's implementation
  let rec mergeMany l =
    match l with 
    | [] -> LEmptyHeap
    | [pq] -> pq
    | pq1::pq2::q -> merge (merge pq1 pq2) (mergeMany q)
  /// merge that lazily preapare the deletion operation 
  and merge pq1 pq2 =
    match pq1, pq2 with 
    | LEmptyHeap, _ -> pq2
    | _, LEmptyHeap -> pq1
    | LHeap(kv1,l1,_), LHeap(kv2,l2,_) when kv1 < kv2 -> 
          let l = pq2 :: l1
          let d = lazy (mergeMany l)
          LHeap(kv1,l,d) 
    | LHeap(kv1,l1,_), LHeap(kv2,l2,_) -> 
          let l = pq1 :: l2
          let d = lazy (mergeMany l)
          LHeap(kv2,l,d)

  let push k v pq = merge pq (singleton k v)
 
  let deleteMin pq = 
    match pq with 
    | LEmptyHeap -> pq 
    | LHeap(_,_,d) -> d.Value // evaluate the lazy tail

  let peekMin pq =
    match pq with 
    | LEmptyHeap -> None
    | LHeap(kv,_,_) -> Some(kv.k, kv.v)

  let popMin pq = 
    match pq with 
    | LEmptyHeap -> None 
    | LHeap(kv,_,d) -> Some( (kv.k,kv.v), d.Value) // evaluate the lazy tail
 
  let fromList l = l |> List.map (fun (k,v) -> singleton k v) |> mergeMany

  let fromSeq sq = [for (k,v) in sq -> singleton k v] |> mergeMany
  
  let toSeq pq = Seq.unfold popMin pq
