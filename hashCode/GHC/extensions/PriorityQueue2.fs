namespace GHC.Extensions

open System
open System.Collections.Generic
open ExtCore.Collections
open GHC.Extensions
open GHC.Extensions.Common
 
//-------------------------------------------------------------------------------------------------
// MUTABLE PRIORITY QUEUE 

// adapted from : https://en.wikipedia.org/wiki/Binary_heap
/// change-in-place Min Priority Queue, quicker
[<RequireQualifiedAccess>]
module MPriorityQueue =
  type HeapEntry<'K,'V> = struct val k:'K val v:'V new(k,v) = { k=k;v=v } end
  /// a priority queue that is changed in place, more efficient than its functionnal counterpart
  type MutablePriorityQueue<'K,'V> = ResizeArray<HeapEntry<'K,'V>>
 
  //---------------------------------------------

  let inline private getFather i = (i-1)/2
  let inline private getSon1 i = 1 + 2*i
  let inline private getSon2 i = 2 + 2*i

  /// buble up an element from myPosition to, potentialy, the top of the heap
  let inline private bubleUp (kv:HeapEntry<_,_>) myPosition (pq: MutablePriorityQueue<_,_>) =
    let mutable myPosition = myPosition 
    let mutable positionFather = getFather myPosition
    let mutable father = pq.[positionFather]
    while kv.k < father.k && myPosition > 0 do 
        pq.[myPosition] <- father 
        myPosition <- positionFather
        positionFather <- getFather myPosition
        father <- pq.[positionFather]
    pq.[myPosition] <- kv

  /// buble down an element from myPosition to, potentialy, the bottom of the heap
  let inline private bubleDown (kv:HeapEntry<_,_>) myPosition (pq: MutablePriorityQueue<_,_>) =
    if myPosition < pq.Count then 
      let mutable myPosition = myPosition 
      let mutable bestPosition = myPosition
      let mutable positionSon1 = getSon1 myPosition
      let mutable positionSon2 = getSon2 myPosition
      let mutable keepGoing = true
      while keepGoing do 
          if positionSon1 < pq.Count && pq.[myPosition].k > pq.[positionSon1].k then 
            bestPosition <- positionSon1
          if positionSon2 < pq.Count && pq.[bestPosition].k > pq.[positionSon2].k then 
            bestPosition <- positionSon2
          if myPosition = bestPosition then keepGoing <- false else
            pq.[myPosition] <- pq.[bestPosition]
            myPosition <- bestPosition
            positionSon1 <- getSon1 myPosition
            positionSon2 <- getSon2 myPosition
      pq.[myPosition] <- kv

  //---------------------------------------------

  let empty<'K,'V> : MutablePriorityQueue<_,_> = MutablePriorityQueue<HeapEntry<'K,'V>>()

  let singleton k v : MutablePriorityQueue<_,_> = HeapEntry(k,v) |> ResizeArray.singleton
 
  let inline isEmpty (pq: MutablePriorityQueue<_,_>) = pq.Count = 0
 
  let inline size (pq: MutablePriorityQueue<_,_>) = pq.Count
 
  let push k v (pq:MutablePriorityQueue<_,_>) =
    let newElement = HeapEntry(k,v)
    pq.Add(newElement)
    bubleUp newElement (pq.Count-1) pq

  let deleteMin (pq: MutablePriorityQueue<_,_>) =
    let bottomElement = pq.[pq.Count - 1]
    pq.RemoveAt(pq.Count - 1) // shrink the queue to get rid of the bottom element
    bubleDown bottomElement 0 pq

  let peekMin (pq:MutablePriorityQueue<_,_>) = 
      if pq.Count < 1 then None else
         let kv = pq.[0]
         Some (kv.k, kv.v)
 
  let popMin (pq:MutablePriorityQueue<_,_>) = 
      if pq.Count < 1 then None else
         let kv = pq.[0]
         deleteMin pq ; Some (kv.k, kv.v)
  
  let fromSeq sq = 
    let pq : MutablePriorityQueue<_,_> = sq |> Seq.map (fun (k,v) -> HeapEntry(k,v)) |> ResizeArray.ofSeq 
    for i = pq.Count/2 downto 1 do 
      bubleDown pq.[i] i pq
    pq

  let toSeq (pq:MutablePriorityQueue<_,_>) = 
    Seq.init pq.Count (fun i -> popMin pq)
    //pq |> Seq.map (fun kv -> kv.k, kv.v)
