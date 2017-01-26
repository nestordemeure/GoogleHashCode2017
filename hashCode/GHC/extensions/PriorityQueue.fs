namespace GHC.Extensions

open System
open ExtCore.Collections
open GHC.Extensions
open GHC.Extensions.Common

/// struct to store a value and its key (cache friendly)
type HeapEntry<'K,'V> = struct val k:'K val v:'V new(k,v) = {k=k;v=v} end

//-------------------------------------------------------------------------------------------------
// PRIORITY QUEUE 
// adapted from : http://rosettacode.org/wiki/Priority_queue#F.23

[<CompilationRepresentation(CompilationRepresentationFlags.UseNullAsTrueValue)>]
[<NoEquality; NoComparison>]
type PriorityQueue<'K,'V> =
    | Mt
    | Br of HeapEntry<'K,'V> * PriorityQueue<'K,'V> * PriorityQueue<'K,'V>

/// Min Priority Queue
[<RequireQualifiedAccess>]
module PriorityQueue =
 
  let empty = Mt
 
  let isEmpty = function | Mt -> true
                         | _  -> false
 
  /// Return number of elements in the priority queue. 
  /// /O(log(n)^2)/ 
  let rec size = function
    | Mt -> 0 
    | Br(_, ll, rr) ->
        let n = size rr
        // rest n p q, where n = size ll, and size ll - size rr = 0 or 1 
        // returns 1 + size ll - size rr. 
        let rec rest n pl pr =
          match pl with
            | Mt -> 1
            | Br(_, pll, plr) ->
                match pr with
                  | Mt -> 2
                  | Br(_, prl, prr) ->
                      let nm1 = n - 1 in let d = nm1 >>> 1
                      if (nm1 &&& 1) = 0
                        then rest d pll prl // subtree sizes: (d or d+1), d; d, d 
                        else rest d plr prr // subtree sizes: d+1, (d or d+1); d+1, d 
        2 * n + rest n ll rr
 
  let peekMin = function | Br(kv, _, _) -> Some(kv.k, kv.v)
                         | _            -> None
 
  let rec push wk wv = 
    function | Mt -> Br(HeapEntry(wk, wv), Mt, Mt)
             | Br(vkv, ll, rr) ->
                 if wk <= vkv.k then
                   Br(HeapEntry(wk, wv), push vkv.k vkv.v rr, ll)
                 else Br(vkv, push wk wv rr, ll)
 
  let inline private siftdown wk wv pql pqr =
    let rec sift pl pr =
      match pl with
        | Mt -> Br(HeapEntry(wk, wv), Mt, Mt)
        | Br(vkvl, pll, plr) ->
            match pr with
              | Mt -> if wk <= vkvl.k then Br(HeapEntry(wk, wv), pl, Mt)
                      else Br(vkvl, Br(HeapEntry(wk, wv), Mt, Mt), Mt)
              | Br(vkvr, prl, prr) ->
                  if wk <= vkvl.k && wk <= vkvr.k then Br(HeapEntry(wk, wv), pl, pr)
                  elif vkvl.k <= vkvr.k then Br(vkvl, sift pll plr, pr)
                  else Br(vkvr, pl, sift prl prr)
    sift pql pqr                                        
 
  let replaceMin wk wv = function | Mt -> Mt
                                  | Br(_, ll, rr) -> siftdown wk wv ll rr
 
  let deleteMin = function 
        | Mt -> Mt
        | Br(_, ll, Mt) -> ll
        | Br(vkv, ll, rr) ->
          let rec leftrem = function | Mt -> vkv, Mt // should never happen
                                     | Br(kvd, Mt, _) -> kvd, Mt
                                     | Br(vkv, Br(kvd, _, _), Mt) ->
                                                 kvd, Br(vkv, Mt, Mt)
                                     | Br(vkv, pl, pr) -> let kvd, pqd = leftrem pl
                                                          kvd, Br(vkv, pr, pqd)
          let (kvd, pqd) = leftrem ll
          siftdown kvd.k kvd.v rr pqd; 
 
  let adjust f pq =
        let rec adj = function 
              | Mt -> Mt
              | Br(vkv, ll, rr) -> let nk, nv = f vkv.k vkv.v
                                   siftdown nk nv (adj ll) (adj rr)
        adj pq
 
  let merge (pq1:PriorityQueue<_,_>) (pq2:PriorityQueue<_,_>) = // merges without using a sequence
    match pq1 with
      | Mt -> pq2
      | _ ->
        match pq2 with
          | Mt -> pq1
          | _ ->
            let rec zipper lvl pq rest =
              if lvl = 0 then Mt, pq, rest else
              let lft = lvl >>> 1 in let rght = (lvl - 1) >>> 1
              match pq with
                | Mt ->
                  match rest with
                    | [] | Mt :: _ -> Mt, pq, [] // Mt in list never happens
                    | Br(kv, ll, Mt) :: tl ->
                        let pl, pql, rstl = zipper lft ll tl
                        let pr, pqr, rstr = zipper rght pql rstl
                        siftdown kv.k kv.v pl pr, pqr, rstr
                    | Br(kv, ll, rr) :: tl ->
                        let pl, pql, rstl = zipper lft ll (rr :: tl)
                        let pr, pqr, rstr = zipper rght pql rstl
                        siftdown kv.k kv.v pl pr, pqr, rstr
                | Br(kv, ll, Mt) ->
                    let pl, pql, rstl = zipper lft ll rest
                    let pr, pqr, rstr = zipper rght pql rstl
                    siftdown kv.k kv.v pl pr, pqr, rstr
                | Br(kv, ll, rr) ->
                    let pl, pql, rstl = zipper lft ll (rr :: rest)
                    let pr, pqr, rstr = zipper rght pql rstl
                    siftdown kv.k kv.v pl pr, pqr, rstr
            let sz = size pq1 + size pq2
            let pq, _, _ = zipper sz pq1 [pq2] in pq
 
  let popMin pq = match peekMin pq with
                      | None -> None
                      | Some(kv) -> Some(kv, deleteMin pq)
 
  let fromSeq sq = 
    if Seq.isEmpty sq then Mt
    else let nmrtr = sq.GetEnumerator()
         let rec build lvl = if lvl = 0 || not (nmrtr.MoveNext()) then Mt
                             else let ck, cv = nmrtr.Current
                                  let lft = lvl >>> 1
                                  let rght = (lvl - 1) >>> 1
                                  siftdown ck cv (build lft) (build rght)
         build (sq |> Seq.length)
  
  let toSeq pq = Seq.unfold popMin pq

//-------------------------------------------------------------------------------------------------
// MAX PRIORITY QUEUE 
// adapted from : http://rosettacode.org/wiki/Priority_queue#F.23

/// Min Priority Queue
[<RequireQualifiedAccess>]
module MaxPriorityQueue =
 
  let empty = Mt
 
  let isEmpty = function | Mt -> true
                         | _  -> false
 
  /// Return number of elements in the priority queue. 
  /// /O(log(n)^2)/ 
  let rec size = function
    | Mt -> 0 
    | Br(_, ll, rr) ->
        let n = size rr
        // rest n p q, where n = size ll, and size ll - size rr = 0 or 1 
        // returns 1 + size ll - size rr. 
        let rec rest n pl pr =
          match pl with
            | Mt -> 1
            | Br(_, pll, plr) ->
                match pr with
                  | Mt -> 2
                  | Br(_, prl, prr) ->
                      let nm1 = n - 1 in let d = nm1 >>> 1
                      if (nm1 &&& 1) = 0
                        then rest d pll prl // subtree sizes: (d or d+1), d; d, d 
                        else rest d plr prr // subtree sizes: d+1, (d or d+1); d+1, d 
        2 * n + rest n ll rr
 
  let peekMax = function | Br(kv, _, _) -> Some(kv.k, kv.v)
                         | _            -> None
 
  let rec push wk wv = 
    function | Mt -> Br(HeapEntry(wk, wv), Mt, Mt)
             | Br(vkv, ll, rr) ->
                 if wk >= vkv.k then
                   Br(HeapEntry(wk, wv), push vkv.k vkv.v rr, ll)
                 else Br(vkv, push wk wv rr, ll)
 
  let inline private siftdown wk wv pql pqr =
    let rec sift pl pr =
      match pl with
        | Mt -> Br(HeapEntry(wk, wv), Mt, Mt)
        | Br(vkvl, pll, plr) ->
            match pr with
              | Mt -> if wk >= vkvl.k then Br(HeapEntry(wk, wv), pl, Mt)
                      else Br(vkvl, Br(HeapEntry(wk, wv), Mt, Mt), Mt)
              | Br(vkvr, prl, prr) ->
                  if wk >= vkvl.k && wk >= vkvr.k then Br(HeapEntry(wk, wv), pl, pr)
                  elif vkvl.k >= vkvr.k then Br(vkvl, sift pll plr, pr)
                  else Br(vkvr, pl, sift prl prr)
    sift pql pqr                                        
 
  let replaceMax wk wv = function | Mt -> Mt
                                  | Br(_, ll, rr) -> siftdown wk wv ll rr
 
  let deleteMax = function 
        | Mt -> Mt
        | Br(_, ll, Mt) -> ll
        | Br(vkv, ll, rr) ->
          let rec leftrem = function | Mt -> vkv, Mt // should never happen
                                     | Br(kvd, Mt, _) -> kvd, Mt
                                     | Br(vkv, Br(kvd, _, _), Mt) ->
                                                 kvd, Br(vkv, Mt, Mt)
                                     | Br(vkv, pl, pr) -> let kvd, pqd = leftrem pl
                                                          kvd, Br(vkv, pr, pqd)
          let (kvd, pqd) = leftrem ll
          siftdown kvd.k kvd.v rr pqd; 
 
  let adjust f pq =
        let rec adj = function 
              | Mt -> Mt
              | Br(vkv, ll, rr) -> let nk, nv = f vkv.k vkv.v
                                   siftdown nk nv (adj ll) (adj rr)
        adj pq
 
  let merge (pq1:PriorityQueue<_,_>) (pq2:PriorityQueue<_,_>) = // merges without using a sequence
    match pq1 with
      | Mt -> pq2
      | _ ->
        match pq2 with
          | Mt -> pq1
          | _ ->
            let rec zipper lvl pq rest =
              if lvl = 0 then Mt, pq, rest else
              let lft = lvl >>> 1 in let rght = (lvl - 1) >>> 1
              match pq with
                | Mt ->
                  match rest with
                    | [] | Mt :: _ -> Mt, pq, [] // Mt in list never happens
                    | Br(kv, ll, Mt) :: tl ->
                        let pl, pql, rstl = zipper lft ll tl
                        let pr, pqr, rstr = zipper rght pql rstl
                        siftdown kv.k kv.v pl pr, pqr, rstr
                    | Br(kv, ll, rr) :: tl ->
                        let pl, pql, rstl = zipper lft ll (rr :: tl)
                        let pr, pqr, rstr = zipper rght pql rstl
                        siftdown kv.k kv.v pl pr, pqr, rstr
                | Br(kv, ll, Mt) ->
                    let pl, pql, rstl = zipper lft ll rest
                    let pr, pqr, rstr = zipper rght pql rstl
                    siftdown kv.k kv.v pl pr, pqr, rstr
                | Br(kv, ll, rr) ->
                    let pl, pql, rstl = zipper lft ll (rr :: rest)
                    let pr, pqr, rstr = zipper rght pql rstl
                    siftdown kv.k kv.v pl pr, pqr, rstr
            let sz = size pq1 + size pq2
            let pq, _, _ = zipper sz pq1 [pq2] in pq
 
  let popMax pq = match peekMax pq with
                      | None -> None
                      | Some(kv) -> Some(kv, deleteMax pq)
 
  let fromSeq sq = 
    if Seq.isEmpty sq then Mt
    else let nmrtr = sq.GetEnumerator()
         let rec build lvl = if lvl = 0 || not (nmrtr.MoveNext()) then Mt
                             else let ck, cv = nmrtr.Current
                                  let lft = lvl >>> 1
                                  let rght = (lvl - 1) >>> 1
                                  siftdown ck cv (build lft) (build rght)
         build (sq |> Seq.length)
  
  let toSeq pq = Seq.unfold popMax pq

//-------------------------------------------------------------------------------------------------
// MUTABLE PRIORITY QUEUE 
// adapted from : https://en.wikipedia.org/wiki/Binary_heap

/// a priority queue that is changed in place, more efficient than its functionnal counterpart
type MutablePriorityQueue<'K,'V> = ResizeArray<HeapEntry<'K,'V>>

/// a priority queue that is changed in place, more efficient than its functionnal counterpart
[<RequireQualifiedAccess>]
module MPriorityQueue =

  let inline private kvToTuple (kv:HeapEntry<_,_>) = kv.k, kv.v
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
      let mutable positionSon1 = getSon1 myPosition
      let mutable positionSon2 = getSon2 myPosition
      let mutable keepGoing = true
      // explore both childrens while they are legal and better than kv
      while keepGoing && positionSon2 < pq.Count && positionSon1 < pq.Count do 
          match pq.[positionSon1].k < pq.[positionSon2].k with 
          | true when kv.k > pq.[positionSon1].k ->
              pq.[myPosition] <- pq.[positionSon1]
              myPosition <- positionSon1
          | false when kv.k > pq.[positionSon2].k ->
              pq.[myPosition] <- pq.[positionSon2]
              myPosition <- positionSon2
          | _ -> keepGoing <- false
          positionSon1 <- getSon1 myPosition
          positionSon2 <- getSon2 myPosition
      // was Son1 not explored because son2 is not legal anymore (bottom of the tree)
      if positionSon1 < pq.Count && kv.k > pq.[positionSon1].k then 
          pq.[myPosition] <- pq.[positionSon1]
          myPosition <- positionSon1
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

  let toSeq (pq:MutablePriorityQueue<_,_>) = Seq.map kvToTuple pq

  let toArray (pq:MutablePriorityQueue<_,_>) = 
    Array.init pq.Count (fun i -> kvToTuple pq.[i])

//-------------------------------------------------------------------------------------------------
// MUTABLE MAX PRIORITY QUEUE 
// adapted from : https://en.wikipedia.org/wiki/Binary_heap

/// a max priority queue that is changed in place, more efficient than its functionnal counterpart
[<RequireQualifiedAccess>]
module MaxMPriorityQueue =
  // only bubleup and bubledown are modified, it would be nice to find a way to factorise the code
  let inline private kvToTuple (kv:HeapEntry<_,_>) = kv.k, kv.v
  let inline private getFather i = (i-1)/2
  let inline private getSon1 i = 1 + 2*i
  let inline private getSon2 i = 2 + 2*i

  /// buble up an element from myPosition to, potentialy, the top of the heap
  let inline private bubleUp (kv:HeapEntry<_,_>) myPosition (pq: MutablePriorityQueue<_,_>) =
    let mutable myPosition = myPosition 
    let mutable positionFather = getFather myPosition
    let mutable father = pq.[positionFather]
    while kv.k > father.k && myPosition > 0 do 
        pq.[myPosition] <- father 
        myPosition <- positionFather
        positionFather <- getFather myPosition
        father <- pq.[positionFather]
    pq.[myPosition] <- kv

  /// buble down an element from myPosition to, potentialy, the bottom of the heap
  let inline private bubleDown (kv:HeapEntry<_,_>) myPosition (pq: MutablePriorityQueue<_,_>) =
    if myPosition < pq.Count then 
      let mutable myPosition = myPosition 
      let mutable positionSon1 = getSon1 myPosition
      let mutable positionSon2 = getSon2 myPosition
      let mutable keepGoing = true
      // explore both childrens while they are legal and better than kv
      while keepGoing && positionSon2 < pq.Count && positionSon1 < pq.Count do 
          match pq.[positionSon1].k > pq.[positionSon2].k with 
          | true when kv.k < pq.[positionSon1].k ->
              pq.[myPosition] <- pq.[positionSon1]
              myPosition <- positionSon1
          | false when kv.k < pq.[positionSon2].k ->
              pq.[myPosition] <- pq.[positionSon2]
              myPosition <- positionSon2
          | _ -> keepGoing <- false
          positionSon1 <- getSon1 myPosition
          positionSon2 <- getSon2 myPosition
      // was Son1 not explored because son2 is not legal anymore (bottom of the tree)
      if positionSon1 < pq.Count && kv.k < pq.[positionSon1].k then 
          pq.[myPosition] <- pq.[positionSon1]
          myPosition <- positionSon1
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

  let deleteMax (pq: MutablePriorityQueue<_,_>) =
    let bottomElement = pq.[pq.Count - 1]
    pq.RemoveAt(pq.Count - 1) // shrink the queue to get rid of the bottom element
    bubleDown bottomElement 0 pq

  let peekMax (pq:MutablePriorityQueue<_,_>) = 
      if pq.Count < 1 then None else
         let kv = pq.[0]
         Some (kv.k, kv.v)
 
  let popMax (pq:MutablePriorityQueue<_,_>) = 
      if pq.Count < 1 then None else
         let kv = pq.[0]
         deleteMax pq ; Some (kv.k, kv.v)
  
  let fromSeq sq = 
    let pq : MutablePriorityQueue<_,_> = sq |> Seq.map (fun (k,v) -> HeapEntry(k,v)) |> ResizeArray.ofSeq 
    for i = pq.Count/2 downto 1 do 
      bubleDown pq.[i] i pq
    pq

  let toSeq (pq:MutablePriorityQueue<_,_>) = Seq.map kvToTuple pq

  let toArray (pq:MutablePriorityQueue<_,_>) = 
    Array.init pq.Count (fun i -> kvToTuple pq.[i])

//-------------------------------------------------------------------------------------------------