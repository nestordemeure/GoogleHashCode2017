namespace GHC.Extensions

open System
open GHC.Extensions
open GHC.Extensions.Common

//-------------------------------------------------------------------------------------------------
// GENERIC MAXVALUE (for generic priority queue)

module LanguagePrimitives =
  type MaxValue = MaxValue with
      static member ($) (_:unit          , _:MaxValue) = ()
      static member ($) (_:bool          , _:MaxValue) = true
      static member ($) (_:char          , _:MaxValue) = Char.MaxValue
      static member ($) (_:byte          , _:MaxValue) = Byte.MaxValue
      static member ($) (_:sbyte         , _:MaxValue) = SByte.MaxValue
      static member ($) (_:float         , _:MaxValue) = Double.MaxValue
      static member ($) (_:int16         , _:MaxValue) = Int16.MaxValue
      static member ($) (_:int           , _:MaxValue) = Int32.MaxValue
      static member ($) (_:int64         , _:MaxValue) = Int64.MaxValue
      static member ($) (_:float32       , _:MaxValue) = Single.MaxValue
      static member ($) (_:uint16        , _:MaxValue) = UInt16.MaxValue
      static member ($) (_:uint32        , _:MaxValue) = UInt32.MaxValue
      static member ($) (_:uint64        , _:MaxValue) = UInt64.MaxValue
      static member ($) (_:decimal       , _:MaxValue) = Decimal.MaxValue
      static member ($) (_:DateTime      , _:MaxValue) = DateTime.MaxValue
      static member ($) (_:DateTimeOffset, _:MaxValue) = DateTimeOffset.MaxValue
      static member ($) (_:TimeSpan      , _:MaxValue) = TimeSpan.MaxValue

  /// generic maxValue
  let inline maxValue() :'r =  Unchecked.defaultof<'r> $ MaxValue

  type MaxValue with
      static member inline ($) ((_:'a*'b         ), _:MaxValue) = maxValue(), maxValue()
      static member inline ($) ((_:'a*'b*'c      ), _:MaxValue) = maxValue(), maxValue(), maxValue()
      static member inline ($) ((_:'a*'b*'c*'d   ), _:MaxValue) = maxValue(), maxValue(), maxValue(), maxValue()
      static member inline ($) ((_:'a*'b*'c*'d*'e), _:MaxValue) = maxValue(), maxValue(), maxValue(), maxValue(), maxValue()

//-------------------------------------------------------------------------------------------------
// PRIORITY QUEUE 

// adapted from : http://rosettacode.org/wiki/Priority_queue#F.23
/// Min Priority Queue
[<RequireQualifiedAccess>]
module PriorityQueue =
  type HeapEntry<'K,'V> = struct val k:'K val v:'V new(k,v) = {k=k;v=v} end
  [<CompilationRepresentation(CompilationRepresentationFlags.UseNullAsTrueValue)>]
  [<NoEquality; NoComparison>]
  type PriorityQueue<'K,'V> =
      | Mt
      | Br of HeapEntry<'K,'V> * PriorityQueue<'K,'V> * PriorityQueue<'K,'V>
 
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
// MUTABLE PRIORITY QUEUE 

// adapted from : http://rosettacode.org/wiki/Priority_queue#F.23
/// change-in-place Min Priority Queue, quicker if you can use them
[<RequireQualifiedAccess>]
module BuggedMPriorityQueue =
  type HeapEntry<'K,'V> = struct val k:'K val v:'V new(k,v) = { k=k;v=v } end
  /// a priority queue that is changed in place, more efficient than its functionnal counterpart
  type MutablePriorityQueue<'K,'V> = ResizeArray<HeapEntry<'K,'V>>
 
  let empty<'K,'V> = MutablePriorityQueue<HeapEntry<'K,'V>>()
 
  let isEmpty (pq: MutablePriorityQueue<_,_>) = pq.Count = 0
 
  let size (pq: MutablePriorityQueue<_,_>) = 
      let cnt = pq.Count
      if cnt = 0 then 0 else cnt - 1
 
  let peekMin (pq:MutablePriorityQueue<_,_>) = 
      if pq.Count <= 1 then None else
         let kv = pq.[0]
         Some (kv.k, kv.v)
 
  let inline push k v (pq:MutablePriorityQueue<_,_>) =
    if pq.Count = 0 then pq.Add(HeapEntry(LanguagePrimitives.GenericZero,v)) //add an extra entry so there's always a right max node
    //if pq.Count = 0 then pq.Add(HeapEntry(k,v)) //add an extra entry so there's always a right max node
    let mutable nxtlvl = pq.Count in let mutable lvl = nxtlvl <<< 1 //1 past index of value added times 2
    pq.Add(pq.[nxtlvl - 1]) //copy bottom entry then do bubble up while less than next level up
    while ((lvl <- lvl >>> 1); nxtlvl <- nxtlvl >>> 1; nxtlvl <> 0) do
      let t = pq.[nxtlvl - 1] in if t.k > k then pq.[lvl - 1] <- t else lvl <- lvl <<< 1; nxtlvl <- 0 //causes loop break
    pq.[lvl - 1] <-  HeapEntry(k,v)
 
  let inline private siftdown k v ndx (pq: MutablePriorityQueue<_,_>) =
    let mutable i = ndx in let mutable ni = i in let cnt = pq.Count - 1
    while (ni <- ni + ni + 1; ni < cnt) do
      let lk = pq.[ni].k in let rk = pq.[ni + 1].k in let oi = i
      let k = if k > lk then i <- ni; lk else k in if k > rk then ni <- ni + 1; i <- ni
      if i <> oi then pq.[oi] <- pq.[i] else ni <- cnt //causes loop break
    pq.[i] <- HeapEntry(k,v)
 
  let replaceMin k v (pq:MutablePriorityQueue<_,_>) = siftdown k v 0 pq//; pq
 
  let deleteMin (pq:MutablePriorityQueue<_,_>) =
    let lsti = pq.Count - 2
    if lsti <= 0 then pq.Clear() else
      let lstkv = pq.[lsti]
      pq.RemoveAt(lsti)
      siftdown lstkv.k lstkv.v 0 pq
 
  /// Adjust all the contents using the function, then re-heapify
  let adjust f (pq:MutablePriorityQueue<_,_>) =
    let cnt = pq.Count - 1
    let rec adj i =
      let lefti = i + i + 1 in let righti = lefti + 1
      let ckv = pq.[i] in let (nk, nv) = f ckv.k ckv.v
      if righti < cnt then adj righti
      if lefti < cnt then adj lefti; siftdown nk nv i pq
      else pq.[i] <- HeapEntry(nk, nv)
    adj 0
 
  let merge (pq1:MutablePriorityQueue<_,_>) (pq2:MutablePriorityQueue<_,_>) =
    if pq2.Count = 0 then pq1 else
    if pq1.Count = 0 then pq2 else
    let pq = empty
    pq.AddRange(pq1); pq.RemoveAt(pq.Count - 1)
    pq.AddRange(pq2)
    let sz = pq.Count - 1
    let rec build i =
      let lefti = i + i + 1
      if lefti < sz then
        let righti = lefti + 1 in build lefti; build righti
        let ckv = pq.[i] in siftdown ckv.k ckv.v i pq
    build 0; pq
 
  let popMin pq = 
   match peekMin pq with
   | None     -> None
   | Some(kv) -> deleteMin pq ; Some kv

  let inline fromSeq sq = 
    if Seq.isEmpty sq then empty
    else let pq = new MutablePriorityQueue<_,_>(sq |> Seq.map (fun (k, v) -> HeapEntry(k, v)))
         let sz = pq.Count in let lkv = pq.[sz - 1]
         pq.Add(HeapEntry(LanguagePrimitives.maxValue(), lkv.v))
         let rec build i =
           let lefti = i + i + 1
           if lefti < sz then
             let righti = lefti + 1 in build lefti; build righti
             let ckv = pq.[i] in siftdown ckv.k ckv.v i pq
         build 0; pq

  let toSeq (pq:MutablePriorityQueue<_,_>) = 
    seq {
      let mutable result = popMin pq
      while result <> None do 
        match result with 
        | Some r -> yield r
        | None -> ()
        //yield result
        result <- popMin pq
    }
