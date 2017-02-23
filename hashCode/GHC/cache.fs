module GHC.Cache

open ExtCore.Collections

open GHC.Extensions
open GHC.Extensions.Common
open GHC.Domain


let fillCache cacheNum points =
    let caches = Array.create cacheNum []

    for point in points do
        for req in point.reqs do
            for i=0 to cacheNum-1 do
                let lat = point.caches.[i]
                if lat >= 0 then
                    let newReq = {req with lat=lat}
                    caches.[i] <- newReq::caches.[i]
    caches

let collapseScoreReqs vidId reqList =
   match reqList with
   | [] -> failwith "fail"
   | t::q ->
      let sum acc r =
         let lat = (float r.lat) + 1.
         {acc with score = acc.score + r.score*lat}
      List.fold sum {t with score = t.score*(float t.lat)+t.score} q

let fuseReqsList (reqs: Request list) =
   reqs
   |> List.groupBy (fun r -> r.video )
   |> List.map (fun (vidId,reqList) -> collapseScoreReqs vidId reqList)

let fuseCache caches =
    Array.mapInPlace fuseReqsList caches



let getListEndpoints cache endpoints =
    let epSet = MutableSet.empty
    for i in endpoints do
        if (i.caches.[cache] <> -1) then
            MutableSet.add i epSet
    epSet

let isSameVideo v1 v2 =
    (v1.idv = v2.idv)

let computeCache (videos, points, cacheNum, cacheSize) =
    let caches = fillCache cacheNum points
    fuseCache caches


let rec selectionneBests tailleMax result cache =
    match cache with
    | [] -> result
    | p::q when p.poid > tailleMax -> selectionneBests tailleMax result q
    | p::q -> selectionneBests (tailleMax-p.poid) (p::result) q

let filterCaches caches tailleMax = 
    Array.mapInPlace (selectionneBests tailleMax []) caches 
    caches

