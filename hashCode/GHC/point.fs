module GHC.Point

open ExtCore.Collections

open GHC.Extensions
open GHC.Extensions.Common
open GHC.Domain

//-------------------------------------------------------------------------------------------------

let setPoids points (videos:Video[]) = 
    for p in points do 
        for r in p.reqs do
            r.poid <- videos.[r.video].size
    

//-------------------------------------------------------------------------------------------------

let collapseReqs vidId reqList =
   List.reduce (fun acc r -> {acc with value = acc.value + r.value}) reqList

let fuseReqs (point:Point) =
   let newReqs = 
      point.reqs
      |> List.groupBy (fun r -> r.video )
      |> List.map (fun (vidId,reqList) -> collapseReqs vidId reqList)
   { point with reqs = newReqs }

//-------------------------------------------------------------------------------------------------

let computeScoreReq req = 
   let poid = float req.poid
   let valeur = float req.value
   //{req with score = poid / valeur}
   {req with score = valeur/poid }

let computeScore (point:Point) = 
   
   { point with reqs = List.map computeScoreReq point.reqs }

//-------------------------------------------------------------------------------------------------
let correctLatence point =
   Array.mapInPlace (fun l -> if l < 0 then l else point.latency - l ) point.caches

//-------------------------------------------------------------------------------------------------

let computePoints (videos, points, cacheNum, cacheSize) = 
   // mettre les poids dans les requetes
   setPoids points videos
   // latence corigées
   Array.iter correctLatence points
   // fusionner les requetes par video
   Array.mapInPlace fuseReqs points
   // calculer un score pour chaque requette
   Array.mapInPlace computeScore points
