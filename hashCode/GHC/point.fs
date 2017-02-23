module GHC.Point

open ExtCore.Collections

open GHC.Extensions
open GHC.Extensions.Common
open GHC.Domain

//-------------------------------------------------------------------------------------------------

let setPoids points videos = 
    for p in points do 
        for r in p.reqs do
            r.poid <- videos[r.video].size
    

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

let computeScoreReq latCenter (latCaches:int[]) req = 
   let latences = Array.map (fun l -> ) latCaches
   let poid = float req.poid
   let valeur = float req.value
   let score = poid / valeur
   {req with score = score}

let computeScore (point:Point) = 
   
   { point with reqs = List.map (computeScoreReq point.latency point.caches) point.reqs }

//-------------------------------------------------------------------------------------------------

let computePoints (videos, points, cacheNum, cacheSize) = 
   // mettre les poids dans les requetes
   setPoids points videos
   // fusionner les requetes par video
   Array.mapInPlace fuseReqs points
   // calculer un score pour chaque requette
   Array.mapInPlace computeScore points
