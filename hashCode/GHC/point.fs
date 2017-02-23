module GHC.Point

open ExtCore.Collections

open GHC.Extensions
open GHC.Extensions.Common
open GHC.Domain

//-------------------------------------------------------------------------------------------------

let setPoids points videos = ()

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

let computePoints (videos, points, cacheNum, cacheSize) = 
   // mettre les poids dans les requetes
   setPoids points videos
   // fusionner les requetes par video
   Array.mapInPlace fuseReqs points
   // calculer un score pour chaque requette