module GHC.Import

open System.IO
open ExtCore.Collections
open ExtCore.IO

open GHC.Extensions.Common
open GHC.Extensions.Scanf
open GHC.Extensions
open GHC.Domain

//-------------------------------------------------------------------------------------------------



//-------------------------------------------------------------------------------------------------
// IMPORT

let import path =
   // File.ReadLines(path)
   let text = File.ReadAllLines(path)
   let (videoNum,pointNum,requestNum,cacheNum,cacheSize) = sscanf "%d %d %d %d %d" text.[0]
   let videos =
      text.[1].Split(' ')
      |> Array.mapi (fun i s -> {idv=i;size=int s})
   let mutable index = 2
   let points =
      [|
         for p = 0 to pointNum-1 do 
            let (latency,localCacheNum) = sscanf "%d %d" text.[index]
            let point =  createPoint p latency cacheNum
            index <- index+1
            for c = 1 to localCacheNum do 
               let (idcache,latencyCache) = sscanf "%d %d" text.[index]
               point.caches.[idcache] <- latencyCache
               index <- index+1
            yield point
      |]
   for r = 0 to requestNum-1 do
      let vidId,pointId,reqNum = sscanf "%d %d %d" text.[index]
      let request = {video = vidId; value=reqNum}
      points.[pointId] <- {points.[pointId] with reqs = request :: points.[pointId].reqs}
      index <- index+1
   videos, points, cacheNum, cacheSize