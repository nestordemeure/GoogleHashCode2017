module GHC.Domain

open ExtCore.Collections
open System.Collections.Generic

open GHC.Extensions
open GHC.Extensions.Common

//-------------------------------------------------------------------------------------------------

//type graph = Dictionary<'key,'Node>
type Video = {idv : int ; size : int}

type Request = { video : int ; value : int ; mutable poid : int ; score : float }

type Point = {idP : int ; latency : int ; caches : int array ; reqs : Request list}

let createPoint id latency cacheNum = { idP = id ; latency = latency ; caches = Array.create cacheNum -1 ; reqs = [] } 

//-------------------------------------------------------------------------------------------------


