module GHC.Cache

open ExtCore.Collections

open GHC.Extensions
open GHC.Extensions.Common
open GHC.Domain

let computeCache (videos, points, cacheNum, cacheSize) = [||]


let getListEndpoints cache endpoints =
    let epSet = MutableSet.empty
    for i in endpoints do
        if (i.caches.[cache] <> -1) then
            MutableSet.add i epSet
    epSet

