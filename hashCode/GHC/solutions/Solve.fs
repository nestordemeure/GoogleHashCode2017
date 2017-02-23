module GHC.Solve

open ExtCore.Collections

open GHC.Extensions
open GHC.Extensions.Common
open GHC.Domain

open GHC.Cache
open GHC.Point

//-------------------------------------------------------------------------------------------------



//-------------------------------------------------------------------------------------------------
// SOLUTION

/// solution
let solution (videos, points, cacheNum, cacheSize) = 
   computePoints (videos, points, cacheNum, cacheSize)
   computeCache (videos, points, cacheNum, cacheSize)
