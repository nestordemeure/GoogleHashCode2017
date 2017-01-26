namespace GHC.Extensions

open System
open System.Collections.Generic
open System.Linq
open GHC.Extensions
open GHC.Extensions.Common

/// Mutable set with basic set operations, based on a .net HashSet
type MutableSet<'T> = HashSet<'T>

/// Mutable set with basic set operations, based on a .net HashSet
[<RequireQualifiedAccess>]
module MutableSet =
   let empty<'T> = HashSet<'T>()

   let inline add x (s:MutableSet<_>) = s.Add(x) |> ignore

   let inline singleton x = empty |-> add x

   let inline contains x (s:MutableSet<_>) = s.Contains(x)

   let inline count (s:MutableSet<_>) = s.Count

   let inline isEmpty (s:MutableSet<_>) = s.Count = 0

   let inline remove x (s:MutableSet<_>) = s.Remove(x) |> ignore

   let inline toSeq (s:MutableSet<_>) = seq s

   let inline fromSeq (s:seq<'T>) = HashSet<'T>(s)

   let inline fromArray (a:array<'T>) = HashSet<'T>(a)

   let inline toArray (s:MutableSet<_>) = Array.zeroCreate s.Count |-> s.CopyTo

   let inline unionInPlace (mutedSet:MutableSet<_>) s = mutedSet.UnionWith(s)

   let inline intersectInPlace (mutedSet:MutableSet<_>) s = mutedSet.IntersectWith(s)

   let inline differenceInPlace (mutedSet:MutableSet<_>) s = mutedSet.ExceptWith(s)
