namespace GHC.Extensions

open System
open System.Collections.Generic
open GHC.Extensions
open GHC.Extensions.Common

/// Mutable set, based on a .net HashSet
type MutableSet<'T> = HashSet<'T>

/// Mutable set, based on a .net HashSet
[<RequireQualifiedAccess>]
module MutableSet =
   let empty<'T> = HashSet<'T>()

   let add x (s:MutableSet<_>) = s.Add(x) |> ignore

   let singleton x = empty |-> add x

   let contains x (s:MutableSet<_>) = s.Contains(x)

   let count (s:MutableSet<_>) = s.Count

   let isEmpty (s:MutableSet<_>) = s.Count = 0

   let remove x (s:MutableSet<_>) = s.Remove(x) |> ignore

   let toSeq (s:MutableSet<_>) = seq s

   let fromSeq (s:seq<'T>) = HashSet<'T>(s)

   let unionWith (s1:MutableSet<_>) (withS2:MutableSet<_>) = s1.UnionWith(withS2)
