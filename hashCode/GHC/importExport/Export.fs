module GHC.Export

open ExtCore.Collections
open System.IO

open GHC.Extensions
open GHC.Domain

//-------------------------------------------------------------------------------------------------

/// turns a list of strings into a single string (containing the elements IN order)
let listToString sep (l : string list) =
    match l with 
    | [] -> ""
    | _ -> List.reduce (fun acc s -> acc + sep + s ) l

//-------------------------------------------------------------------------------------------------
// EXPORTATION

let printReqList (cache, reqList) = 
   sprintf "%d %s" cache (listToString " " reqList)

let export path (caches:(Request list)[]) =
   let cacheNumber = Array.sumBy (fun rl -> if List.isEmpty rl then 0 else 1) caches |> string
   let lines = 
      caches
      |> Array.map ( List.map (fun r -> r.video |> string) )
      |> Array.indexed
      |> Array.filter (fun (i,l) -> l <> [])
      |> Array.map printReqList
      |> List.ofArray
   File.WriteAllLines(path, cacheNumber :: lines)
