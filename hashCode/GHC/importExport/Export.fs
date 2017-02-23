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

let rec ConvertToString list =
   match list with
   | [l] -> l.ToString()
   | head :: tail -> head.ToString() + " " + ConvertToString tail
   | [] -> ""

//-------------------------------------------------------------------------------------------------
// EXPORTATION

let export path caches =
   //File.WriteAllText(path, text)
   let cacheNumber = Seq.length caches |> string
   let lines = 
    caches
    |> List.map (fun c -> sprintf "%d %s" (c.idC) (ConvertToString c.video))
   File.WriteAllLines(path, cacheNumber :: lines)
