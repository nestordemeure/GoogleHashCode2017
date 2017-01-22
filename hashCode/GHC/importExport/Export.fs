module GHC.Export

open FSharpx.Collections
open System.IO

open GHC.Extensions
open GHC.Domain

//-------------------------------------------------------------------------------------------------



//-------------------------------------------------------------------------------------------------
// EXPORTATION

let export path lines =
   //File.WriteAllText(path, text)
   File.WriteAllLines(path, lines)
