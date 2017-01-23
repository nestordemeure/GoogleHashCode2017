module GHC.Import

open FSharpx.Collections
open System.IO

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
   let (a,b) = sscanf "%d %d" text.[0]
   a