module GHC.Import

open FSharpx.Collections
open System.IO

open GHC.Extensions
open GHC.Domain

//-------------------------------------------------------------------------------------------------



//-------------------------------------------------------------------------------------------------
// IMPORT

let import path =
   // File.ReadLines(path)
   File.ReadAllLines(path)