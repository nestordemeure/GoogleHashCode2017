namespace GHC.Extensions

open System
//open System.Collections.Generic // for Dictionary
//open FSharp.Collections.ParallelSeq // for PSeq
//open System.Threading.Tasks // for Parallel.ForEach(data, action) |> ignore

//-------------------------------------------------------------------------------------------------
// FUNCTIONS

module Common =
    /// unit pipe
    let inline (|->) x f = f x ; x

//-------------------------------------------------------------------------------------------------
// ARRAY

module Array =
   /// swap the values at the given indexes
   let inline swap (a : 'T array) i j =
      let temp = a.[i]
      a.[i] <- a.[j]
      a.[j] <- temp

//-------------------------------------------------------------------------------------------------
// LIST

module List =
   /// output a list as a string separated by the given string
   let toString sep (l : string list) =
      match l with 
      | [] -> ""
      | _ -> List.reduce (fun acc s -> acc + sep + s ) l

//-------------------------------------------------------------------------------------------------
// MUTABLE SET 

/// modify in place set
module MSet =
    type MutableSet<'T> = 'T
    (*let contains e s =
        Set.co*)

//-------------------------------------------------------------------------------------------------
// GENERIC MAXVALUE

module LanguagePrimitives =
  type MaxValue = MaxValue with
      static member ($) (_:unit          , _:MaxValue) = ()
      static member ($) (_:bool          , _:MaxValue) = true
      static member ($) (_:char          , _:MaxValue) = Char.MaxValue
      static member ($) (_:byte          , _:MaxValue) = Byte.MaxValue
      static member ($) (_:sbyte         , _:MaxValue) = SByte.MaxValue
      static member ($) (_:float         , _:MaxValue) = Double.MaxValue
      static member ($) (_:int16         , _:MaxValue) = Int16.MaxValue
      static member ($) (_:int           , _:MaxValue) = Int32.MaxValue
      static member ($) (_:int64         , _:MaxValue) = Int64.MaxValue
      static member ($) (_:float32       , _:MaxValue) = Single.MaxValue
      static member ($) (_:uint16        , _:MaxValue) = UInt16.MaxValue
      static member ($) (_:uint32        , _:MaxValue) = UInt32.MaxValue
      static member ($) (_:uint64        , _:MaxValue) = UInt64.MaxValue
      static member ($) (_:decimal       , _:MaxValue) = Decimal.MaxValue
      static member ($) (_:DateTime      , _:MaxValue) = DateTime.MaxValue
      static member ($) (_:DateTimeOffset, _:MaxValue) = DateTimeOffset.MaxValue
      static member ($) (_:TimeSpan      , _:MaxValue) = TimeSpan.MaxValue

  /// generic maxValue
  let inline maxValue() :'r =  Unchecked.defaultof<'r> $ MaxValue

  type MaxValue with
      static member inline ($) ((_:'a*'b         ), _:MaxValue) = maxValue(), maxValue()
      static member inline ($) ((_:'a*'b*'c      ), _:MaxValue) = maxValue(), maxValue(), maxValue()
      static member inline ($) ((_:'a*'b*'c*'d   ), _:MaxValue) = maxValue(), maxValue(), maxValue(), maxValue()
      static member inline ($) ((_:'a*'b*'c*'d*'e), _:MaxValue) = maxValue(), maxValue(), maxValue(), maxValue(), maxValue()

