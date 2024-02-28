module SheetBeautifyHelpers

open CommonTypes
open DrawModelType

open DrawModelType.BusWireT
open Electron
open Symbol
open BlockHelpers

//-----------------Module for beautify Helper functions--------------------------//
// Typical candidates: all individual code library functions.
// Other helpers identified by Team
module Helpers =
    let strictOverlap1D ((a1, a2): float * float) ((b1, b2): float * float) : bool =
        let a_min, a_max = min a1 a2, max a1 a2
        let b_min, b_max = min b1 b2, max b1 b2
        a_max > b_min && b_max > a_min
    let strictOverlap2D ((a1, a2): XYPos * XYPos) ((b1, b2): XYPos * XYPos) : bool =
        (strictOverlap1D (a1.X, a2.X) (b1.X, b2.X)) && (strictOverlap1D (a1.Y, a2.Y) (b1.Y, b2.Y))


    let strictGetWiresInBox (model: SheetT.Model) (box: BoundingBox)  : (Wire * int) list =
        let wires = (List.ofSeq (Map.values model.Wire.Wires))

        let bottomRight =
            {   X = box.TopLeft.X + box.W
                Y = box.TopLeft.Y + box.H
            }

        // State Tuple - (overlapping: bool, overlapping_wire_index: int)
        let checkOverlapFolder (startPos: XYPos) (endPos: XYPos) (state: bool * int) (segment: Segment) : bool * int =
            let strictOverlap = strictOverlap2D (startPos, endPos) (box.TopLeft, bottomRight)
            (fst state || strictOverlap), if strictOverlap then segment.Index else snd state

        List.map (fun w -> foldOverNonZeroSegs checkOverlapFolder (false, -1) w, w) wires
        |> List.filter (fun l -> fst (fst l))
        |> List.map (fun ((_, index), w) -> w, index)

    let getSegmentVector  (wire:BusWireT.Wire) (index:int)  :XYPos =
            //Takes Segment index and wire and returns XY representation
            let (|IsEven|IsOdd|) (n: int) =
                match n % 2 with
                | 0 -> IsEven
                | _ -> IsOdd

            match index, wire.InitialOrientation with
            | IsEven, Vertical | IsOdd, Horizontal -> {X=0.; Y=wire.Segments[index].Length}
            | IsEven, Horizontal | IsOdd, Vertical -> {X=wire.Segments[index].Length; Y=0.}


    ///index elements of a list
    let indexList (list :List<'T>) =  List.mapi (fun i list_element ->(i,list_element)) list
    ///form distinct pairs from a single list and ensures the same element is not combined with itself
    let distinctPairs (l1:List<'T>)=
        let l1Indexed = indexList l1
        let l2Indexed = indexList l1
        List.allPairs l1Indexed l2Indexed
        |>List.filter( fun ((i1,l1e),(i2,l2e))->  i1<>i2 )
        |>List.map ( fun ((i1,l1e),(i2,l2e))->  if i1<i2 then (i1,l1e),(i2,l2e) else (i2,l2e),(i1,l1e))
        |>List.distinct
        |>List.map (fun ((i1,l1e),(i2,l2e))-> (l1e,l2e) )


module B1 =
    let B1R (symbol : SymbolT.Symbol) = (symbol.Component.H,symbol.Component.W)
    let B1W (symbol:SymbolT.Symbol) (H:float) (W:float) = setCustomCompHW H W symbol


module B2 =
    let B2W (symbol : SymbolT.Symbol) (x:float) (y:float) =
        {symbol with Pos.X =x ;Pos.Y  = y }

module B3 =

    let B3R (symbol : SymbolT.Symbol) (edge : Edge) = symbol.PortMaps.Order[edge]

    let B3W (symbol:SymbolT.Symbol) (edge : Edge) (ports : List<string>) =

        {symbol with PortMaps.Order= Map.add edge ports symbol.PortMaps.Order }

module B4 =
    let B4R (symbol :SymbolT.Symbol) = symbol.ReversedInputPorts

    //TODO:check if i should switch state or just set to false
    let B4W (symbol :SymbolT.Symbol) =
        {symbol with ReversedInputPorts = Some true }
module B5 =
    let B5R (port : Port) (symbol : SymbolT.Symbol) =
         symbol.Pos + getPortPos symbol port

module B6 =
    let B6R (symbol :SymbolT.Symbol) = getSymbolBoundingBox symbol

module B7 =

    let B7R (symbol :SymbolT.Symbol) = symbol.STransform.Rotation

    let B7W (symbol :SymbolT.Symbol) (rotation : Rotation) = {symbol with SymbolT.Symbol.STransform.Rotation= rotation }

module B8 =

    let B8R (symbol :SymbolT.Symbol) = symbol.STransform.Flipped
    let B8W (symbol :SymbolT.Symbol) (flip : bool) = {symbol with SymbolT.Symbol.STransform.Flipped = flip }

module T1 =

    open Helpers
    let T1R (sheet :SheetT.Model) =
        let boundingBoxes =
            Map.values sheet.BoundingBoxes
            |> Seq.toList
        distinctPairs boundingBoxes
        |> List.filter (fun (bb1,bb2) ->overlap2DBox bb1 bb2)
        |>List.length


module T2 =
    open Helpers

    let T2R (sheet:SheetT.Model) =
        sheet.BoundingBoxes
        |> Map.values
        |> Seq.toList
        |> List.collect (strictGetWiresInBox sheet)
        |> List.distinct
        |>List.length



module T3 =


    open Helpers
    let T3R (sheet :SheetT.Model)=
        //Returns true if any portion of the segments are Perpendicular
        let segmentsFilter (segment1 : List<ASegment>) (segment2 : List<ASegment>)=
            List.allPairs segment1 segment2


//overlap 1d , portid same --> cross otherwise Ts and crosses
        let wires =
            sheet.Wire.Wires
            |>Map.values
            |>Seq.toList

        distinctPairs wires
        |>


module T5 =


    open Helpers
    let T5R (sheet :SheetT.Model)=









