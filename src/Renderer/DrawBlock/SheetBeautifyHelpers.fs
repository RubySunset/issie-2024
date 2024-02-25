module SheetBeautifyHelpers

open CommonTypes
open DrawModelType
open Symbol


//-----------------Module for beautify Helper functions--------------------------//
// Typical candidates: all individual code library functions.
// Other helpers identified by Team
module Helpers =
    let visibleSegments  (model: SheetT.Model) (wId: ConnectionId) : XYPos list =

        let wire = model.Wire.Wires[wId] // get wire from model

        /// helper to match even and off integers in patterns (active pattern)
        let (|IsEven|IsOdd|) (n: int) =
            match n % 2 with
            | 0 -> IsEven
            | _ -> IsOdd

        /// Convert seg into its XY Vector (from start to end of segment).
        /// index must be the index of seg in its containing wire.
        let getSegmentVector (index: int) (seg: BusWireT.Segment) =
            // The implicit horizontal or vertical direction  of a segment is determined by
            // its index in the list of wire segments and the wire initial direction
            match index, wire.InitialOrientation with
            | IsEven, BusWireT.Vertical
            | IsOdd, BusWireT.Horizontal -> { X = 0.; Y = seg.Length }
            | IsEven, BusWireT.Horizontal
            | IsOdd, BusWireT.Vertical -> { X = seg.Length; Y = 0. }

        /// Return a list of segment vectors with 3 vectors coalesced into one visible equivalent
        /// if this is possible, otherwise return segVecs unchanged.
        /// Index must be in range 1..segVecs
        let tryCoalesceAboutIndex (segVecs: XYPos list) (index: int) =
            if segVecs[index] =~ XYPos.zero then
                segVecs[0 .. index - 2]
                @ [ segVecs[index - 1] + segVecs[index + 1] ]
                @ segVecs[index + 2 .. segVecs.Length - 1]
            else
                segVecs

        wire.Segments
        |> List.mapi getSegmentVector
        |> (fun segVecs ->
            (segVecs, [ 1 .. segVecs.Length - 2 ])
            ||> List.fold tryCoalesceAboutIndex)


module B1 =
    let B1R (symbol : SymbolT.Symbol) = (symbol.LabelBoundingBox.H,symbol.LabelBoundingBox.W)
    let B1W (symbol:SymbolT.Symbol) (H:float) (W:float) = {symbol with LabelBoundingBox.H = H ; LabelBoundingBox.W = W }


module B2 =
    let B2W (symbol : SymbolT.Symbol) (x:float) (y:float) =
        {symbol with Pos.X =x ;Pos.Y  = y }

module B3 =

    let B3R (symbol : SymbolT.Symbol) (edge : Edge) = symbol.PortMaps.Order[edge]

    let B3W (symbol:SymbolT.Symbol) (edge : Edge) (ports : List<string>) =
        {symbol with PortMaps.Order = Map [edge,ports] }

module B4 =
    let B4R (symbol :SymbolT.Symbol) = symbol.ReversedInputPorts
    let B4W (symbol :SymbolT.Symbol) =
        {symbol with ReversedInputPorts = Some true }
module B5 =
    let B5R (port : Port) (symbol : SymbolT.Symbol) =
         symbol.Pos + getPortPos symbol port

module B6 =
    let B6R (symbol :SymbolT.Symbol) = symbol.LabelBoundingBox

module B7 =

    let B7R (symbol :SymbolT.Symbol) = symbol.STransform.Rotation

    let B7W (symbol :SymbolT.Symbol) (rotation : Rotation) = {symbol with SymbolT.Symbol.STransform.Rotation= rotation }

module B8 =

    let B8R (symbol :SymbolT.Symbol) = symbol.STransform.Flipped
    let B8W (symbol :SymbolT.Symbol) (flip : bool) = {symbol with SymbolT.Symbol.STransform.Flipped = flip }

module T1 =

    let T1R (sheet :SheetT.Model) =
        let boundingBoxIndexed =
            Map.values sheet.BoundingBoxes
            |> Seq.toList
            |> List.mapi (fun i bb -> (i, bb))

        List.allPairs boundingBoxIndexed boundingBoxIndexed
        |> List.filter (fun ((i1,bb1), (i2,bb2)) -> ((BlockHelpers.overlap2DBox bb1 bb2) && i1<>i2))
        |>List.length

module T2 =
    open Helpers
    let T2R (sheet :SheetT.Model) =
        let cIds = List.ofArray( mapKeys sheet.Wire.Wires)
        let visibleSegmentXY = List.collect (visibleSegments sheet) cIds
        let symbolsXY = Map.values sheet.Wire.Symbol.Symbols
                        |> Seq.toList
                        |> List.map (fun symbol -> symbol.Pos)
        List.allPairs symbolsXY visibleSegmentXY
        |>List.filter (fun (pos1,pos2)->pos1=~pos2)
        |>List.map snd
        |>List.distinct
        |>List.length




