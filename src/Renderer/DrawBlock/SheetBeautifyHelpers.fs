module SheetBeautifyHelpers

open CommonTypes
open DrawModelType

open Symbol
open BlockHelpers

//-----------------Module for beautify Helper functions--------------------------//
// Typical candidates: all individual code library functions.
// Other helpers identified by Team
module Helpers =

    let getWiresInBox (model: SheetT.Model) (box: BoundingBox)  : (BusWireT.Wire * int) list =
        let wires = (List.ofSeq (Map.values model.Wire.Wires))

        let bottomRight =
            {   X = box.TopLeft.X + box.W
                Y = box.TopLeft.Y + box.H
            }

        // State Tuple - (overlapping: bool, overlapping_wire_index: int)
        let checkOverlapFolder (startPos: XYPos) (endPos: XYPos) (state: bool * int) (segment: BusWireT.Segment) : bool * int =
            let overlap = overlap2D (startPos, endPos) (box.TopLeft, bottomRight)
            (fst state || overlap), if overlap then segment.Index else snd state

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
            | IsEven, BusWireT.Vertical | IsOdd, BusWireT.Horizontal -> {X=0.; Y=wire.Segments[index].Length}
            | IsEven, BusWireT.Horizontal | IsOdd, BusWireT.Vertical -> {X=wire.Segments[index].Length; Y=0.}


module B1 =
    let B1R (symbol : SymbolT.Symbol) = (symbol.Component.H,symbol.Component.W)
    let B1W (symbol:SymbolT.Symbol) (H:float) (W:float) = {symbol with SymbolT.Symbol.Component.H = H ; SymbolT.Symbol.Component.W =W}


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

    let T1R (sheet :SheetT.Model) =
        let boundingBoxIndexed =
            Map.values sheet.BoundingBoxes
            |> Seq.toList
            |> List.mapi (fun i bb -> (i, bb))

        List.allPairs boundingBoxIndexed boundingBoxIndexed
        |> List.filter (fun ((i1,bb1), (i2,bb2)) -> ((overlap2DBox bb1 bb2) && i1<>i2))
        |>List.map (fun ((i1, bb1), (i2, bb2)) -> if i1 < i2 then (i1, i2) else (i2, i1))
        |>List.distinct
        |>List.length


module T2 =
    open Helpers
    let T2R (sheet:SheetT.Model) =
        let boundingBoxes =
            sheet.BoundingBoxes
            |> Map.values
            |> Seq.toList

        boundingBoxes
        |> List.collect (getWiresInBox sheet)
        |> List.distinct
        |> List.map (fun (busWires, idx) -> getSegmentVector busWires idx)
        |> List.filter (fun pos -> not (pos=~XYPos.zero))
        |>List.length



module T3 =


    let T3R (sheet :SheetT.Model)=



