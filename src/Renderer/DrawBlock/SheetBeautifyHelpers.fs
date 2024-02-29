module SheetBeautifyHelpers

open CommonTypes
open DrawModelType

open DrawModelType.BusWireT
open Optics.Compose
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

    let getSegmentVector  (wire:Wire) (index:int)  :XYPos =
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
    let mergeASegmentsOnXAxis (segments:  list<ASegment>) =
            let rec mergeHelper acc current = function
                | [] -> List.rev (current :: acc)
                | next :: rest ->
                    if current.End.X >= next.Start.X then
                        let merged = {
                            current with Start = { X = min current.Start.X next.Start.X; Y = current.Start.Y };
                                         End = { X = max current.End.X next.End.X; Y = current.End.Y }
                        }
                        mergeHelper acc merged rest
                    else
                        mergeHelper (current :: acc) next rest

            match segments with
            | [] -> []
            | first :: rest -> mergeHelper [] first rest

    let mergeASegmentsOnYAxis (segments:  list<ASegment>) =
        let rec mergeHelper acc current = function
            | [] -> List.rev (current :: acc)
            | next :: rest ->
                if current.End.Y >= next.Start.Y then
                    let merged = {
                        current with Start = { Y = min current.Start.Y next.Start.Y; X = current.Start.X };
                                    End = { Y = max current.End.Y next.End.Y; X = current.End.X }
                    }
                    mergeHelper acc merged rest
                else
                    mergeHelper (current :: acc) next rest

        match segments with
        | [] -> []
        | first :: rest -> mergeHelper [] first rest

    let sameNet (sheet :SheetT.Model) (segment1 :ASegment) (segment2:ASegment)  =
        let wId1 = segment1.GetId |> snd
        let wId2 = segment2.GetId |> snd
        sheet.Wire.Wires[wId1].OutputPort = sheet.Wire.Wires[wId2].OutputPort

    let groupASegmentsByNet (sheet :SheetT.Model) (aSegments :List<ASegment>)=
        List.groupBy (fun (aSeg:ASegment) -> sheet.Wire.Wires[aSeg.GetId|>snd].OutputPort) aSegments
        |>List.map snd
    let getVisibleASegments (segments:List<ASegment>) :(ASegment list * ASegment list)=
        let horizontalSegments,verticalSegments = List.partition (fun (segment:ASegment) -> (segment.Orientation = Horizontal)) segments
        let visibleHorizontalSegments= List.groupBy (fun segment->segment.Start.Y) horizontalSegments
                                        |>List.map snd
                                        |>List.map (List.map (fun seg ->
                                            if seg.Start.X > seg.End.X then
                                              {seg with Start = {seg.Start with X = seg.End.X}; End = {seg.End with X = seg.Start.X}}
                                            else
                                              seg))
                                        |>List.map (List.sortBy (fun seg->seg.Start.X))
                                        |>List.collect mergeASegmentsOnXAxis

        let visibleVerticalSegments = List.groupBy (fun segment->segment.Start.X) verticalSegments
                                      |>List.map snd
                                      |>List.map (List.map (fun seg ->
                                            if seg.Start.Y > seg.End.Y then
                                              {seg with Start = {seg.Start with Y = seg.End.Y}; End = {seg.End with Y = seg.Start.Y}}
                                            else
                                              seg))
                                      |>List.map (List.sortBy (fun seg->seg.Start.X))
                                      |>List.collect mergeASegmentsOnYAxis
        visibleHorizontalSegments,visibleVerticalSegments


module B1 =
    let B1R (symbol : SymbolT.Symbol) = (symbol.Component.H,symbol.Component.W)
    let B1W (symbol:SymbolT.Symbol) (H:float) (W:float) = setCustomCompHW H W symbol


module B2 =

    let B2W (symbol : SymbolT.Symbol) (x:float) (y:float) =
        { symbol with Pos = { symbol.Pos with X = x; Y = y } }


module B3 =

    let B3R (symbol : SymbolT.Symbol) (edge : Edge) = symbol.PortMaps.Order[edge]

    let B3W (symbol:SymbolT.Symbol) (edge : Edge) (ports : List<string>) =


        { symbol with
            PortMaps =
                { symbol.PortMaps with
                    Order = Map.add edge ports symbol.PortMaps.Order
                }
        }


module B4 =
    // do option default value for this none == false  otherwise val

    let B4R (symbol :SymbolT.Symbol) = Option.defaultValue false symbol.ReversedInputPorts
    let B4W (symbol :SymbolT.Symbol) (state : bool) =
        {symbol with ReversedInputPorts = Some state }
module B5 =
    let B5R (port : Port) (symbol : SymbolT.Symbol) =
         symbol.Pos + getPortPos symbol port

module B6 =
    let B6R (symbol :SymbolT.Symbol) = getSymbolBoundingBox symbol

module B7 =

    let B7R (symbol :SymbolT.Symbol) = symbol.STransform.Rotation

    let B7W (symbol :SymbolT.Symbol) (rotation : Rotation) = {symbol with SymbolT.Symbol.STransform = {symbol.STransform with Rotation= rotation }}

module B8 =

    let B8R (symbol :SymbolT.Symbol) = symbol.STransform.Flipped
    let B8W (symbol :SymbolT.Symbol) (flip : bool) = {symbol with SymbolT.Symbol.STransform={symbol.STransform with Flipped =  flip  }}

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

        let allASegments =
            sheet.Wire.Wires
            |>Map.values
            |>Seq.toList
            |>List.collect getNonZeroAbsSegments

        let visibleHorizontalSegments,visibleVerticalSegments =getVisibleASegments allASegments

        let sameNet,diffNet = List.allPairs visibleHorizontalSegments  visibleVerticalSegments
                                |> List.partition (fun (s1,s2) -> sameNet sheet s1 s2)

        let sameNetCount = List.filter (fun (horizontalSegment,verticalSegment) ->strictOverlap1D (horizontalSegment.Start.X ,horizontalSegment.End.X) (verticalSegment.Start.X ,verticalSegment.End.X) && strictOverlap1D (horizontalSegment.Start.Y ,horizontalSegment.End.Y) (verticalSegment.Start.Y ,verticalSegment.End.Y)) sameNet
                            |>List.length
        let diffNetCount = List.filter (fun (horizontalSegment,verticalSegment) ->overlap1D (horizontalSegment.Start.X ,horizontalSegment.End.X) (verticalSegment.Start.X ,verticalSegment.End.X) && overlap1D (horizontalSegment.Start.Y ,horizontalSegment.End.Y) (verticalSegment.Start.Y ,verticalSegment.End.Y)) diffNet
                            |>List.length

        sameNetCount+diffNetCount



module T4 =


    open Helpers
    let T4R (sheet :SheetT.Model)=
        let allASegments =
            sheet.Wire.Wires
            |>Map.values
            |>Seq.toList
            |>List.collect getNonZeroAbsSegments


        allASegments
        |>groupASegmentsByNet sheet
        |>List.map getVisibleASegments
        |>List.collect (fun (hList,vList) -> hList@vList)
        |>List.map (fun aSeg -> aSeg.Segment)
        |>List.map (fun seg ->(abs seg.Length))
        |>List.sum



