module SheetBeautifyHelpers

open CommonTypes
open DrawModelType

open DrawModelType.BusWireT
open Optics.Compose
open Symbol
open BlockHelpers
open Optics

//-----------------Module for beautify Helper functions--------------------------//
// Typical candidates: all individual code library functions.
// Other helpers identified by Team

/// Determines if two 1D intervals have strict overlap.
/// Strict overlap in the context of geometric intervals typically means that two intervals share at least one common point, and neither interval entirely contains the other. Specifically:
/// For two intervals A and B:
/// A strict overlap occurs if there exists a point that is in both A and B.

///
/// <param name="intervalA">The first interval specified as a tuple (a1, a2) where a1 and a2 are floats.</param>
/// <param name="intervalB">The second interval specified as a tuple (b1, b2) where b1 and b2 are floats.</param>
/// <returns>Returns true if the intervals have strict overlap; otherwise, false.</returns>
let strictOverlap1D ((a1, a2): float * float) ((b1, b2): float * float) : bool =
    let a_min, a_max = min a1 a2, max a1 a2
    let b_min, b_max = min b1 b2, max b1 b2
    a_max > b_min && b_max > a_min

/// Determines if two 2D rectangles have strict overlap in both dimensions.
///
/// <param name="rectangleA">The first rectangle specified by its corners as a tuple (a1, a2) where a1 and a2 are XYPos.</param>
/// <param name="rectangleB">The second rectangle specified by its corners as a tuple (b1, b2) where b1 and b2 are XYPos.</param>
/// <returns>Returns true if the rectangles have strict overlap in both X and Y dimensions; otherwise, false.</returns>
let strictOverlap2D ((a1, a2): XYPos * XYPos) ((b1, b2): XYPos * XYPos) : bool =
    (strictOverlap1D (a1.X, a2.X) (b1.X, b2.X)) && (strictOverlap1D (a1.Y, a2.Y) (b1.Y, b2.Y))

/// Retrieves wires from a model that have segments strictly within a specified 2D bounding box. This prevents counting wires "plugged" into symbols
///
/// <param name="model">The model containing wires to check.</param>
/// <param name="box">The 2D bounding box specified by its top-left corner and dimensions.</param>
/// <returns>
/// Returns a list of pairs where each pair consists of a Wire and an index representing the overlapping segment.
/// </returns>
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

/// Retrieves the vector of a segment in a wire based on its index and the wire's orientation.
///
/// <param name="wire">The wire containing the segment.</param>
/// <param name="index">The index of the segment in the wire.</param>
/// <returns>Returns the XYPos vector of the specified segment.</returns>
let getSegmentVector  (wire:Wire) (index:int)  :XYPos =
        let (|IsEven|IsOdd|) (n: int) =
            match n % 2 with
            | 0 -> IsEven
            | _ -> IsOdd

        match index, wire.InitialOrientation with
        | IsEven, Vertical | IsOdd, Horizontal -> {X=0.; Y=wire.Segments[index].Length}
        | IsEven, Horizontal | IsOdd, Vertical -> {X=wire.Segments[index].Length; Y=0.}

/// Checks if an ASegment intersects with a specified 2D bounding box.
///
/// <param name="box">The 2D bounding box specified by its top-left corner and dimensions.</param>
/// <param name="aSegment">The ASegment to check for intersection.</param>
/// <returns>Returns true if the ASegment intersects with the bounding box; otherwise, false.</returns>
let doesSegmentIntersectsBoundingBox (box: BoundingBox) (aSegment:ASegment) =
    let toRect p1 p2 =
        let topLeft, bottomRight =
            if lThanEqualPos p1 p2 then
                p1, p2
            else
                p2, p1

        { TopLeft = topLeft
          BottomRight = bottomRight }

    let bbBottomRight =
        { X = box.TopLeft.X + box.W
          Y = box.TopLeft.Y + box.H }

    let bbRect = toRect box.TopLeft bbBottomRight
    let segRect = toRect aSegment.Start aSegment.End

    rectanglesIntersect bbRect segRect


/// Creates a new list of tuples where each tuple consists of an index and an element from the input list.
///
/// <param name="list">The input list to be indexed.</param>
/// <returns>Returns a list of tuples where each tuple contains an index and an element from the input list.</returns>
let indexList (list :List<'T>) =  List.mapi (fun i list_element ->(i,list_element)) list

/// Generates distinct pairs of elements from a list, works for objects
///
/// <param name="l1">The input list from which pairs are generated.</param>
/// <returns>Returns a list of distinct pairs of elements from the input list.</returns>
let distinctPairs (l1:List<'T>)=
    let l1Indexed = indexList l1
    let l2Indexed = indexList l1
    List.allPairs l1Indexed l2Indexed
    |>List.filter( fun ((i1,l1e),(i2,l2e))->  i1<>i2 )
    |>List.map ( fun ((i1,l1e),(i2,l2e))->  if i1<i2 then (i1,l1e),(i2,l2e) else (i2,l2e),(i1,l1e))
    |>List.distinct
    |>List.map (fun ((i1,l1e),(i2,l2e))-> (l1e,l2e) )

/// Same as distinctPairs but takes 2 lists
///
/// <typeparam name="'T">The type of elements in the lists.</typeparam>
/// <param name="l1">The first input list from which pairs are generated.</param>
/// <param name="l2">The second input list from which pairs are generated.</param>
let distinctPairs2 (l1:List<'T>) (l2:List<'T>)=
    let l1Indexed = indexList l1
    let l2Indexed = indexList l2
    List.allPairs l1Indexed l2Indexed
    |>List.filter( fun ((i1,l1e),(i2,l2e))->  i1<>i2 )
    |>List.map ( fun ((i1,l1e),(i2,l2e))->  if i1<i2 then (i1,l1e),(i2,l2e) else (i2,l2e),(i1,l1e))
    |>List.distinct
    |>List.map (fun ((i1,l1e),(i2,l2e))-> (l1e,l2e) )

/// Merges adjacent segments on the X-axis in a list of ASegments.
///
/// <param name="segments">The list of ASegments to be merged.</param>
/// <returns>Returns a new list of ASegments where adjacent segments on the X-axis are merged.</returns>
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

/// Merges adjacent segments on the Y-axis in a list of ASegments.
///
/// <param name="segments">The list of ASegments to be merged.</param>
/// <returns>Returns a new list of ASegments where adjacent segments on the Y-axis are merged.</returns>
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

/// Checks if two ASegments belong to the same net in a given sheet.
///
/// <param name="sheet">The sheet containing the wires and segments.</param>
/// <param name="segment1">The first ASegment to compare.</param>
/// <param name="segment2">The second ASegment to compare.</param>
/// <returns>Returns true if the segments belong to the same net; otherwise, false.</returns>
let sameNet (sheet :SheetT.Model) (segment1 :ASegment) (segment2:ASegment)  =
    let wId1 = segment1.GetId |> snd
    let wId2 = segment2.GetId |> snd
    sheet.Wire.Wires[wId1].OutputPort = sheet.Wire.Wires[wId2].OutputPort

/// Groups a list of ASegments by their associated nets in a given sheet.
///
/// <param name="sheet">The sheet containing the wires and segments.</param>
/// <param name="aSegments">The list of ASegments to be grouped by nets.</param>
let groupASegmentsByNet (sheet :SheetT.Model) (aSegments :List<ASegment>)=
    List.groupBy (fun (aSeg:ASegment) -> sheet.Wire.Wires[aSeg.GetId|>snd].OutputPort) aSegments
    |>List.map snd

/// Separates a list of ASegments into visible horizontal and vertical segments.
///
/// <param name="segments">The list of ASegments to be separated.</param>
/// <returns>
/// Returns a tuple containing lists of visible horizontal and vertical ASegments,
/// where visibility is determined by merging overlapping segments on the respective axes. (ie actually visible on screen)
/// </returns>
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

/// Checks if a list of ASegments forms a retracing pattern.
///
/// A retracing pattern is identified when:
/// - The list contains exactly three segments.
/// - The second segment has zero length.
/// - the first and third segment have opposite signs
///
/// <param name="aSegments">The list of ASegments to check for a retracing pattern.</param>
/// <returns>Returns true if a retracing pattern is detected; otherwise, false.</returns>
let doesRetrace (aSegments :List<ASegment>)=
        let segmentsArray = aSegments|>List.toArray

        segmentsArray.Length = 3 && segmentsArray[1].Segment.IsZero && (segmentsArray[0].Segment.Length *segmentsArray[2].Segment.Length )<0

/// Checks if an ASegment intersects with any of the provided bounding boxes.
///
/// <param name="aSegment">The ASegment to check for intersection.</param>
/// <param name="boundingBoxes">The list of bounding boxes to compare with the ASegment.</param>
/// <returns>Returns true if the ASegment intersects with any bounding box; otherwise, false.</returns>
let aSegmentIntersectsAnyBoundingBoxed (aSegment :ASegment) ( boundingBoxes :List<BoundingBox>) =
       List.filter (fun (bb) -> doesSegmentIntersectsBoundingBox bb aSegment) boundingBoxes
       |>List.isEmpty


/// Gets the dimensions (height, width) of a custom component from a Symbol.
///
/// <param name="symbol">The Symbol representing the custom component.</param>
/// <returns>Returns a tuple (height, width) representing the dimensions of the custom component.</returns>
let getDimensionOfComponent (symbol : SymbolT.Symbol) = (symbol.Component.H,symbol.Component.W)

/// Sets the dimensions (height, width) of a custom component in a Symbol.
/// <param name="H">The new height value for the custom component.</param>
/// <param name="W">The new width value for the custom component.</param>
/// <param name="symbol">The Symbol representing the custom component.</param>
/// <returns>Returns a new Symbol with updated dimensions for the custom component.</returns>
let setDimensionOfComponent  (H:float,W:float) (symbol:SymbolT.Symbol) = setCustomCompHW H W symbol

/// Lens for accessing and modifying the dimensions (height, width) of a custom component in a Symbol.
let customComponentDimension_ = Lens.create getDimensionOfComponent setDimensionOfComponent

/// Sets the position (X, Y) of a Symbol.
///
/// <param name="symbol">The Symbol to be repositioned.</param>
/// <param name="x">The new X-coordinate for the Symbol's position.</param>
/// <param name="y">The new Y-coordinate for the Symbol's position.</param>
/// <returns>Returns a new Symbol with updated position coordinates.</returns>
let setPositionOfSymbol (symbol : SymbolT.Symbol) (x:float) (y:float) =
    { symbol with Pos = {  X = x; Y = y } }



/// Gets the ports associated with a specific edge in a Symbol.
///
/// <param name="symbol">The Symbol containing port information.</param>
/// <param name="edge">The Edge for which ports are retrieved.</param>
/// <returns>Returns a list of ports associated with the specified edge in the Symbol.</returns>
let getPortsOfSymbol (edge : Edge) (symbol : SymbolT.Symbol) = symbol.PortMaps.Order[edge]

/// Sets the ports for a specific edge in a Symbol.
///
/// <param name="symbol">The Symbol to be updated with new port information.</param>
/// <param name="edge">The Edge for which ports are set.</param>
/// <param name="ports">The list of ports to be associated with the specified edge.</param>
/// <returns>Returns a new Symbol with updated port information for the specified edge.</returns>
let setPortsOfSymbol (symbol:SymbolT.Symbol) (edge : Edge) (ports : List<string>) =
    { symbol with
        PortMaps =
            { symbol.PortMaps with
                Order = Map.add edge ports symbol.PortMaps.Order
            }
    }


/// Gets the reverse state of the input ports for a Mux2 symbol.
///
/// <param name="symbol">The Mux2 symbol to retrieve the reverse state from.</param>
/// <returns>Returns true if the input ports are reversed; otherwise, false.</returns>
let getReverseStateOfMux2 (symbol :SymbolT.Symbol) = Option.defaultValue false symbol.ReversedInputPorts

/// Sets the reverse state of the input ports for a Mux2 symbol.
///
/// <param name="state">The new reverse state to set for the input ports.</param>
/// <param name="symbol">The Mux2 symbol to be updated with the new reverse state.</param>
/// <returns>Returns a new Mux2 symbol with the updated reverse state for the input ports.</returns>
let setReverseStateOfMux2  (state : bool) (symbol :SymbolT.Symbol) =
    {symbol with ReversedInputPorts = Some state }

/// Lens for accessing and modifying the reverse state of the input ports for a Mux2 symbol.
let reverseStateMux2_ = Lens.create getReverseStateOfMux2 setReverseStateOfMux2


/// Gets the absolute position on the sheet of a specific port within a Symbol.
///
/// <param name="port">The Port for which to retrieve the position.</param>
/// <param name="symbol">The Symbol containing the specified port.</param>
/// <returns>Returns the absolute position of the specified port on the sheet within the Symbol.</returns>
let getPortPosition (port : Port) (symbol : SymbolT.Symbol) =
     symbol.Pos + getPortPos symbol port

/// Gets the outline of the bounding box for a Symbol.
///
/// <param name="symbol">The Symbol for which to retrieve the bounding box outline.</param>
/// <returns>Returns the bounding box outline for the specified Symbol.</returns>
let getSymbolBoundingBoxOutline (symbol :SymbolT.Symbol) = getSymbolBoundingBox symbol

/// Gets the rotation state of a Symbol.
///
/// <param name="symbol">The Symbol for which to retrieve the rotation state.</param>
/// <returns>Returns the rotation state of the specified Symbol.</returns>
let getRotationState (symbol :SymbolT.Symbol) = symbol.STransform.Rotation

/// Sets the rotation state for a Symbol.
///
/// <param name="rotation">The new rotation state to set for the Symbol.</param>
/// <param name="symbol">The Symbol to be updated with the new rotation state.</param>
/// <returns>Returns a new Symbol with the updated rotation state.</returns>
let setRotationState  (rotation : Rotation) (symbol :SymbolT.Symbol) = {symbol with SymbolT.Symbol.STransform = {symbol.STransform with Rotation= rotation }}


/// Lens for accessing and modifying the rotation state of a Symbol
let rotationState_ = Lens.create getRotationState setRotationState

/// Gets the flipped state of a Symbol.
///
/// <param name="symbol">The Symbol for which to retrieve the flipped state.</param>
/// <returns>Returns true if the Symbol is flipped; otherwise, false.</returns>
let getFlippedState (symbol :SymbolT.Symbol) = symbol.STransform.Flipped

/// Sets the flipped state for a Symbol.
///
/// <param name="flip">The new flipped state to set for the Symbol.</param>
/// <param name="symbol">The Symbol to be updated with the new flipped state.</param>
/// <returns>Returns a new Symbol with the updated flipped state.</returns>
let setFlippedState  (flip : bool) (symbol :SymbolT.Symbol) = {symbol with SymbolT.Symbol.STransform={symbol.STransform with Flipped =  flip  }}


/// Lens for accessing and modifying the flipped state of a Symbol.
let flippedState_ = Lens.create getFlippedState setFlippedState


/// Counts the number of pairs of symbols that intersect each other in the given sheet.
///
/// This function considers all pairs of symbols in the sheet and calculates the
/// distinct number of pairs where the symbols intersect.
///
/// <param name="sheet">The sheet model containing symbols and bounding boxes.</param>
/// <returns>Returns the count of distinct pairs of symbols that intersect with each other.</returns>
let numOfPairOfSymbolsIntersecting (sheet :SheetT.Model) =
    let boundingBoxes =
        Map.values sheet.BoundingBoxes
        |> Seq.toList
    distinctPairs boundingBoxes
    |> List.filter (fun (bb1,bb2) ->overlap2DBox bb1 bb2)
    |>List.length


/// Counts the number of distinct wire visible segments that intersect with one or more symbols.
///
/// This function considers all visible wire segments in the sheet and calculates
/// the number of segments that intersect with one or more symbols.
///
/// <param name="sheet">The sheet model containing symbols and wire segments.</param>
/// <returns>Returns the count of distinct wire visible segments that intersect with symbols.</returns>
let  numOfSymbolSegmentsIntersecting(sheet:SheetT.Model) =
    sheet.BoundingBoxes
    |> Map.values
    |> Seq.toList
    |> List.collect (strictGetWiresInBox sheet)
    |> List.distinct
    |>List.length





/// Counts the number of segment pairs crossing in the given sheet.
///
/// This function considers all visible wire segments in the sheet and calculates
/// the number of pairs of segments that cross each other, considering both the same
/// and different nets. Same nets consider +'s only different nets consider both T and +
///
/// <param name="sheet">The sheet model containing wire segments.</param>
/// <returns>Returns the count of segment pairs that cross each other.</returns>
let numOfSegmentPairsCrossing (sheet :SheetT.Model)=

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

/// Calculates the total length of visible wire segments in the given sheet.
///
/// This function considers all visible wire segments in the sheet and calculates
/// the total length of these segments.
///
/// <param name="sheet">The sheet model containing wire segments.</param>
/// <returns>Returns the total length of visible wire segments in the sheet.</returns>
let totalVisibleWireLength (sheet :SheetT.Model)=
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


///  The total number of visible right-angles in wires in the given sheet.
/// <param name="sheet">The sheet model containing wire segments.</param>
/// <returns>Returns the total number of visible right-angles in wires</returns>
let numOfVisibleRightAngles (sheet :SheetT.Model)=
    let allASegments =
        sheet.Wire.Wires
        |>Map.values
        |>Seq.toList
        |>List.map getNonZeroAbsSegments


    let allASegmentsPartitioned = List.map (List.partition (fun (segment:ASegment) -> (segment.Orientation = Horizontal))) allASegments
    List.map (fun (l1,l2)->List.allPairs l1 l2) allASegmentsPartitioned
    |>List.map (List.filter (fun (horizontalSegment,verticalSegment) ->overlap1D (horizontalSegment.Start.X ,horizontalSegment.End.X) (verticalSegment.Start.X ,verticalSegment.End.X) && overlap1D (horizontalSegment.Start.Y ,horizontalSegment.End.Y) (verticalSegment.Start.Y ,verticalSegment.End.Y) && (sameNet sheet verticalSegment horizontalSegment)))
    |>(List.collect id)
    |>List.length


/// Finds and categorizes retracing wire segments in the given sheet.
///
/// Retracing segments are identified as zero-length segments in a wire with non-zero segments
/// on either side that have lengths of opposite signs. This function partitions all wire segments
/// into triplets, checks for retracing segments in each triplet, and categorizes them. Additionally,
/// it identifies start and end segments that intersect with symbols.
///
/// <param name="sheet">The sheet model containing wire segments.</param>
/// <returns>
/// Returns a tuple containing two lists:
/// - The first list contains all retracing segments found in the wire.
/// - The second list contains segments at the start and end of wires that intersect with symbols.
/// </returns>
let findRetracingSegments (sheet :SheetT.Model) =


    let symbolsBoundingBoxes = sheet.Wire.Symbol.Symbols
                                |>Map.values
                                |>Seq.toList
                                |>List.map (fun symbol -> symbol.SymbolBoundingBox)


    let allASegments =
        sheet.Wire.Wires
        |>Map.values
        |>Seq.toList
        |>List.map getAbsSegments

    let allASegmentsPartitioned = allASegments
                                |>List.map (List.windowed 3)

    let allRetracingSegments =
                                                allASegmentsPartitioned
                                                |> List.map (fun outerList ->
                                                    outerList
                                                    |> List.filter doesRetrace)
                                                |> List.collect (List.collect id)

    let allStartingSegments= List.map (List.map List.head ) allASegmentsPartitioned
    let allEndingSegments= List.map (List.map List.last ) allASegmentsPartitioned
    let intersectingStartSegmentsWithSymbols = allStartingSegments
                                               |> List.filter doesRetrace
                                               |>List.map List.last
                                               |>List.filter ( fun aSegment -> aSegmentIntersectsAnyBoundingBoxed aSegment symbolsBoundingBoxes)

    let intersectingEndSegmentsWithSymbols = allStartingSegments
                                               |> List.filter doesRetrace
                                               |>List.map List.head
                                               |>List.filter ( fun aSegment -> aSegmentIntersectsAnyBoundingBoxed aSegment symbolsBoundingBoxes)

    (allRetracingSegments , intersectingStartSegmentsWithSymbols@intersectingEndSegmentsWithSymbols)






