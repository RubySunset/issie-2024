module SheetBeautifyHelpers
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open Optics
open CommonTypes
open Symbol
open RotateScale
open SymbolHelpers
open BlockHelpers
open SymbolUpdate
open EEExtensions


//-----------------Module for beautify Helper functions--------------------------//
// Typical candidates: all individual code library functions.
// Other helpers identified by Team

// IMPORTANT NOTE:
// Functions used for B1-B8 are only used to get and set fields of symbols and not update them in the sheet 
// as there are functions in the codebase already that do the updating of the symbols in the sheet and using them here 
// is redundant and unnecessary. This confusion was clarified by me asking the question on ed #229.
// Update sheet fucntions already are there and can be used later to update the bounding boxes, etc later if needed.


(* B1 *)

/// <summary>
/// Retrieves the dimensions of a custom symbol, factoring in any rotations and scaling.
/// </summary>
/// <param name="customSymbol">The custom symbol from which to get dimensions.</param>
/// <returns>
/// A tuple representing the width (x-axis dimension) and height (y-axis dimension) of the custom symbol.
/// </returns>
let cusotmCompDimGetter (customSymbol : SymbolT.Symbol) : (float * float) = 
    // get the dimentions of a custom symbol considering any rotationa and scaling
    // let dims = (getCustomSymCorners customSymbol)[2]
    // (dims.X, dims.Y)
    let width = customSymbol.Component.W
    let height = customSymbol.Component.H
    width , height

/// <summary>
/// Sets new dimensions for a custom symbol's component.
/// </summary>
/// <param name="width">The new width (x-axis dimension) for the custom symbol.</param>
/// <param name="height">The new height (y-axis dimension) for the custom symbol.</param>
/// <param name="customSymbol">The custom symbol whose dimensions are to be updated.</param>
/// <returns>
/// A new symbol instance with updated dimensions.
/// </returns>
let customCompDimSetter ( width : float , height : float ) (customSymbol : SymbolT.Symbol) : SymbolT.Symbol = 
    // Alternative method
    // setCustomCompHW height width customSymbol
    { customSymbol with Component = {customSymbol.Component with W = width; H = height} }

/// <summary>
/// A lens for getting and setting the dimensions of a custom component as a tuple.
/// </summary>
/// <remarks>
/// The lens abstracts the operations for accessing and modifying the width and height of a custom symbol's component.
/// </remarks>
let customCompDimLens_ = Lens.create cusotmCompDimGetter customCompDimSetter

(* B2W *)
/// <summary>
/// Updates the position of a symbol to a new specified position.
/// </summary>
/// <param name="newPos">The new position (XYPos) to set for the symbol.</param>
/// <param name="symbol">The symbol to be updated with the new position.</param>
/// <returns>
/// A new instance of the symbol with its position updated to the specified newPos.
/// </returns>
let symbolPosSetter (newPos : XYPos) (symbol : SymbolT.Symbol) : SymbolT.Symbol = 
    // alternative way to do this using functions that already exist in the codebase
    // let offset = newPos - symbol.Pos
    // moveSymbol offset symbol
    { symbol with Pos = newPos }


(* B3 *)

/// <summary>
/// Retrieves the order of ports for a given side of a symbol.
/// </summary>
/// <param name="symbol">The symbol from which to retrieve port order.</param>
/// <param name="side">The side (Edge) of the symbol for which the port order is requested.</param>
/// <returns>
/// A list of strings representing the ordered port IDs on the specified side of the symbol.
/// </returns>
let symbolEdgePortOrderGetter (symbol : SymbolT.Symbol) (side : Edge) : list<string> = 
    // get portmaps of thhe symbol
    let portMaps = symbol |> fst portMaps_
    // get order field of portMaps
    let order = portMaps |> fst order_
    // return the list of ports in order on the given side in correct order
    order[side]
    // could do all in one line maybe less readable
    //symbol.PortMaps.Order[side]

/// <summary>
/// Sets a new order for the ports on a specific side of a symbol.
/// </summary>
/// <param name="newOrder">A list of strings representing the new order of port.</param>
/// <param name="side">The side (Edge) of the symbol where the order will be applied.</param>
/// <param name="symbol">The symbol to be updated with the new port order.</param>
/// <returns>
/// A new instance of the symbol with the port order on the specified side updated.
/// </returns>
let symbolEdgePortOrderSetter (newOrder : list<string>) (side : Edge) (symbol : SymbolT.Symbol) : SymbolT.Symbol = 
    // get the current map using lens
    let currPortOrder = symbol |> fst portMaps_ |> fst order_
    // make the new order
    let newPortOrder = Map.add side newOrder currPortOrder
    // set new order returning a new PortMaps using lens
    let newPortMaps = (newPortOrder, symbol.PortMaps) ||> snd order_
    // set the new PortMaps in the symbol using lens
    (newPortMaps, symbol) ||> snd portMaps_
    // could do all in one line maybe less readable
    // {symbol with PortMaps = {symbol.PortMaps with Order = Map.add side newOrder symbol.PortMaps.Order}}


(* B4 *)

/// <summary>
/// Retrieves the reversed input ports state for MUX and DEMUX components.
/// </summary>
/// <param name="symbol">The symbol (MUX or DEMUX) for which to retrieve the state.</param>
/// <returns>
/// A boolean indicating if the input ports are reversed. Returns false for symbols not applicable or if the field is absent, indicating old circuits.
/// </returns>
let MuxDemuxReversedInputPortsGetter (symbol : SymbolT.Symbol) : bool = 
    let MuxDemuxLst = [Mux2; Mux4; Mux8; Demux2; Demux4; Demux8]
    let isMuxOrDemux = List.exists (fun x -> x = symbol.Component.Type) MuxDemuxLst

    match symbol.ReversedInputPorts with
    // for the old circuits where this field is not present hence return the normal state which is considered to be false according to ed stem posts
    // | None when isMuxOrDemux -> false
    | Some state when isMuxOrDemux -> state
    // for components that are not Mux or Demux, return false since they don't have this field or it is already set to false
    // referencing to Symbol.fs lines 707 and 622
    | _ -> false    

/// <summary>
/// Sets the reversed input ports state for MUX and DEMUX components.
/// </summary>
/// <param name="state">The new state to set for the ReversedInputPorts property.</param>
/// <param name="symbol">The symbol (MUX or DEMUX) to update with the new state.</param>
/// <returns>
/// A new instance of the symbol with the ReversedInputPorts property updated to the specified state, if applicable.
/// </returns>
let MuxDemuxReversedInputPortsSetter (state : bool) (symbol : SymbolT.Symbol) : SymbolT.Symbol = 
    // { symbol with ReversedInputPorts = reversed }
    let MuxDemuxLst = [Mux2; Mux4; Mux8; Demux2; Demux4; Demux8]
    let isMuxOrDemux = List.exists (fun x -> x = symbol.Component.Type) MuxDemuxLst
    match isMuxOrDemux with
    | true -> { symbol with ReversedInputPorts = Some state }
    // not changing the field if the symbol is not a MUX or DEMUX since the symbol boolean is only used for MUX and DEMUX (ed stem #143)
    | false -> symbol

// NOTE: LENS SHOULD ONLY BE USED FOR MUX AND DEMUX COMPONENTS ACCORDING TO SPECIFICATION BUT OTHER CASES HAVE BEEN HANDLED
/// <summary>
/// A lens for managing the ReversedInputPorts property of MUX and DEMUX components in a functional manner.
/// </summary>
let MuxDemuxReversedInputPortsLens_ = Lens.create MuxDemuxReversedInputPortsGetter MuxDemuxReversedInputPortsSetter


(*B5*)

/// <summary>
/// Calculates the absolute position of a port on the sheet relative to the symbol's position.
/// </summary>
/// <param name="symbol">The symbol containing the port.</param>
/// <param name="port">The port whose position is to be calculated.</param>
/// <returns>
/// An XYPos representing the absolute position of the port on the sheet.
/// </returns>
let getPortPosOnSheet (symbol : SymbolT.Symbol) (port : Port) : XYPos=
    // get the offset of port relative to topleft pos of symbol
    let offset = getPortPos symbol port
    // add the offset to the Pos of the symbol, you need operator overloading for this, open Operators maybe
    symbol.Pos + offset


(*B6*)
/// <summary>
/// Retrieves the bounding box of a symbol.
/// </summary>
/// <param name="symbol">The symbol for which the bounding box is to be retrieved.</param>
/// <returns>
/// A BoundingBox structure that encapsulates the top-left position, width, and height of the symbol, defining the rectangular area that the symbol occupies on the sheet.
/// </returns>
let getBoundingBox (symbol : SymbolT.Symbol) : BoundingBox = 
    getSymbolBoundingBox symbol


(*B7*)
/// <summary>
/// Retrieves the STransform of a symbol.
/// </summary>
/// <param name="symbol">The symbol whose STransform is being retrieved.</param>
/// <returns>
/// An STransform structure containing the rotation and flip state of the symbol.
/// </returns>
let getSTransform (symbol : SymbolT.Symbol) : STransform = 
    symbol.STransform

/// <summary>
/// Sets the STransform for a symbol.
/// </summary>
/// <param name="sTransform">The new STransform to be applied to the symbol.</param>
/// <param name="symbol">The symbol to update with the new STransform.</param>
/// <returns>
/// A new symbol instance with the updated STransform.
/// </returns>
let setSTransform (sTransform : STransform) (symbol : SymbolT.Symbol) : SymbolT.Symbol = 
    { symbol with STransform = sTransform }

/// <summary>
/// A lens for getting and setting the STransform of a symbol.
/// </summary>
let sTransformLens_ = Lens.create getSTransform setSTransform

/// <summary>
/// Retrieves the rotation component of an STransform.
/// </summary>
/// <param name="STransform">The STransform from which to get the rotation.</param>
/// <returns>
/// The rotation of the STransform.
/// </returns>
let getRotation (sTransform : STransform) : Rotation = 
    sTransform.Rotation

/// <summary>
/// Sets the rotation component of an STransform.
/// </summary>
/// <param name="rotation">The new rotation to set.</param>
/// <param name="sTransform">The STransform to update with the new rotation.</param>
/// <returns>
/// An STransform with the updated rotation.
/// </returns>
let setRotation (rotation : Rotation) (sTransform : STransform) : STransform = 
    { sTransform with Rotation = rotation }

/// <summary>
/// A lens for getting and setting the rotation of an STransform.
/// </summary>
let rotationLens_ = Lens.create getRotation setRotation

/// <summary>
/// Retrieves the rotation state of a symbol.
/// </summary>
/// <param name="symbol">The symbol whose rotation state is being retrieved.</param>
/// <returns>
/// The rotation state of the symbol as a Rotation value.
/// </returns>
let rotationStateOfSymbolGetter (symbol : SymbolT.Symbol) : Rotation = 
    // symbol.STransform.Rotation
    symbol |> fst sTransformLens_ |> fst rotationLens_

/// <summary>
/// Sets the rotation state for a symbol.
/// </summary>
/// <param name="rotationState">The new rotation state to apply to the symbol.</param>
/// <param name="symbol">The symbol to update with the new rotation state.</param>
/// <returns>
/// A symbol with the updated rotation state.
/// </returns>
let rotationStateOfSymbolSetter (rotationState : Rotation) (symbol : SymbolT.Symbol) : SymbolT.Symbol = 
    // alternative to do everything in one line
    // { symbol with STransform = {symbol.STransform with Rotation = rotationState} }
    // rotateSymbolByDegree rotationState symbol
    let oldSTransform = symbol |> fst sTransformLens_
    let newSTransform = (rotationState, oldSTransform) ||> snd rotationLens_
    (newSTransform, symbol) ||> snd sTransformLens_

/// <summary>
/// A lens for getting and setting the rotation state of a symbol.
/// </summary>
let rotationStateOfSymbolLens_ = Lens.create rotationStateOfSymbolGetter rotationStateOfSymbolSetter


(*B8*)

/// <summary>
/// Retrieves the flip state from an STransform structure.
/// </summary>
/// <param name="sTransform">The STransform structure from which to get the flip state.</param>
/// <returns>
/// A boolean indicating the flip state of the symbol; true if flipped, false otherwise.
/// </returns>
let getFlipState (sTransform : STransform) : bool = 
    sTransform.Flipped

/// <summary>
/// Sets the flip state in an STransform structure.
/// </summary>
/// <param name="flipState">The new flip state to set (true for flipped, false for not flipped).</param>
/// <param name="sTransform">The STransform structure to update with the new flip state.</param>
/// <returns>
/// An STransform structure with the updated flip state.
/// </returns>
let setFlipState (flipState : bool) (sTransform : STransform) : STransform = 
    { sTransform with Flipped = flipState }

/// <summary>
/// A lens for getting and setting the flip state of an STransform structure.
/// </summary>
let flipStateLens_ = Lens.create getFlipState setFlipState

/// <summary>
/// Retrieves the flip state of a symbol using the STransform structure.
/// </summary>
/// <param name="symbol">The symbol from which to get the flip state.</param>
/// <returns>
/// A boolean indicating the flip state of the symbol; true if flipped, false otherwise.
/// </returns>
let flipStateOfSymbolGetter (symbol : SymbolT.Symbol) : bool = 
    symbol |> fst sTransformLens_ |> fst flipStateLens_

/// <summary>
/// Sets the flip state for a symbol using its STransform structure.
/// </summary>
/// <param name="flipState">The new flip state to apply (true for flipped, false for not flipped).</param>
/// <param name="symbol">The symbol to update with the new flip state.</param>
/// <returns>
/// A symbol with the updated flip state.
/// </returns>
let flipStateOfSymbolSetter (flipState : bool) (symbol : SymbolT.Symbol) : SymbolT.Symbol = 
    // alternative to do everything in one line
    // { symbol with STransform = {symbol.STransform with Flipped = flipState} }
    // or
    // flipSymbolInBlock flipState symbol.CentrePos symbol
    let oldSTransform = symbol |> fst sTransformLens_
    let newSTransform = (flipState, oldSTransform) ||> snd flipStateLens_
    (newSTransform, symbol) ||> snd sTransformLens_

/// <summary>
/// A lens for getting and setting the flip state of a symbol.
/// </summary>
let flipStateOfSymbolLens_ = Lens.create flipStateOfSymbolGetter flipStateOfSymbolSetter






(*T FUCNTIONS BEGIN*)

(*T1*)

/// <summary>
/// This function ensures no duplicate pairs and no mirrored pairs are generated.
/// </summary>
/// <param name="list">The list from which to generate pairs.</param>
/// <returns>
/// A list of unique pairs (tuples) where each pair consists of elements from different positions in the original list.
/// </returns>
let createPairs (list : 'a list) : list<'a * 'a> =
    let indexedList = List.mapi (fun index value -> (index, value)) list
    let allPairs = 
        List.collect (fun (index1, value1) -> 
            List.choose (fun (index2, value2) -> 
                if index1 < index2 then Some (value1, value2) else None) indexedList) indexedList
    allPairs

/// <summary>
/// Calculates the number of pairs of bounding boxes that intersect each other from a given model.
/// Utilizes the createPairs function to generate all possible pairs of bounding boxes and then filters them based on intersection.
/// </summary>
/// <param name="model">The model containing bounding boxes of symbols.</param>
/// <returns>
/// An integer representing the count of intersecting pairs of bounding boxes.
/// </returns>
let PairsOfIntersectingBoxes (model : SheetT.Model) : int = 
    let boundingBoxesMap = model.BoundingBoxes
    let lstOfBB = boundingBoxesMap |> Map.toSeq |> Seq.map snd |> Seq.toList
    let pairsOfBB = createPairs lstOfBB
    pairsOfBB |> List.filter (fun (x, y) -> overlap2DBox x y) |> List.length


(*T2*)

/// <summary>
/// Identifies and transforms wire segments into a list of visible XY position vectors, potentially coalescing consecutive segments into single vectors based on visibility.
/// </summary>
/// <param name="wId">The unique identifier for a wire within the model.</param>
/// <param name="model">The model containing wires and their segments.</param>
/// <returns>
/// A list of XY position vectors representing the visible segments of the specified wire.
/// </returns>
let visibleSegments (wId: ConnectionId) (model: SheetT.Model) : XYPos list =

    let wire = model.Wire.Wires[wId] // get wire from model

    let (|IsEven|IsOdd|) (n: int) = match n % 2 with | 0 -> IsEven | _ -> IsOdd

        /// Convert seg into its XY Vector (from start to end of segment).
        /// index must be the index of seg in its containing wire.
    let getSegmentVector (index:int) (seg: BusWireT.Segment) : XYPos =
            // The implicit horizontal or vertical direction  of a segment is determined by 
            // its index in the list of wire segments and the wire initial direction
        match index, wire.InitialOrientation with
        | IsEven, BusWireT.Vertical | IsOdd, BusWireT.Horizontal -> {X=0.; Y=seg.Length}
        | IsEven, BusWireT.Horizontal | IsOdd, BusWireT.Vertical -> {X=seg.Length; Y=0.}

        /// Return a list of segment vectors with 3 vectors coalesced into one visible equivalent
        /// if this is possible, otherwise return segVecs unchanged.
        /// Index must be in range 1..segVecs
    let tryCoalesceAboutIndex (segVecs: XYPos list) (index: int) : list<XYPos> =
        if segVecs[index] =~ XYPos.zero
        then
            segVecs[0..index-2] @
            [segVecs[index-1] + segVecs[index+1]] @
            segVecs[index+2..segVecs.Length - 1]
        else
            segVecs

    wire.Segments
    |> List.mapi getSegmentVector
    |> (fun segVecs ->
            (segVecs,[1..segVecs.Length-2])
            ||> List.fold tryCoalesceAboutIndex)

/// <summary>
/// Determines if a given wire segment intersects with a bounding box.
/// </summary>
/// <param name="box">The bounding box to check intersection against.</param>
/// <param name="segment">The wire segment being analyzed.</param>
/// <returns>
/// A boolean indicating whether the segment intersects with the bounding box.
/// </returns>
let segIntersectingBB (box : BoundingBox) (segment : BusWireT.ASegment) : bool = 
    let intersectOption = segmentIntersectsBoundingBox box segment.Start segment.End
    match intersectOption with
    | Some _ -> true
    | None -> false

/// <summary>
/// Counts the number of visible wire segments intersecting any symbol within the model.
/// </summary>
/// <param name="model">The model containing wires and symbols represented as bounding boxes.</param>
/// <returns>
/// The total count of visible wire segments that intersect with any symbol's bounding box within the model.
/// </returns>
let numVisSegsIntersectingSymbols (model : SheetT.Model) : int = 
    // map of bounding boxes
    let BBMap = model.BoundingBoxes
    // List of bounding boxes
    let lstOfBB = BBMap |> Map.toSeq |> Seq.map snd |> Seq.toList
    let lstOfWires = model.Wire.Wires |> Map.toSeq |> Seq.map snd |> Seq.toList
    // list of visible segments
    let lstOfVisibleSegments = lstOfWires |> List.collect (fun wire -> getNonZeroAbsSegments wire)

    lstOfVisibleSegments
    |> List.map (fun seg -> lstOfBB |> List.map (fun bb -> (seg, bb)))
    |> List.map (fun x -> List.map (fun (seg, bb) -> segIntersectingBB bb seg) x)
    |> List.map (fun x -> List.exists (fun y -> y = true) x)
    |> List.filter (fun x -> x = true)
    |> List.length


(*T3*)

/// <summary>
/// Calculates the count of right angle intersections between wire segments from different nets within the model.
/// </summary>
/// <param name="model">The model containing all wires and their segments.</param>
/// <returns>
/// The total count of unique right angle intersections between wire segments belonging to different nets.
/// </returns>
let rightAngleSegCount (model : SheetT.Model) : int = 
    // list of all wires in model
    let allWiresInSheet = model.Wire.Wires |> Map.toSeq |> Seq.map snd |> Seq.toList 
    // list of all unique pairs of wires
    let uniqueWirePairs = createPairs allWiresInSheet
    //  list of all unique pairs of wires with different nets
    let uniqueWirePairsDiffNet = uniqueWirePairs |> List.filter (fun (w1, w2) -> w1.OutputPort <> w2.OutputPort)
    // get all paris of segments of two wires
    let getSegmentPairsOfWires (wire1 : Wire) (wire2 : Wire) : list<ASegment * ASegment> = 
        let wire1Segments = getNonZeroAbsSegments wire1
        let wire2Segments = getNonZeroAbsSegments wire2
        List.allPairs wire1Segments wire2Segments
    // list of all segment paris within the sheet from different nets
    let segmentPairsOfWires = List.collect (fun (w1, w2) -> getSegmentPairsOfWires w1 w2) uniqueWirePairsDiffNet
    // return true if the segment pair is a right angle pair
    let perpSegmentPair (segment1 : ASegment) (segment2 : ASegment) : bool = 
        let seg1Dir = segment1.End - segment1.Start
        let seg2Dir = segment2.End - segment2.Start
        let dotProd = seg1Dir.X * seg2Dir.X + seg1Dir.Y * seg2Dir.Y
        match dotProd with
        | 0.0 -> true
        | _ -> false
    
    let segmentCross (segment1 : ASegment) (segment2 : ASegment) : bool = 
        overlap1D (segment1.Start.X, segment1.End.X) (segment2.Start.X, segment2.End.X) && overlap1D (segment1.Start.Y, segment1.End.Y) (segment2.Start.Y, segment2.End.Y)
    
    segmentPairsOfWires
    |> List.filter (fun (seg1, seg2) -> perpSegmentPair seg1 seg2 && segmentCross seg1 seg2)
    |> List.distinct
    |> List.length



(*T4*)

/// <summary>
/// Calculates the total visible length of all wire segments within the model, accounting for overlaps within the same net.
/// </summary>
/// <param name="model">The model containing all wires and their segments.</param>
/// <returns>
/// The total visible length of wire segments as a float, after subtracting the lengths of overlapping segments within the same net.
/// </returns>
let totalVisWireLen (model : SheetT.Model) : float = 
    let lstSegments = model.Wire.Wires |> Map.toSeq |> Seq.map snd |> Seq.toList |> List.collect getAbsSegments
    let sumSegmentLength = lstSegments |> List.fold (fun acc segment -> acc + segment.Segment.Length) 0.0

    // grouping wires into sublist based on their outputports, which is helpful later for handling same-net segment overlapping
    let partitionedWireIntoNets = BlockHelpers.partitionWiresIntoNets model.Wire

    // Helper function to calculate the overlap between two 1D segments on the same axis
    let calculateOverlap (start1 : float) (end1 : float) (start2 : float) (end2 : float) : float = 
        let sortedPoints = [start1; end1; start2; end2] |> List.sort
        let overlapStart = max start1 start2
        let overlapEnd = min end1 end2
        max 0.0 (overlapEnd - overlapStart)
    
    let measureSegmentOverlap (segment1 : ASegment) (segment2 : ASegment) : float =
        //check id both segments are vertical or horizontal
        if (segment1.Start.X = segment1.End.X && segment2.Start.X = segment2.End.X) || (segment1.Start.Y = segment1.End.Y && segment2.Start.Y = segment2.End.Y) then
            if segment1.Start.X = segment1.End.X then
                // if both are vertical, calculate Y overlap
                calculateOverlap segment1.Start.Y segment1.End.Y segment2.Start.Y segment2.End.Y
            else 
                // segments are horizontal, calculate X overlap
                calculateOverlap segment1.Start.X segment1.End.X segment2.Start.X segment2.End.X
        else
            // segments are not parallel, thus cannot overlap
            0.0
    
    let lenOverlapSegForWires (ws : BusWireT.Wire list) : float =
        let segments = ws |> List.collect getAbsSegments
        let uniqueSegmentPairs = createPairs segments
        let overlappingSegments = uniqueSegmentPairs |> List.filter (fun (seg1, seg2) -> measureSegmentOverlap seg1 seg2 > 0.0)
        overlappingSegments |> List.fold (fun acc (seg1, seg2) -> acc + measureSegmentOverlap seg1 seg2) 0.0 
    
    let sumOverlapSegments = partitionedWireIntoNets |> List.map snd |> List.map (List.map snd) |> List.map lenOverlapSegForWires |> List.sum

    sumSegmentLength - sumOverlapSegments


(*T5*)

/// <summary>
/// Calculates the total number of right angles formed by visible segments of wires within the model.
/// </summary>
/// <param name="model">The model containing all wires and their segments.</param>
/// <returns>
/// An integer representing the total count of right angles formed by the visible segments of wires.
/// </returns>
let perpWireCount (model : SheetT.Model) : int = 
    // this is just the number of vectors given by the function visibleSegments which are perpendicular to each other hence there would be 
    // the length of this list minus 1 wire right-angles

    let conIdList = model.Wire.Wires |> Map.toSeq |> Seq.map fst |> Seq.toList
    // use this list to extract the list of visible segments per wire
    let visSegLstPerWire = conIdList |> List.map (fun conId -> visibleSegments conId model)
    // for each list representing a wire get the length - 1 which shows wire right angles
    let wireRightAngles = visSegLstPerWire |> List.map (fun x -> (List.length x) - 1)
    // return the sum of this list 
    wireRightAngles |> List.sum


(*T6*)

// Not implemented, unclear to grasp


