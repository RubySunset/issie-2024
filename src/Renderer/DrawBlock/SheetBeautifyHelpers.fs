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

//-----------------Module for beautify Helper functions--------------------------//
// Typical candidates: all individual code library functions.
// Other helpers identified by Team

// IMPORTANT NOTE:
// Functions used for B1-B8 are only used to get and set fields of symbols and not update them in the sheet 
// as there are functions in the codebase already that do the updating of the symbols in the sheet and using them here 
// is redundant and unnecessary. This confusion was clarified by me asking the question on ed #229.
// Update sheet fucntions already are there and can be used later to update the bounding boxes, etc later if needed.


(*   B1: Functions to get and set the dimensions of a custom component   *)
// check these later
/// TODO: for this check if you will need to use the function from SymbolHelpers.fs using getCustomSymCorners
/// TODO: or use getRotatedHAndW
/// TODO: if have mroe time maybe implement the getter and setter for dimentions seperately


// this returns a tuple in the conventional mathematical manner assuming width corresponds to x and height corresponds to y
let cusotmCompDimGetter (customSymbol : SymbolT.Symbol) : (float * float) = 
    // get the dimentions of a custom symbol considering any rotationa and scaling
    // let dims = (getCustomSymCorners customSymbol)[2]
    // (dims.X, dims.Y)
    let width = customSymbol.Component.W
    let height = customSymbol.Component.H
    width , height
    
    


// TODO: check from the BlockHelper.fs if you can use setCustomCompHW
let customCompDimSetter ( width : float , height : float ) (customSymbol : SymbolT.Symbol) = 
    // setCustomCompHW height width customSymbol
    { customSymbol with Component = {customSymbol.Component with W = width; H = height} }

// make a lens for the custom component dimensions
// lens gets ad sets dimensions as a tuple where first element is width(in x direction/horizontal dim) and second is height(in y direction/vertical dim)
let customCompDimLens_ = Lens.create cusotmCompDimGetter customCompDimSetter







(* B2W *)
// updating the position of a symbol given new position 
// using the provided lens to set the position of a symbol

// maybe use the function from line 329 of SymbolUpdate.fs
// maybe use moveSymbol in block helpers
// or use moveSymbols
let symbolPosSetter (newPos : XYPos) (symbol : SymbolT.Symbol) : SymbolT.Symbol = 
    // let offset = newPos - symbol.Pos
    // moveSymbol offset symbol
    { symbol with Pos = newPos }







(* B3 *)

// Read/write the order of ports on a specified side of a symbol

// getter function for the order of ports on a specific side of a symbol
let symbolEdgePortOrderGetter (symbol : SymbolT.Symbol) (side : Edge) : list<string> = 
    // get portmaps of thhe symbol
    let portMaps = symbol |> fst portMaps_
    // get order field of portMaps
    let order = portMaps |> fst order_
    // return the list of ports in order on the given side in correct order
    order[side]

    // could do all in one line maybe less readable
    //symbol.PortMaps.Order[side]



// setter function for the order of ports on a specific side of a symbol
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
// The reverses state of the inputs of a MUX2
// check the code from line 652 from SymbolUpdate.fs

// for this maybe just return the boolean instead of the option
// this should be implemented for all MUXs and DEMUXs
// check which components use this field
// for old circuits it will be read as NONE
// for new circuits it will be Some thing
// maybe for old circuits just set it to false
// dealing with the None cases: it shoudl be same as the normal state for the added featrue which is False

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




let MuxDemuxReversedInputPortsSetter (state : bool) (symbol : SymbolT.Symbol) : SymbolT.Symbol = 
    // { symbol with ReversedInputPorts = reversed }
    let MuxDemuxLst = [Mux2; Mux4; Mux8; Demux2; Demux4; Demux8]
    let isMuxOrDemux = List.exists (fun x -> x = symbol.Component.Type) MuxDemuxLst
    match isMuxOrDemux with
    | true -> { symbol with ReversedInputPorts = Some state }
    // not changing the field if the symbol is not a MUX or DEMUX since the symbol boolean is only used for MUX and DEMUX (ed stem #143)
    | false -> symbol


// NOTE: LENS SHOULD ONLY BE USED FOR MUX AND DEMUX COMPONENTS ACCORDING TO SPECIFICATION BUT OTHER CASES HAVE BEEN HANDLED
let MuxDemuxReversedInputPortsLens_ = Lens.create MuxDemuxReversedInputPortsGetter MuxDemuxReversedInputPortsSetter








(*B5*)
// The position of a port on the sheet. It cannot directly be written.

let getPortPosOnSheet (symbol : SymbolT.Symbol) (port : Port) : XYPos=
    // get the offset of port relative to topleft pos of symbol
    let offset = getPortPos symbol port
    // add the offset to the Pos of the symbol, you need operator overloading for this, open Operators maybe
    symbol.Pos + offset






(*B6*)

/// check Symbol.fs line 124 for function getSymbolBoundingBox


let getBoundingBox (symbol : SymbolT.Symbol) = 
    getSymbolBoundingBox symbol







(*B7*)
// The rotation state of a symbol RW

let getSTransform (symbol : SymbolT.Symbol) = 
    symbol.STransform

let setSTransform (sTransform : STransform) (symbol : SymbolT.Symbol) = 
    { symbol with STransform = sTransform }

let sTransformLens_ = Lens.create getSTransform setSTransform

let getRotation (sTransform : STransform) = 
    sTransform.Rotation

let setRotation (rotation : Rotation) (sTransform : STransform) = 
    { sTransform with Rotation = rotation }

let rotationLens_ = Lens.create getRotation setRotation

// maybe use the function in line 477 from RotateScale.fs
let rotationStateOfSymbolGetter (symbol : SymbolT.Symbol) : Rotation = 
    // symbol.STransform.Rotation
    symbol |> fst sTransformLens_ |> fst rotationLens_

let rotationStateOfSymbolSetter (rotationState : Rotation) (symbol : SymbolT.Symbol) : SymbolT.Symbol = 
    //{ symbol with STransform = {symbol.STransform with Rotation = rotationState} }
    // rotateSymbolByDegree rotationState symbol
    let oldSTransform = symbol |> fst sTransformLens_
    let newSTransform = (rotationState, oldSTransform) ||> snd rotationLens_
    (newSTransform, symbol) ||> snd sTransformLens_

// make the lens for this
let rotationStateOfSymbolLens_ = Lens.create rotationStateOfSymbolGetter rotationStateOfSymbolSetter







(*B8*)

let getFlipState (sTransform : STransform) = 
    sTransform.Flipped

let setFlipState (flipState : bool) (sTransform : STransform) = 
    { sTransform with Flipped = flipState }

let flipStateLens_ = Lens.create getFlipState setFlipState

let flipStateOfSymbolGetter (symbol : SymbolT.Symbol) : bool = 
    symbol |> fst sTransformLens_ |> fst flipStateLens_

let flipStateOfSymbolSetter (flipState : bool) (symbol : SymbolT.Symbol) = 
    // { symbol with STransform = {symbol.STransform with Flipped = flipState} }
    // or
    // flipSymbolInBlock flipState symbol.CentrePos symbol
    let oldSTransform = symbol |> fst sTransformLens_
    let newSTransform = (flipState, oldSTransform) ||> snd flipStateLens_
    (newSTransform, symbol) ||> snd sTransformLens_

// make lens for this
let flipStateOfSymbolLens_ = Lens.create flipStateOfSymbolGetter flipStateOfSymbolSetter






(*T FUCNTIONS BEGIN*)

// T1R : The number of pairs of symbols that intersect each other. See Tick3 for a related
// function. Count over all pairs of symbols.

// helpers for T1:

// this fucntion creates a list of par of elements from list that dont have repeated elements (a, a) and dont have mirrored pairs (a, b) and (b, a)
let createPairs list =
    let indexedList = List.mapi (fun index value -> (index, value)) list
    let allPairs = 
        List.collect (fun (index1, value1) -> 
            List.choose (fun (index2, value2) -> 
                if index1 < index2 then Some (value1, value2) else None) indexedList) indexedList
    allPairs

let PairsOfIntersectingBoxes (model : SheetT.Model) : int = 
    let boundingBoxesMap = model.BoundingBoxes
    let lstOfBB = boundingBoxesMap |> Map.toSeq |> Seq.map snd |> Seq.toList
    let pairsOfBB = createPairs lstOfBB
    pairsOfBB |> List.filter (fun (x, y) -> overlap2DBox x y) |> List.length



(*T2*)

// The number of distinct wire visible segments that intersect with one or more
// symbols. See Tick3.HLPTick3.visibleSegments for a helper. Count over all visible wire
// segments.

// use getWire function from BlockHelpers.fs
// check the functions from BusWireUpdate.fs lines 481 and 471 search for list of wire
// check the fucntion from BlockHelpers.fs line 228
// check BlockHelpers.fs line 543. returns None if it does not intersect
// check BlockHelpers getAbsSegments
// can get the segment list of a wire using the function in BlockHelpers.fs line 160
// 

let visibleSegments (wId: ConnectionId) (model: SheetT.Model): XYPos list =

    let wire = model.Wire.Wires[wId] // get wire from model

    let (|IsEven|IsOdd|) (n: int) = match n % 2 with | 0 -> IsEven | _ -> IsOdd

        /// Convert seg into its XY Vector (from start to end of segment).
        /// index must be the index of seg in its containing wire.
    let getSegmentVector (index:int) (seg: BusWireT.Segment) =
            // The implicit horizontal or vertical direction  of a segment is determined by 
            // its index in the list of wire segments and the wire initial direction
        match index, wire.InitialOrientation with
        | IsEven, BusWireT.Vertical | IsOdd, BusWireT.Horizontal -> {X=0.; Y=seg.Length}
        | IsEven, BusWireT.Horizontal | IsOdd, BusWireT.Vertical -> {X=seg.Length; Y=0.}

        /// Return a list of segment vectors with 3 vectors coalesced into one visible equivalent
        /// if this is possible, otherwise return segVecs unchanged.
        /// Index must be in range 1..segVecs
    let tryCoalesceAboutIndex (segVecs: XYPos list) (index: int)  =
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

// let getVisSegLst (model : SheetT.Model) = 
//     // map of connId and wires
//     let mapOfWiresConnId = model.Wire.Wires |> Map.toList
//     // list of wires
//     let lstOfWires = mapOfWiresConnId |> List.map snd
//     // get the list of starting position in order 
//     let lstWireStartPos = lstOfWires |> List.map (fun wire -> wire.StartPos)
//     // get the list of connection IDs
//     let lstOfConId = mapOfWiresConnId |> List.map fst
//     // using visibleSegments get the vectors lists corresponding to the connection IDs and their wires
//     let lstOfVectors = lstOfConId |> List.map (fun conId -> visibleSegments conId model)

//     // get a list of vertices
//     let vectorsToPositions vectors startPos =
//         let rec accumulatePositions acc = function
//             | [] -> acc
//             | v :: vs ->
//                 let newPos = match acc with
//                               | [] -> startPos + v  // Assuming this '+' is an abstract representation of vector addition
//                               | p :: _ -> p + v
//                 accumulatePositions (newPos :: acc) vs
//         accumulatePositions [] vectors |> List.rev
    
//     // lst of vectors into positions 
//     let positions = List.map2 vectorsToPositions lstOfVectors lstWireStartPos
//     // position to segments
//     let segments = positions |> List.map List.pairwise |> List.collect id
//     segments
    // remove zero length segments
    // segments |> List.filter (fun segs -> segs <> [])

// let segIntersectingBB (box : BoundingBox) (segment : BusWireT.ASegment) = 
//     let intersectOption = segmentIntersectsBoundingBox box segment.Start segment.End
//     match intersectOption with
//     | Some _ -> true
//     | None -> false

// let segIntersectingBB2 (box : BoundingBox) (segment : XYPos*XYPos) = 
//     let intersectOption = segmentIntersectsBoundingBox box (fst segment) (snd segment)
//     match intersectOption with
//     | Some _ -> true
//     | None -> false

let segIntersectingBB (box : BoundingBox) (segment : BusWireT.ASegment) = 
    let intersectOption = segmentIntersectsBoundingBox box segment.Start segment.End
    match intersectOption with
    | Some _ -> true
    | None -> false

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




let rightAngleSegCount (model : SheetT.Model) : int = 
    // list of all wires in model
    let allWiresInSheet = model.Wire.Wires |> Map.toSeq |> Seq.map snd |> Seq.toList 
    // list of all unique pairs of wires
    let uniqueWirePairs = createPairs allWiresInSheet
    //  list of all unique pairs of wires with different nets
    let uniqueWirePairsDiffNet = uniqueWirePairs |> List.filter (fun (w1, w2) -> w1.OutputPort <> w2.OutputPort)
    // get all paris of segments of two wires
    let getSegmentPairsOfWires wire1 wire2 = 
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
// for this one you would want to use getAbsSegments \

let totalVisWireLen (model : SheetT.Model) : float = 
    let lstSegments = model.Wire.Wires |> Map.toSeq |> Seq.map snd |> Seq.toList |> List.collect getAbsSegments

    let sumSegmentLength = lstSegments |> List.fold (fun acc segment -> acc + segment.Segment.Length) 0.0

    // grouping wires into sublist based on their outputports, which is helpful later for handling same-net segment overlapping
    let partitionedWireIntoNets = BlockHelpers.partitionWiresIntoNets model.Wire




    // TODO: make these functions helper functions to make the code more readable
    // if two segments intersect and are not perperndicular to eachother then they overlap
    // copied from T3
    // these were initially used for a different approach to solve T4
    // let segmentCross (segment1 : ASegment) (segment2 : ASegment) : bool = 
    //     overlap1D (segment1.Start.X, segment1.End.X) (segment2.Start.X, segment2.End.X) && overlap1D (segment1.Start.Y, segment1.End.Y) (segment2.Start.Y, segment2.End.Y)
    // let perpSegmentPair (segment1 : ASegment) (segment2 : ASegment) : bool = 
    //     let seg1Dir = segment1.End - segment1.Start
    //     let seg2Dir = segment2.End - segment2.Start
    //     let dotProd = seg1Dir.X * seg2Dir.X + seg1Dir.Y * seg2Dir.Y
    //     match dotProd with
    //     | 0.0 -> true
    //     | _ -> false

    // Helper function to calculate the overlap between two 1D segments on the same axis
    let calculateOverlap (start1 : float) (end1 : float) (start2 : float) (end2 : float) = 
        let sortedPoints = [start1; end1; start2; end2] |> List.sort
        let overlapStart = max start1 start2
        let overlapEnd = min end1 end2
        max 0.0 (overlapEnd - overlapStart)
    
    let measureSegmentOverlap (segment1 : ASegment) (segment2 : ASegment) =
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


