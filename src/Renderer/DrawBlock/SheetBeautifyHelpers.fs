module SheetBeautifyHelpers

//-----------------Module for beautify Helper functions--------------------------//
// Typical candidates: all individual code library functions.
// Other helpers identified by Team

open DrawModelType.SymbolT
open DrawModelType.SheetT
open DrawModelType
open CommonTypes
open Optics
open Symbol
open Helpers
open BlockHelpers

// B1R
let getCustomComponentDimensions (sym: Symbol) : float * float = 
    // Use of lens already provided for getting bounding box
    let comp = sym.Component
    comp.W, comp.H

// B1W
let setCustomComponentDimensions 
    (newDim: float * float) // This is width by height
    (sym: Symbol) 
    : Symbol =
    let comp = sym.Component
    let newComp = {comp with W = fst newDim; H = snd newDim}
    snd component_ newComp sym // Using provided lenses

// B1 Lens
let customComponentDimension_ = Lens.create getCustomComponentDimensions setCustomComponentDimensions

// B2
let setSymbolPos (sym: Symbol) (pos: XYPos) =
    {sym with Pos=pos}

// B3R
/// Read port order on specified side of a symbol.
let getPortOrder (side: Edge) (sym: Symbol) =
    sym.PortMaps.Order[side]

// B3W
/// Write port order on a specified side of a symbol
let setPortOrder (side: Edge) (order: string list) (sym: Symbol) =
    sym.PortMaps.Order
    |> Map.add side order
    // {sym with PortMaps={sym.PortMaps with Order=newPortOrder}}
    |> fun newPortOrder -> snd portMaps_ {sym.PortMaps with Order=newPortOrder} sym

// B3 Lens
let changePortOrder_ (edge: Edge) = Lens.create (getPortOrder edge) (setPortOrder edge)

// B4R
/// Get the reverses state of the inputs of a MUX2
let getReverseStateMux (sym: Symbol) =
    sym.ReversedInputPorts

// B4W
/// Set the reverses state of the inputs of a MUX2
let setReverseStateMux (newReverse: bool option) (sym: Symbol) =
    {sym with ReversedInputPorts=newReverse}

// B4 Lens
let reverseStateMux_ = Lens.create getReverseStateMux setReverseStateMux

// B5R
/// Read the position of a port on the sheet
let getPortPosition (sheet: SymbolT.Model) (port: Port) =
    let sym = sheet.Symbols[ComponentId port.HostId]
    let offset = getPortPos sym port
    sym.Pos + offset

// B6R
/// Get the bounding box of a symbol outline
let getBoundingBox (sym: Symbol) =
    getSymbolBoundingBox sym

// B7R
/// Get the rotation state of a symbol
let getRotationState (sym: Symbol) =
    sym.STransform.Rotation

// B7W
let setRotationState (newRotation: Rotation) (sym: Symbol) =
    {sym with STransform={sym.STransform with Rotation=newRotation}}

// B7 Lens
let rotationState_ = Lens.create getRotationState setRotationState

// B8R
/// Get the rotation state of a symbol
let getFlipState (sym: Symbol) =
    sym.STransform.Flipped

// B8W
let setFlipState (newFlip: bool) (sym: Symbol) =
    {sym with STransform={sym.STransform with Flipped=newFlip}}

// B8 Lens
let flipState_ = Lens.create getFlipState setFlipState


//------------ Testing Helpers ------------//

// T1R
// Inspired from failOnSymbolIntersectsSymbol code from TestDrawBlock.fs
/// Get the number of pairs of symbols that intersect each other
let getSymbolIntersectCount (sheet: SheetT.Model) : int =
    let boxes =
        mapValues sheet.BoundingBoxes
        |> Array.toList
        |> List.mapi (fun n box -> n,box)
    List.allPairs boxes boxes 
    |> List.filter (fun ((n1,box1),(n2,box2)) -> (n1 <> n2) && overlap2DBox box1 box2)
    |> List.length


// visibleSegments function copied from TestDrawBlock.fs:
/// The visible segments of a wire, as a list of vectors, from source end to target end.
/// Note that in a wire with n segments a zero length (invisible) segment at any index [1..n-2] is allowed 
/// which if present causes the two segments on either side of it to coalesce into a single visible segment.
/// A wire can have any number of visible segments - even 1.
let visibleSegments (wId: ConnectionId) (model: SheetT.Model): XYPos list =

    let wire = model.Wire.Wires[wId] // get wire from model

    /// helper to match even and off integers in patterns (active pattern)
    let (|IsEven|IsOdd|) (n: int) = match n % 2 with | 0 -> IsEven | _ -> IsOdd

    /// Convert seg into its XY Vector (from start to end of segment).
    /// index must be the index of seg in its containing wire.
    let getSegmentVector (index:int) (seg: BusWireT.Segment) =
        // The implicit horizontal or vertical direction  of a segment is determined by 
        // its index in the list of wire segments and the wire initial direction
        match index, wire.InitialOrientation with
        | IsEven, BusWireT.Vertical | IsOdd, BusWireT.Horizontal -> {X=0.; Y=seg.Length}
        | IsEven, BusWireT.Horizontal | IsOdd, BusWireT.Vertical -> {X=seg.Length; Y=0.}
    
    // Bug fix addition from edstem - change from original:
    /// Return the list of segment vectors with 3 vectors coalesced into one visible equivalent
    /// wherever this is possible
    let rec coalesce (segVecs: XYPos list)  =
        match List.tryFindIndex (fun segVec -> segVec =~ XYPos.zero) segVecs[1..segVecs.Length-2] with          
        | Some zeroVecIndex -> 
            let index = zeroVecIndex + 1 // base index onto full segVecs
            segVecs[0..index-2] @
            [segVecs[index-1] + segVecs[index+1]] @
            segVecs[index+2..segVecs.Length - 1]
            |> coalesce // recurse as long as more coalescing might be possible
        | None -> segVecs // end when there are no inner zero-length segment vectors
    
    wire.Segments
    |> List.mapi getSegmentVector
    |> coalesce

/// Uses visibleSegments to turn the offset vectors into XYPos coordinates.
/// Returns list of segments beginnings and ends, of all the wire.
let wireVectorsToCoordinates (sheet: SheetT.Model) (wId: ConnectionId) =
    let wire = sheet.Wire.Wires[wId]
    let startPos = wire.StartPos
    let vectors = visibleSegments wId sheet
    let verticesList =
        (startPos, vectors)
        ||> List.scan (fun vertex vector -> vertex + vector)
    List.zip verticesList (List.tail verticesList)
    

/// Finds all visible segments in a sheet
let allVisibleSegments (sheet: SheetT.Model) : (XYPos*XYPos) list =
    sheet.Wire.Wires
    |> Map.toList
    |> List.map fst
    |> List.map (wireVectorsToCoordinates sheet)
    |> List.concat // This would be a list of all start and end segments as tuples in the sheet


// T2R
// Draws inspiration from findWireSymbolIntersections in BusWireRoute.fs
/// Get number of distinct wire visible segments that intersect with one or more symbols in the sheet.
let getVisibleWireSymbolIntersect (sheet: SheetT.Model) =
    let allSymbolsBoundingBox =
        sheet.Wire.Symbol.Symbols
        |> Map.values
        |> Seq.toList
        |> List.filter (fun s -> s.Annotation = None)
        |> List.map (fun s -> (s.Component.Type, getSymbolBoundingBox s))

    
    // These are the distances of all visible segments to a single box
    let distinctDistancesPerBox (box: BoundingBox) = 
        allVisibleSegments sheet
        |> List.map (fun (a, b) -> segmentIntersectsBoundingBox box a b)
        |> List.filter (function
            | Some _ -> true
            | None -> false) // If it is none, then the wire doesn't intersect the box.

    allSymbolsBoundingBox
    |> List.map snd
    |> List.map distinctDistancesPerBox
    |> List.concat
    |> List.length


let rec uniqueListPairs (lst: 'a list) : ('a*'a) list =
    match lst with
    | [] -> []
    | hd::tl ->
        List.map (fun item -> (hd, item)) tl @ uniqueListPairs tl


// T3R
/// Number of distinct visible segments that intersect each other at right angles.
let segmentsIntersectingRightAngle (sheet: SheetT.Model) : int =
    // Returns 0 if segment is horizontal, 1 if it is vertical
    let segmentOrientation (seg: XYPos*XYPos) =
        let startSeg = fst seg
        let endSeg = snd seg
        if startSeg.X = endSeg.X then Some 0
        else if startSeg.Y = endSeg.Y then Some 1
        else None
    
    (allVisibleSegments sheet)
    |> List.map (segmentOrientation)
    |> List.filter (function
        | Some _ -> true
        | None -> false)
    |> uniqueListPairs
    |> List.filter (fun (a,b) -> // Filters out intersecting segments of same orientation
        if a=b then false
        else true)
    |> List.length
