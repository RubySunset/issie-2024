module SheetBeautifyHelpers

//--------------------Module for beautify Helper functions--------------------//
// Typical candidates: all individual code library functions.
// Other helpers identified by Team


/// Module for the Lens-like functions, might need a better name...
module LensLike =
    open Optics
    open Optics.Operators
    open CommonTypes
    open DrawModelType
    open DrawModelType.SymbolT
    

    /// As only Custom Components can be resized,\
    /// checks symbol component to test whether or not it has scale values
    /// for the getCCSize and setCCSize functions:\
    /// Returns a Result of a tuple of the scale values, if they exist\
    /// Returns an Error string if there's an issue with the symbol
    let getScaleValues (symbol: Symbol) : Result<float*float, string> = 
        match symbol.HScale, symbol.VScale with
        | None, None -> 
            Error "No Scale values: this may not be a Custom Component symbol."
        | Some hscale, Some vscale -> Ok (hscale, vscale)
        | _ -> Error "Malformed Custom Component symbol. Missing a Scale value."

    /// gets the size of a Custom Component symbol (width, height), as a Result
    let getCCSize (symbol: Symbol) : Result<XYPos, string> = 
        let comp = symbol.Component

        getScaleValues symbol
        |> Result.map (fun (hscale, vscale) ->
            {X = comp.W * hscale; Y = comp.H * vscale})
    
    /// didn't have to implement it,\
    /// but this is just a redundant function wrapper, so...
    let getCCSize2 (symbol: Symbol) =
        Symbol.getRotatedHAndW symbol
        
    /// returns the resized Custom Component symbol\
    /// using the provided (width, height), as a Result
    let setCCSize (symbol: Symbol) (newSize: XYPos) : Result<Symbol, string> = 
        let comp = symbol.Component

        getScaleValues symbol
        |> Result.map (fun (hscale, vscale) ->
            (newSize.X / comp.W, newSize.Y / comp.H))
        |> Result.map (fun (newHScale, newVScale) -> {symbol with HScale = Some newHScale; VScale = Some newVScale})


    /// takes in a sheet model, a symbol, and a new position, and returns the sheet, with that symbol in its new position
    let repositionSymbol (sheetModel: SheetT.Model) (symbol: Symbol) (newCentrePos: XYPos) =
        let sheetSymbolLens = SheetT.symbolOf_ symbol.Id
        let symbol = Optic.get sheetSymbolLens sheetModel
        let offset = newCentrePos - symbol.CentrePos

        Optic.set sheetSymbolLens {symbol with CentrePos = symbol.CentrePos + offset; Pos = symbol.Pos + offset}


    /// taking inspiration from SheetT.symbolOf_, this function returns a lens:\
    /// getting and setting of a portMaps' portList for a given Edge
    let portListOf_ edge = 
        Lens.create <|| (
            (fun pMaps -> 
                (Optic.get order_ pMaps)[edge]), 
            (fun newList pMaps -> 
                (Optic.get order_ pMaps)
                |> Map.add edge newList
                |> fun newPortMap -> 
                    Optic.set order_ newPortMap pMaps
            )
        )

    /// gets the list of strings defining port order\
    /// for a symbol, for its given edge
    let getPortOrder (symbol: Symbol) (edge: Edge) =
        Optic.get (portMaps_ >-> portListOf_ edge) symbol

    
    /// sets the list of strings defining port order, at the given edge,\
    /// to the provided new port ordering :)
    let setPortOrder (symbol: Symbol) (edge: Edge) newPortOrder = 
        Optic.set (portMaps_ >-> portListOf_ edge) newPortOrder symbol 


    /// A lens that allows getting and setting of `ReversedInputPorts` option\
    /// for a `Mux2` symbol
    let mux2ReversedState_ = Lens.create <|| (
        (fun symbol -> symbol.ReversedInputPorts),
        (fun stateOption symbol -> 
            {symbol with ReversedInputPorts = stateOption})
    )

    
    /// Performs a 'clockwise shift' on an edge value
    let shiftEdge (edge: Edge)= 
        match edge with
        | Top -> Right
        | Right -> Bottom
        | Bottom -> Left
        | Left -> Top

    /// grabs the port's parent symbol and performs some arithmetic\
    /// to determine the port's position on the sheet\
    /// There's almost definitely a quicker/neater way of doing this :/
    let getPortPosition (sheet: SheetT.Model) (port: Port) = 
        let symbol = 
            Optic.get (
                SheetT.symbolOf_ (ComponentId port.HostId)
            ) sheet

        let portEdge: Edge = 
            match port.PortType with
            | PortType.Input -> Left
            | PortType.Output -> Right
            |> fun edge -> 
                match symbol.STransform.Rotation with
                | Degree0 -> edge
                | Degree90 -> shiftEdge edge
                | Degree180 -> (shiftEdge >> shiftEdge) edge
                | Degree270 -> (shiftEdge >> shiftEdge >> shiftEdge) edge
            |> fun edge -> 
                match symbol.STransform.Flipped with
                | true -> (shiftEdge >> shiftEdge) edge
                | false -> edge

        let edgeCentreOffset edge =
            match edge with
            | Top -> 
                symbol.CentrePos + { X = 0.0; Y = - symbol.Component.H / 2.0 }
            | Right -> 
                symbol.CentrePos + { X = symbol.Component.W / 2.0; Y = 0.0 }
            | Bottom -> 
                symbol.CentrePos - { X = 0.0; Y = symbol.Component.H / 2.0 }
            | Left -> 
                symbol.CentrePos - { X = - symbol.Component.W / 2.0; Y = 0.0 }

        symbol.CentrePos + edgeCentreOffset portEdge


    /// takes in a symbol and returns a BoundingBox\
    /// representing the dimensions of that symbol
    let getSymbolBB (symbol: Symbol) =
        let comp = symbol.Component

        match getScaleValues symbol with
        | Ok (hscale, vscale) -> // CustomComponent
            let size = {X = comp.W * hscale; Y = comp.H * vscale}
            { TopLeft = symbol.Pos; W = size.X; H = size.Y }
        | Error _ ->  // not a CustomComponent
            {TopLeft = symbol.Pos; W = comp.W; H = comp.H}


    /// Lens for getting and setting the rotation state of a symbol
    let symbolRotationState_ = Lens.create <|| (
        (fun symbol -> symbol.STransform.Rotation),
        (fun newRotation symbol -> 
            {symbol with Symbol.STransform.Rotation = newRotation})
    )


    /// Lens for getting and setting the flip state of a symbol
    let symbolFlipState_ = Lens.create <|| (
        (fun symbol -> symbol.STransform.Flipped),
        (fun newFlip symbol -> 
            {symbol with Symbol.STransform.Flipped = newFlip})
    )


(* Useful Lenses and Records and things
SheetT.boundingBoxes_ - Lens<SheetT, BoundingBox list>
CommonTypes.BoundingBox - Bounding Box type
CommonTypes.size_ - Lens<BoundingBox, Size>
*)

/// copied in from TestDrawBlock, because F# doesn't like forward declarations
module Helpers =
    open EEExtensions
    open Optics
    open CommonTypes
    open DrawModelType

    /// Optic to access SheetT.Model from Issie Model
    let sheetModel_ = ModelType.sheet_

    /// Optic to access BusWireT.Model from SheetT.Model
    let busWireModel_ = SheetT.wire_

    /// Optic to access SymbolT.Model from SheetT.Model
    let symbolModel_ = SheetT.symbol_

    /// allowed max X or y coord of svg canvas
    let maxSheetCoord = Sheet.Constants.defaultCanvasSize
    let middleOfSheet = {X=maxSheetCoord/2.;Y=maxSheetCoord/2.}
    
    //--------------------------------------------------------------------------
    // visibleSegments is included here as a helper for info
    // and because it is needed in project work
    //--------------------------------------------------------------------------

    // have taken `coalesce` out because it's very useful
    /// Return a list of segment vectors with 3 vectors
    /// coalesced into one visible equivalent
    /// if this is possible,\
    /// otherwise return segVecs unchanged.\
    /// Index must be in range 1..segVecs
    let rec coalesce (segVecs: XYPos list)  =
        match (List.tryFindIndex 
        (fun segVec -> segVec =~ XYPos.zero) 
        segVecs[1..segVecs.Length-2]) with  // omit nubs
        | Some zeroVecIndex -> 
            // base index onto full segVecs
            let index = zeroVecIndex + 1 
            segVecs[0..index-2] @
            [segVecs[index-1] + segVecs[index+1]] @
            segVecs[index+2..segVecs.Length - 1]
            |> coalesce 
            // ^ recurse as long as more coalescing might be possible
        | None -> segVecs 
        // ^ end when there are no inner zero-length segment vectors

    // I feel 'effective segment' may be more descriptive
    // than 'visible segment', since the latter could be taken to imply that
    // the segment is visible on the screen, which is not what it means.
    /// The visible segments of a wire, as a list of vectors, 
    /// from source end to target end.\
    /// Note that in a wire with n segments, 
    /// a zero length (invisible) segment at any index [1..n-2] is allowed,\
    /// which, if present, causes the two segments on either side of it 
    /// to coalesce into a single visible segment.\
    /// A wire can have any number of visible segments - even 1.
    let effectiveSegments (model: SheetT.Model) (absolute: bool) (wId: ConnectionId): XYPos list =
        let wire = model.Wire.Wires[wId] // get wire from model

        /// helper to match even and odd integers in patterns (active pattern)
        let (|IsEven|IsOdd|) (n: int) = match n % 2 with | 0 -> IsEven | _ -> IsOdd

        /// Convert seg into its XY Vector (from start to end of segment).\
        /// index must be the index of seg in its containing wire.
        let getSegmentVector (index:int) (seg: BusWireT.Segment) =
            // The implicit horizontal or vertical direction of a segment
            // is determined by its index in the list of wire segments
            // and the wire's initial direction
            match index, wire.InitialOrientation with
            | IsEven, BusWireT.Vertical | IsOdd, BusWireT.Horizontal -> 
                {X=0.; Y=seg.Length}
            | IsEven, BusWireT.Horizontal | IsOdd, BusWireT.Vertical -> 
                {X=seg.Length; Y=0.}

        /// takes in a list of relative Segment Vectors\
        /// returns a list of the absolute start point of each Segment Vector\
        /// oh, and the end point, of course
        let getAbsoluteSegVec (segVec: XYPos list) = 
            wire.StartPos::segVec 
            |> List.scan (+) XYPos.zero
            // Yes, this does output an extra element at the head of the list,
            // in contrast to relative vectors, but I can't see another way, rn

        wire.Segments
        |> List.mapi getSegmentVector
        |> coalesce
        |> if absolute  // modification to allow returning of absolute positions
           then getAbsoluteSegVec
           else id


/// A module of utility functions that count things
module Counters =
    open DrawModelType
    open CommonTypes
    open Optics
    open Optics.Operators


    /// Counts the number of symbol pairs in the sheet that intersect each other
    let symbolIntersectionCount (sheet: SheetT.Model) =
        let symbols = 
            sheet
            |> Optic.get SheetT.symbol_
            |> Optic.get SymbolT.symbols_
            |> Map.toSeq
            |> Seq.map snd
            |> Seq.toList

        List.allPairs symbols symbols
        |> List.filter (
            fun (symbol1, symbol2) -> 
                let bb1 = LensLike.getSymbolBB symbol1
                let bb2 = LensLike.getSymbolBB symbol2
                (symbol1 <> symbol2) && BlockHelpers.overlap2DBox bb1 bb2
        )
        |> List.length


    /// returns a function: that takes an absolute segment vector\
    /// and returns true if it intersects any symbols in the provided sequence
    let wireIntersectingSymbol (symbols: SymbolT.Symbol seq): (XYPos*XYPos) -> bool =
        let symbolBBs = Seq.map LensLike.getSymbolBB symbols
        
        let bbToXYPos (symbolBB: BoundingBox): (XYPos*XYPos) = 
            symbolBB.TopLeft, symbolBB.BottomRight()
        
        let symbolXYPosTuples = 
            Seq.map bbToXYPos symbolBBs
        
        fun segmentVector -> 
            symbolXYPosTuples
            |> Seq.exists (
                fun bb -> 
                    BlockHelpers.overlap2D bb segmentVector
            )

    /// gets all wires from the sheet, and applies the selector to each wire\
    /// expecting selector to be `fst` or `snd`,\
    /// but other valid selectors could also be useful
    let getWires (model: SheetT.Model) selector = 
        Optic.get SheetT.wires_ model
        |> Map.toList
        |> List.map selector

    /// counts the number of effective wire segments that intersect >= 1 symbols
    let effectiveSegmentSymbolIntersections (model: SheetT.Model) = 
        let wireIds = getWires model fst

        let symbols = 
            model
            |> Optic.get (SheetT.symbol_ >-> SymbolT.symbols_)
            |> Map.toSeq
            |> Seq.map snd
        
        // creates tuples of the starts and ends of each effective wire segment
        // then filters out the ones that don't intersect with any symbols
        wireIds
        |> List.collect (Helpers.effectiveSegments model true >> List.pairwise)
        |> List.filter (wireIntersectingSymbol symbols)
        |> List.length

(*
    /// returns a function: sth that takes a wire or sth and checks sth
    let wireIntersectingWire (wire1: BusWireT.Wire): BusWireT.Wire -> bool =
        let symbolBBs = Seq.map LensLike.getSymbolBB symbols
        
        let bbToXYPos (symbolBB: BoundingBox): (XYPos*XYPos) = 
            symbolBB.TopLeft,  // already an XYPos
            { X = symbolBB.TopLeft.X + symbolBB.W; 
              Y = symbolBB.TopLeft.Y - symbolBB.H; }
        
        let symbolXYPosTuples = 
            Seq.map bbToXYPos symbolBBs
        
        fun segmentVector -> 
            symbolXYPosTuples
            |> Seq.exists (
                fun bb -> 
                    BlockHelpers.overlap2D bb segmentVector
            )
*)
(*
    /// TODO: figure out how to keep Orientations while using `coalesce`
    /// that'll solve the whole thing, cos you check:
    /// intersecting && orientation clash for each pair, and you're done
    let T3R (model: SheetT.Model) = 
        let wires = getWires model snd

        wires
        |> List.map (id (*COME UP WITH STH TO SAVE ORIENTATION*))
        |> List.allPairs
        |> List.filter (
            // see if their Orientations clash
            fun (seg1, seg2) -> 
                BlockHelpers.overlap1D (seg1.X, seg2.X) (seg1.Y, seg2.Y)
        )
        |> List.length
        // The number of distinct pairs of segments that cross each other at right angles. Does not include 0 length segments or segments on same net intersecting at one end, or segments on same net on top of each other. Count over whole sheet.
*)

    /// idk
    let T4R = 
        () // Sum of wiring segment length, counting only one when there are N same-net segments overlapping (this is the visible wire length on the sheet). Count over whole sheet.
        // this means that any segment that is on top of another segment is only counted once, check Nets and whatnot


    /// counts the number of right angles for all wires in the sheet
    let wireRightAngleCount (model: SheetT.Model) = 
        let wireIds = getWires model fst

        wireIds
        |> List.map (Helpers.effectiveSegments model false)
        |> List.map (fun wire -> List.length wire - 1)
        |> List.sum
        // List.length - 1 gives the number of orientation changes
        // in an 'effective Segment' list, for a wire :)


    /// idk
    let T6R = 
        () // The zero-length segments in a wire with non-zero segments on either side that have Lengths of opposite signs lead to a wire retracing itself. Note that this can also apply at the end of a wire (where the zero-length segment is one from the end). This is a wiring artifact that should never happen but errors in routing or separation can cause it. Count over the whole sheet. Return from one function a list of all the segments that retrace, and also a list of all the end of wire segments that retrace so far that the next segment (index = 3 or Segments.Length – 4) - starts inside a symbol.


    /// idk
    let Ext = 
        ()  // See `TestDrawBlock.HLPTick3.visibleSegments` The visible segments of a wire, as a list of vectors, from source end to target end. Note that a zero length segment one from either end of a wire is allowed which if present causes the end three segments to coalesce into a single visible segment.


(****************************Other Random Functions****************************

/// check that the port order list is valid
let checkValidIndices portOrderList =
    let portOrderSet = Set.ofList portOrderList
    portOrderSet = Set.ofList [0..portOrderList.Length-1]

******************************************************************************)
