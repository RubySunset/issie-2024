module SheetBeautifyHelpers
//-----------------Module for beautify Helper functions--------------------------//
// Typical candidates: all individual code library functions.
// Other helpers identified by Team

open CommonTypes
open DrawModelType.SymbolT
open DrawModelType.SheetT
open DrawModelType.BusWireT
open DrawModelType
open Optics
open Symbol
open Optics.Optic
open Operators
open Helpers
open BlockHelpers
open BusWireUpdateHelpers

module Lenses =
    // I interpreted lenses to only affect the variable it is assigned as functions already exist

    // Original Code NOT needed as functions already exist, it would be making a meaningless lens
    // I am also not going to rewrite a function that already exists
    // to adjust the corresponding/ related variables
    let _initialCustomCompHW_' =
        Lens.create
            (fun sym -> match sym.Component.Type with | Custom _ -> getRotatedHAndW sym | _ -> 0, 0)
            (fun (h, w) sym -> setCustomCompHW h w sym)
    
    // Option given to either use a lens for both or each dimension individually

    // HLP2024 B1 ah121
    /// Lens for the height of a symbol component
    /// ```(Symbol.Component.H)```
    let symCompH_ = component_ >-> h_

    // HLP2024 B1 ah121
    /// Lens for the width of a symbol component
    /// ```(Symbol.Component.W)```
    let symCompW_ = component_ >-> w_

    // HLP2024 B1 ah121
    /// Lens for the height and width of a symbol component
    /// ```Symbol.Component.H * Symbol.Component.W```
    let symCompHW_ =
        Lens.create
            (fun symbol -> get symCompH_ symbol, get symCompW_ symbol)
            (fun (h, w) symbol -> symbol |> set symCompH_ h |> set symCompW_ w)

    // HLP2024 B2 ah121
    /// <description> Moves a symbol to a new position </description>
    /// <param name="symbol">The symbol to be moved.</param>
    /// <param name="newPos">The new position, represented by the top-left coordinate, to which the symbol will be moved.</param>
    /// <returns>The symbol after it has been moved to the new position.</returns>
    let moveSymbolPosition (symbol: Symbol) (newPos: XYPos) =
        moveSymbol (newPos - symbol.Pos) symbol

    // HLP2024 B3 ah121
    // Names such as portOrder_ used from SymbolInfo
    /// Lens for the port order of a symbol
    /// ```(Symbol.PortMaps.Order)```
    let symPortOrder_ = portMaps_ >-> order_

    // HLP2024 B3 ah121
    /// Returns the port order for the given side of a symbol
    let getSymbolSidePortOrder (symbol: Symbol) (side: Edge) =
        // symbol.PortMaps.Order[side]
        get symPortOrder_ symbol
        |> Map.tryFind side // Fairly sure tryFind is unnecessary but as explained below
        |> Option.defaultValue [] // Bad code in future/ other areas may require this

    // HLP2024 B3 ah121
    // Can't use Lens even if symbol is last parameter as corresponding Lens.get only takes one input
    // i.e. expecting (Edge -> list<string>) but got Edge -> list<string> in Lens.set
    /// Overwrites the port order for the given side of a symbol
    let setSymbolSidePortOrder (symbol: SymbolT.Symbol) (side: Edge) (ports: string list) =
        get symPortOrder_ symbol
        |> Map.add side ports
        |> set symPortOrder_ <| symbol

    // HLP2024 B4 ah121
    // This lens should not be used if the component is not a MUX 
    // so I have decided to not check for it to maintain simplicity.
    // If it is used for a not MUX component it should only affect the symbol variable
    // and not the actual component in ISSIE as it would not be accounted for in such a case
    // This also enables ReversedInputPorts to be used in future 
    // components without need to change the lens code
    /// Lens for the reversed input ports of a (MUX) symbol
    /// ```(Symbol.ReversedInputPorts)```
    let revInputPorts_ =
        Lens.create
            (fun symbol -> Option.defaultValue false symbol.ReversedInputPorts)
            (fun r symbol -> { symbol with ReversedInputPorts = Some r })

    // HLP2024 B5 ah121
    /// Returns the position of a port on the sheet
    let getSheetPortPos (symbol: SymbolT.Symbol) (port: Port) =
        (getPortPos symbol port) + symbol.Pos

    // HLP2024 B6 ah121
    // Function already exists...
    /// Returns the bouding box of a symbol
    let getSymbolBoundingBox' = Symbol.getSymbolBoundingBox

    // HLP2024 B7 ah121
    /// Lens for the transform state of a symbol
    /// ```(Symbol.STransform)```
    let sTransform_ =
        Lens.create
            (fun symbol -> symbol.STransform)
            (fun s symbol -> { symbol with STransform = s })

    // HLP2024 B7 ah121
    /// Lens for the rotation of a transform state
    /// ```(STransform.Rotation)```
    let rotation_ =
        Lens.create
            (fun transform -> transform.Rotation)
            (fun r transform -> { transform with Rotation = r })

    // HLP2024 B8 ah121
    /// Lens for the flip of a transform state
    /// ```(STransform.Flipped)```
    let flipped_ =
        Lens.create
            (fun transform -> transform.Flipped)
            (fun f transform -> { transform with Flipped = f })

    // HLP2024 B8 ah121
    // Simple lens made as RotateScale.rotateSymbolByDegree and SymbolT.rotateAntiClockByAng exist
    /// Lens for the rotation of a symbol
    /// ```(Symbol.STransform.Rotation)```
    let symRotation_ = sTransform_ >-> rotation_

    // HLP2024 B8 ah121
    // Simple lens made as SymbolT.Flip exists
    /// Lens for the flip of a symbol
    /// ```(Symbol.STransform.Flipped)```
    let symFlipped_ = sTransform_ >-> flipped_


module Helpers = 

        // HLP2024 T1 ah121
        // Folds over list[i] and pairs it with each value in list[i+1:]
        // Sums the number of intersecting bound pairs found on each fold
        /// Folder function fo unique intersecting bounding box pairs
        let intersectingPairCount (tail, count) curVal = 
            // More efficient to filter first before pairing
            // Could be worth to return only non repeated pairs for more functionality
            let newCount =
                tail
                |> List.map (fun nextVal -> (curVal, nextVal))
                |> List.filter (fun (box1, box2) -> overlap2DBox box1 box2)
                |> List.length
                |> (+) count
            match tail with 
            | _hd::tl -> (tl, newCount)
            | _ -> ([], newCount)

        // HLP2024 T1 ah121
        /// Returns number of intersecting box pairs in a BoundingBox list
        let pairsCount lst = 
            // Not using snd for better clarity
            List.fold intersectingPairCount (List.tail lst, 0) lst
            |> function | _state, count -> count

        // HLP2024 T2 ah121
        /// Checks if Segment 2 is contained within Segment 1
        let isSegContained (seg1: ASegment) (seg2: ASegment) = 
            match seg1.Orientation, seg2.Orientation with
            | Horizontal, Horizontal -> 
                seg1.Start.Y  = seg2.Start.Y 
                && seg1.End.X   >= seg2.End.X
                && seg1.Start.X <= seg2.Start.X
            | Vertical, Vertical -> 
                seg1.Start.X  = seg2.Start.X 
                && seg1.End.Y   >= seg2.End.Y
                && seg1.Start.Y <= seg2.Start.Y
            | _ -> false

        // HLP2024 T2 ah121
        /// Returns true if a segment is visible/ not contained 
        /// within another segment given a segment list
        let isSegmentVisible (segments: ASegment list) (seg1: ASegment) =
            segments 
            // Used to remove duplicate segments even if in reverse directions
            |> List.groupBy (fun seg -> (min seg.Start seg.End, max seg.Start seg.End))
            |> List.collect (fun (_, similarSegments) ->
                match similarSegments with
                | [seg] -> [seg]
                | seg :: _ -> [seg]
                | _ -> [])
            |> List.exists (fun seg2 -> seg1 <> seg2 && isSegContained seg1 seg2)
            |> not

        // HLP2024 T2 ah121
        /// Combines segments if they are adjacent
        let combineAdjacentSegments (segments: ASegment list) =
            let adjacent prv cur = 
                prv.End.X = cur.Start.X 
                && prv.End.Y = cur.Start.Y 
                && prv.Orientation = cur.Orientation 

            let combineSegments seg1 seg2 =
                { seg1 with End = { X = seg2.End.X; Y = seg2.End.Y }}
            
            let adjacentSegments lst seg =
                match lst with
                | [] -> [seg]
                | prevSeg :: tail when adjacent prevSeg seg
                    -> combineSegments prevSeg seg :: tail
                | _ -> seg :: lst

            ([], segments)
            ||> List.fold adjacentSegments

        // HLP2024 T3 ah121
        let inDiffNet (wires: Map<ConnectionId, Wire>) (seg1: ASegment, seg2: ASegment) =
            match seg1.GetId, seg2.GetId with
            | (_n, connId1), (_m, connId2) -> wires[connId1].OutputPort <> wires[connId2].OutputPort

        // HLP2024 T3 ah121
        let doSegsCross (seg1, seg2) =
            let a_min, a_max = min seg1.Start.X  seg1.End.X, max seg1.Start.X seg1.End.X
            let b_min, b_max = min seg2.Start.Y  seg2.End.Y, max seg2.Start.Y seg2.End.Y
            a_max > b_min && b_max > a_min

        // HLP2024 T3 ah121
        let doSegsTouch (hSeg, vSeg) = 
            hSeg.Start.X = vSeg.Start.X 
            || hSeg.End.X = vSeg.Start.X
            || vSeg.Start.Y = hSeg.Start.Y
            || vSeg.End.Y = hSeg.Start.Y


module Testers = 

    open Helpers

    // HLP2024 T1 ah121
    /// Returns the number of intersecting symbol pairs in a sheet
    let intersectingSymbolPairsCount (model: SheetT.Model) = 
        model.BoundingBoxes
        |> mapValues
        |> Array.toList
        |> pairsCount
    
    // HLP2024 T2 ah121
    /// Returns the number of intersecting symbol pairs in a sheet
    /// Returns number of distinct segments that intersect a symbol
    let visibleSegmentIntersectCount (model: SheetT.Model) =
        let boxMap = model |> get boundingBoxes_
        let boxes = boxMap |> mapValues |> Array.toList
        let compIds = boxMap |> mapKeys |> Array.toList
        let connPortsCount = getConnectedWireIds model.Wire compIds |> List.length |> (*) 2
        let wires = model |> get SheetT.wires_ |> mapValues |> Array.toList 
        let segments = wires |> List.collect getNonZeroAbsSegments 
        let combSegments = segments |> combineAdjacentSegments

        // Actual number of distinct visible segments
        // Can't seem to find a away (within reasonable time) 
        // to convert this into the correct number of intersections
        // (currently segments already overlap with the block via the ports)
        let visibleSegsCount = 
            combSegments
            |> List.filter (isSegmentVisible combSegments)
            |> List.length 
            |> (fun x -> printfn "Ports: %A " (connPortsCount); x)
            |> (fun x -> printfn "Segments: %A "  (segments |> List.length) ; x)
            |> (fun x -> printfn "Filtered Segments: %A "  (segments |> List.filter (isSegmentVisible segments) |> List.length) ; x)
            |> (fun x -> printfn "Comb Segments: %A " (List.length combSegments); x)
            |> (fun x -> printfn "Distinct Visible Segments: %A " x; x)

        // Returns the number of segments that intersect with a symbol        
        // Removes the number of ports as these are default intersects
        segments
        |> List.map (fun seg -> 
            List.map (fun box -> segmentIntersectsBoundingBox box seg.Start seg.End) boxes)
        |> List.filter (List.exists (function Some _float -> true | _ -> false))
        |> List.length 
        |> (-) <| connPortsCount

    // HLP2024 T3 ah121
    // The number of distinct pairs of segments that cross each other at right angles. Does
    // not include 0 length segments or segments on same net intersecting at one end, or
    // segments on same net on top of each other. Count over whole sheet.
    let crossingSegmentPairsCount (model: SheetT.Model) = 

        let wires = get SheetT.wires_ model

        let segments = 
            wires
            |> mapValues
            |> Array.toList
            |> List.collect getNonZeroAbsSegments
            |> combineAdjacentSegments
            |> List.mapi (fun i x -> printfn "Index: %A,\nStart: { X = %A; Y = %A },\nEnd:   { X = %A; Y = %A }" i x.Start.X x.Start.Y x.End.X x.End.Y; x)

        let horizontalSegs, verticalSegs = 
            segments 
            |> List.filter (isSegmentVisible segments)
            |> List.groupBy (fun segment -> segment.Orientation)
            |> function 
            | [Horizontal, hList; Vertical, vList] -> hList, vList
            | [Vertical, vList; Horizontal, hList] -> hList, vList
            | _ -> [], []

        /// horizontalSegment * verticalSegment list
        let segmentPairs = horizontalSegs |> List.collect (fun hSeg -> verticalSegs |> List.map (fun vSeg -> hSeg, vSeg))

        // ISSUE HERE
        /// Returns the number of fully crossing segment pairs
        let fullyCrossingSegsCount = 
            segmentPairs 
            |> List.filter doSegsCross
            |> List.length
        
        /// Returns number of touching segment pairs 
        /// iff segments are in different nets  
        let touchingSegsCount =
            segmentPairs 
            |> List.filter (inDiffNet wires)
            |> List.filter doSegsTouch
            |> List.length
        
        let printer = 
            segmentPairs 
            |> List.filter (inDiffNet wires)
            |> List.filter doSegsTouch
            |> List.map (fun (seg1, seg2) -> ((seg1.Start, seg1.End, wires[snd seg1.GetId].OutputPort), (seg2.Start, seg2.End, wires[snd seg2.GetId].OutputPort)) ) 
            |> List.map (fun (x, y) -> printfn "%A\n%A" x y; x)

        fullyCrossingSegsCount + touchingSegsCount