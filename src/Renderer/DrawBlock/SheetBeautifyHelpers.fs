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
open Operators
open Helpers
open BlockHelpers
open BusWireUpdateHelpers

module IndividualPhaseWork =
    // I interpreted lenses to only affect the variable it is assigned as functions already exist
    // If 

    // Original Code NOT needed as functions already exist, it would be making a meaningless lens
    // I am also not going to rewrite a function that already exists
    // to adjust the corresponding/ related variables
    let initialCustomCompHW_' =
        Lens.create
            (fun sym -> match sym.Component.Type with | Custom _ -> Symbol.getRotatedHAndW sym | _ -> 0, 0)
            (fun (h, w) sym -> BlockHelpers.setCustomCompHW h w sym)
    
    /// Lens for the height of a symbol component
    /// ```(Symbol.Component.H)```
    ///
    /// Get Example:
    /// ```
    /// let h = Optic.get customCompH_ symbol
    /// ```
    ///Set Example:
    /// ```
    /// symbol
    /// |> Optic.set customCompH_ h
    /// ```
    let symCompH_ = component_ >-> h_

    /// Lens for the width of a symbol component
    /// ```(Symbol.Component.W)```
    ///
    /// Get Example:
    /// ```
    /// let w = Optic.get customCompW_ symbol
    /// ```
    ///Set Example:
    /// ```
    /// symbol
    /// |> Optic.set customCompW_ w
    /// ```
    let symCompW_ = component_ >-> w_

    // Option to use both based on need

    /// Lens for the height and width of a symbol component
    ///
    /// ```Symbol.Component.H * Symbol.Component.W```
    ///
    /// Get Example:
    /// ```
    /// let h, w = Optic.get customCompHW_
    /// ```
    ///Set Example:
    /// ```
    /// let _, w = Optic.get customCompHW_
    /// Optic.set customCompHW_ (newH, w) symbol1
    /// Optic.set customCompHW_ (newH, newW) symbol2
    /// ```
    let symCompHW_ =
        Lens.create
            (fun symbol -> Optic.get symCompH_ symbol, Optic.get symCompW_ symbol)
            (fun (h, w) symbol -> symbol |> Optic.set symCompH_ h |> Optic.set symCompW_ w)

    /// <description> Moves a symbol to a new position </description>
    /// <param name="symbol">The symbol to be moved.</param>
    /// <param name="newPos">The new position, represented by the top-left coordinate, to which the symbol will be moved.</param>
    /// <returns>The symbol after it has been moved to the new position.</returns>
    let moveSymbolPosition (symbol: Symbol) (newPos: XYPos) =
        BlockHelpers.moveSymbol (newPos - symbol.Pos) symbol

    // Names such as portOrder_ used from SymbolInfo
    /// Lens for the port order of a symbol
    /// ```(Symbol.PortMaps.Order)```
    let symPortOrder_ = portMaps_ >-> order_

    /// Returns the port order for the given side of a symbol
    let getSymbolSidePortOrder (symbol: Symbol) (side: Edge) =
        // symbol.PortMaps.Order[side]
        Optic.get symPortOrder_ symbol
        |> Map.tryFind side // Fairly sure tryFind is unnecessary but as explained below
        |> Option.defaultValue [] // Bad code in future/ other areas may require this

    // Can't use Lens even if symbol is last parameter as corresponding Lens.get only takes one input
    // i.e. expecting (Edge -> list<string>) but got Edge -> list<string> in Lens.set
    /// Overwrites the port order for the given side of a symbol
    let setSymbolSidePortOrder (symbol: SymbolT.Symbol) (side: Edge) (ports: string list) =
        Optic.get symPortOrder_ symbol
        |> Map.add side ports
        |> Optic.set symPortOrder_ <| symbol

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

    /// Returns the position of a port on the sheet
    let getSheetPortPos (symbol: SymbolT.Symbol) (port: Port) =
        (Symbol.getPortPos symbol port) + symbol.Pos

    // Function already exists...
    /// Returns the bouding box of a symbol
    let getSymbolBoundingBox = Symbol.getSymbolBoundingBox

    /// Lens for the transform state of a symbol
    /// ```(Symbol.STransform)```
    let sTransform_ =
        Lens.create
            (fun symbol -> symbol.STransform)
            (fun s symbol -> { symbol with STransform = s })

    /// Lens for the rotation of a transform state
    /// ```(STransform.Rotation)```
    let rotation_ =
        Lens.create
            (fun transform -> transform.Rotation)
            (fun r transform -> { transform with Rotation = r })

    /// Lens for the flip of a transform state
    /// ```(STransform.Flipped)```
    let flipped_ =
        Lens.create
            (fun transform -> transform.Flipped)
            (fun f transform -> { transform with Flipped = f })

    // Simple lens made as RotateScale.rotateSymbolByDegree and SymbolT.rotateAntiClockByAng exist
    /// Lens for the rotation of a symbol
    /// ```(Symbol.STransform.Rotation)```
    let symRotation_ = sTransform_ >-> rotation_

    // Simple lens made as SymbolT.Flip exists
    /// Lens for the flip of a symbol
    /// ```(Symbol.STransform.Flipped)```
    let symFlipped_ = sTransform_ >-> flipped_


    /// Returns the number of intersecting symbol pairs in a sheet
    let intersectingSymbolPairsCount (model: SheetT.Model) = 
        // Folds over list[i] and pairs it with each value in list[i+1:]
        // Sums the number of intersecting bound pairs found on each fold
        /// Folder function fo unique intersecting bounding box pairs
        let intersectingPairCount (tail, count) curVal = 
            let newResult =
                tail
                |> List.map (fun nextVal -> (curVal, nextVal))
                |> List.filter (fun (box1, box2) -> BlockHelpers.overlap2DBox box1 box2)
                |> List.length
                |> (+) count
            match tail with 
            | _hd::tl -> (tl, newResult)
            | _ -> ([], newResult)

        // Not using snd for clarity
        /// Returns number of intersecting box pairs in a BoundingBox list
        let pairsCount lst = 
            List.fold intersectingPairCount (List.tail lst, 0) lst
            |> function | _state, count -> count

        model.BoundingBoxes
        |> mapValues
        |> Array.toList
        |> pairsCount
    
    /// Returns number of distinct segments that intersect a symbol
    let visibleSegmentIntersectCount (model: SheetT.Model) =
        /// Checks if Segment 2 is contained within Segment 1
        let isContained (seg1: ASegment) (seg2: ASegment) = 
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
            |> List.exists (fun seg2 -> seg1 <> seg2 && isContained seg1 seg2)
            |> not

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

        let boxMap = model |> Optic.get boundingBoxes_
        let boxes = boxMap |> mapValues |> Array.toList
        let compIds = boxMap |> mapKeys |> Array.toList
        let connPortsCount = getConnectedWireIds model.Wire compIds |> List.length |> (*) 2
        let wires = model |> Optic.get SheetT.wires_ |> mapValues |> Array.toList 
        let segments = wires |> List.collect getNonZeroAbsSegments 
        let combSegments = segments |> combineAdjacentSegments

        // Actual number of distinct visible segments
        // Can't seem to find a away (within reasonable time) 
        // to convert this into the correct number of intersections
        // (currently segments already overlap with the block via the ports)
        let visibleSegmentsCount = 
            combSegments
            |> List.length 
            |> (-) <| connPortsCount
            |> (fun x -> printfn "Ports: %A " (connPortsCount); x)
            |> (fun x -> printfn "Segments: %A "  (segments |> List.length) ; x)
            |> (fun x -> printfn "Filtered Segments: %A "  (segments |> List.filter (isSegmentVisible segments) |> List.length) ; x)
            |> (fun x -> printfn "Comb Segments: %A " (List.length combSegments); x)
            |> (fun x -> printfn "Distinct Visible Segments: %A " x; x)
        
        segments
        |> List.map (fun seg -> 
            List.map (fun box -> segmentIntersectsBoundingBox box seg.Start seg.End) boxes)
        |> List.filter (List.exists (function Some _float -> true | _ -> false))
        |> List.length 
        |> (-) <| connPortsCount