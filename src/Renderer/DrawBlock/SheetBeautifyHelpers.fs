module SheetBeautifyHelpers

//--------------------Module for beautify Helper functions--------------------//
// Typical candidates: all individual code library functions.
// Other helpers identified by Team

module Lenses =
    open Optics
    open Optics.Operators
    open CommonTypes
    open DrawModelType
    open DrawModelType.SymbolT
    
    
    (* Useful Lenses and Records and things
    SheetT.boundingBoxes_ - Lens<SheetT, BoundingBox list>
    CommonTypes.BoundingBox - Bounding Box type
    CommonTypes.size_ - Lens<BoundingBox, Size>
    *)

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
                symbol.CentrePos + { X = 0.0; Y = symbol.Component.H / 2.0 }
            | Right -> 
                symbol.CentrePos + { X = symbol.Component.W / 2.0; Y = 0.0 }
            | Bottom -> 
                symbol.CentrePos - { X = 0.0; Y = symbol.Component.H / 2.0 }
            | Left -> 
                symbol.CentrePos - { X = symbol.Component.W / 2.0; Y = 0.0 }

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


let T1R = ()
let T2R = ()
let T3R = ()
let T4R = ()
let T5R = ()
let T6R = ()

let Ext = ()  // See `TestDrawBlock.HLPTick3.visibleSegments`


(****************************Other Random Functions****************************

/// check that the port order list is valid
let checkValidIndices portOrderList =
    let portOrderSet = Set.ofList portOrderList
    portOrderSet = Set.ofList [0..portOrderList.Length-1]

******************************************************************************)



/// TODO: Move to TestDrawBlock
module TestDrawblockD2 =
    () // TODO: your section of the Project
    // rotate, flip, alter port order :)
    // but no need to alter position or scales :)