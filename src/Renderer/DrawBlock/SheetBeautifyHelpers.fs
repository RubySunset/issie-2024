module SheetBeautifyHelpers

//--------------------Module for beautify Helper functions--------------------//
// Typical candidates: all individual code library functions.
// Other helpers identified by Team

module Lenses =
    open Optics
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
    let repositionSymbol (sheetModel: SheetT.Model) (compID: ComponentId) (newCentrePos: XYPos) =
        let symbol = Optic.get (SheetT.symbolOf_ compID) sheetModel
        let offset = newCentrePos - symbol.CentrePos

        Optic.set (SheetT.symbolOf_ compID) {symbol with CentrePos = symbol.CentrePos + offset; Pos = symbol.Pos + offset}

    
    let B3RW = ()  // can surely do something with Symbol record


    /// A lens that allows getting and setting of `ReversedInputPorts` option\
    /// for a `Mux2` symbol
    let mux2ReversedState_ = Lens.create (fun symbol -> symbol.ReversedInputPorts) (fun stateOption symbol -> {symbol with ReversedInputPorts = stateOption})


    let B5R = ()
    let B6R = ()
    let B7RW = ()
    let B8RW = ()


let T1R = ()
let T2R = ()
let T3R = ()
let T4R = ()
let T5R = ()
let T6R = ()

let Ext = ()  // See `TestDrawBlock.HLPTick3.visibleSegments`



/// TODO: Move to TestDrawBlock
module TestDrawblockD2 =
    () // TODO: your section of the Project
    // rotate, flip, alter port order :)
    // but no need to alter position or scales :)