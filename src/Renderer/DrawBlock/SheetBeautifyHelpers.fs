module SheetBeautifyHelpers

//-----------------Module for beautify Helper functions--------------------------//
// Typical candidates: all individual code library functions.
// Other helpers identified by Team

open DrawModelType.SymbolT
open CommonTypes
open Optics

// B1R
let getCustomComponentDimensions (sym: Symbol) : float * float = 
    // Use of lens already provided for getting bounding box
    match sym.Component.Type with
    | Custom _ ->
        let box = sym.LabelBoundingBox
        box.W, box.H
    | _ -> failwithf "Non-custom component pattern matching with get"

// B1W
let setCustomComponentDimensions 
    (newDim: float * float) // This is length in x by length in y
    (sym: Symbol) 
    : Symbol =
    match sym.Component.Type with
    | Custom _ ->
        let box = sym.LabelBoundingBox
        let newBox = {box with W = fst newDim; H = snd newDim}
        snd labelBoundingBox_ newBox sym // Using provided lenses
    | _ -> failwithf "Non-custom component pattern matching with set"

// B1 Lens
let customComponentDimension_ = Lens.create getCustomComponentDimensions setCustomComponentDimensions

// B2
let setSymbolPos (sym: Symbol) (pos: XYPos) =
    {sym with Pos=pos}

// B3R
// Read port order on specified side of a symbol.
let getPortOrder (side: Edge) (sym: Symbol) =
    sym.PortMaps.Order[side]

// B3W
// Write port order on a specified side of a symbol
let setPortOrder (side: Edge) (order: string list) (sym: Symbol) =
    let oldPortOrder = sym.PortMaps.Order
    let newPortOrder = Map.add side order oldPortOrder
    {sym with PortMaps={sym.PortMaps with Order=newPortOrder}}

// B3 Lens
let changePortOrder_ (edge: Edge) = Lens.create (getPortOrder edge) (setPortOrder edge)

// B4R -- Don't understand requirement

// B5R
