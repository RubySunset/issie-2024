module SheetBeautifyHelpers

//-----------------Module for beautify Helper functions--------------------------//
// Typical candidates: all individual code library functions.
// Other helpers identified by Team

open DrawModelType.SymbolT
open CommonTypes
open Optics
open Symbol

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
let getPortPosition (sheet: Model) (port: Port) =
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
