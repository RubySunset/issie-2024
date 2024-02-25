module SheetBeautifyHelpers

//-----------------Module for beautify Helper functions--------------------------//
// Typical candidates: all individual code library functions.
// Other helpers identified by Team

open DrawModelType.SymbolT
open CommonTypes
open Optics
open SymbolHelpers

// B1R
let getCustomComponentDimensions (sym: Symbol) : float * float = 
    // Use of lens already provided for getting bounding box
    match sym.Component.Type with
    | Custom _ ->
        let box = fst labelBoundingBox_ sym
        box.W, box.H
    | _ -> failwithf "Non-custom component pattern matching with get"

// B1W
let setCustomComponentDimensions 
    (newDim: float * float) // This is length in x by length in y
    (sym: Symbol) 
    : Symbol =
    match sym.Component.Type with
    | Custom _ ->
        let box = fst labelBoundingBox_ sym
        let newBox = {box with W = fst newDim; H = snd newDim}
        snd labelBoundingBox_ newBox sym
    | _ -> failwithf "Non-custom component pattern matching with set"

let customComponentDimension_ = Lens.create getCustomComponentDimensions setCustomComponentDimensions