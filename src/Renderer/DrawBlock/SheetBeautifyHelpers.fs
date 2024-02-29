module SheetBeautifyHelpers
open DrawModelType
open DrawModelType.SymbolT
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

// helpers:

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





    


    
