module SheetBeautifyHelpers
open DrawModelType
open DrawModelType.SymbolT
open Optics
open CommonTypes
open Symbol
open RotateScale
open SymbolHelpers
open BlockHelpers

//-----------------Module for beautify Helper functions--------------------------//
// Typical candidates: all individual code library functions.
// Other helpers identified by Team


(*   B1: Functions to get and set the dimensions of a custom component   *)
// check these later
/// TODO: for this check if you will need to use the function from SymbolHelpers.fs using getCustomSymCorners
/// TODO: or use getRotatedHAndW
/// TODO: if have mroe time maybe implement the getter and setter for dimentions seperately


// this returns a tuple in the conventional mathematical manner assuming width corresponds to x and height corresponds to y
let cusotmCompDimGetter (customSymbol : SymbolT.Symbol) : (float * float) = 
    // get the dimentions of a custom symbol considering any rotationa and scaling
    let dims = (getCustomSymCorners customSymbol)[2]
    (dims.X, dims.Y)
    
    


// TODO: check from the BlockHelper.fs if you can use setCustomCompHW
let customCompDimSetter ( width : float , height : float ) (customSymbol : SymbolT.Symbol) = 
    setCustomCompHW height width customSymbol

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
    let offset = newPos - symbol.Pos
    moveSymbol offset symbol







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

let reversedInputPortsGetter (symbol : SymbolT.Symbol) : option<bool> = 
    symbol.ReversedInputPorts

let reversedInputPortsSetter (reversed : option<bool>) (symbol : SymbolT.Symbol) : SymbolT.Symbol = 
    { symbol with ReversedInputPorts = reversed }

// make the lens for this 
let reversedInputPortsLens_ = Lens.create reversedInputPortsGetter reversedInputPortsSetter








(*B5*)


let getPortPosOnSheet (symbol : SymbolT.Symbol) (portName : Port) : XYPos=
    // get the offset of port relative to topleft pos of symbol
    let offset = getPortPos symbol portName
    // add the offset to the Pos of the symbol, you need operator overloading for this, open Operators maybe
    symbol.Pos + offset






(*B6*)

let getBoundingBox (symbol : SymbolT.Symbol) = 
    {TopLeft = symbol.LabelBoundingBox.TopLeft ; W = symbol.LabelBoundingBox.W ; H = symbol.LabelBoundingBox.H}
    // or just do this 
    //getSymbolBoundingBox symbol
    // or
    //symbol |> fst labelBoundingBox_







(*B7*)
// maybe use the function in line 477 from RotateScale.fs
let rotationStateOfSymbolGetter (symbol : SymbolT.Symbol) : Rotation = 
    symbol.STransform.Rotation

let rotationStateOfSymbolSetter (rotationState : Rotation) (symbol : SymbolT.Symbol) = 
    { symbol with STransform = {symbol.STransform with Rotation = rotationState} }
    // or 
    // rotateSymbolInBlock rotationState symbol.CentrePos symbol

// make the lens for this
let rotationStateOfSymbolLens_ = Lens.create rotationStateOfSymbolGetter rotationStateOfSymbolSetter







(*B8*)

let flipStateOfSymbolGetter (symbol : SymbolT.Symbol) : bool = 
    symbol.STransform.Flipped

let flipStateOfSymbolSetter (flipState : bool) (symbol : SymbolT.Symbol) = 
    { symbol with STransform = {symbol.STransform with Flipped = flipState} }
    // or
    // flipSymbolInBlock flipState symbol.CentrePos symbol

// make lens for this
let flipStateOfSymbolLens_ = Lens.create flipStateOfSymbolGetter flipStateOfSymbolSetter


    


    
