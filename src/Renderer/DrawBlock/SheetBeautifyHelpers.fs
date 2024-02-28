module SheetBeautifyHelpers
open DrawModelType
open DrawModelType.SymbolT
open Optics
open CommonTypes

//-----------------Module for beautify Helper functions--------------------------//
// Typical candidates: all individual code library functions.
// Other helpers identified by Team


(*   B1: Functions to get and set the dimensions of a custom component   *)

// this returns a tuple in the conventional mathematical manner assuming width corresponds to x and height corresponds to y
let cusotmCompDimGetter (customSymbol : SymbolT.Symbol) : (float * float) = 
    // Assuming there will be no need to check if the component is a custom symbol
    let width = customSymbol.LabelBoundingBox.W
    let height = customSymbol.LabelBoundingBox.H
    (width, height)
    // alternative method using the provided lens
    // get the custom box using lens
    // let customBox = 
    //     customSymbol
    //     |> fst labelBoundingBox_
    // (customBox.W, customBox.H)



// reminder function signature for a setter in lens is 'b -> 'a -> 'a not 'a -> 'b -> 'a, this caused a bug in the code when making the lens
// new dimensions passed as a 2-tuple to match the getter for making the lens
// again assuming width is for x dimension and height is for y dimension
let customCompDimSetter ( width : float , height : float ) (customSymbol : SymbolT.Symbol) : SymbolT.Symbol = 
    { customSymbol with LabelBoundingBox = { customSymbol.LabelBoundingBox with W = width; H = height}}

    // alternative method using the provided lens
    // let currBox = customSymbol |> fst labelBoundingBox_
    // let newBox = {currBox with W = width; H = height}
    // (newBox, customSymbol) ||> snd labelBoundingBox_

// make a lens for the custom component dimensions
let customCompDimLens_ = Lens.create cusotmCompDimGetter customCompDimSetter


(* B2W *)
// using the provided lens to set the position of a symbol
let symbolPosSetter (newPos : XYPos) (symbol : SymbolT.Symbol) : SymbolT.Symbol = 
    let getSetter = snd posOfSym_
    getSetter newPos symbol


(* B3 *)

// getter function for the order of ports on a specific side of a symbol
let symbolEdgePortOrderGetter (symbol : SymbolT.Symbol) (side : Edge) : list<string> = 
    // get port maps using a lens
    let portsMap = symbol |> fst portMaps_
    // get order field of the ports map
    let order = portsMap |> fst order_
    // return the list of ports on the given side in correct order
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

// not possible to make a lens for this using Lens.create
// I can make a custom lens where getter would be 'a -> 'b -> 'c and the setter would be 'c -> 'b -> 'a -> 'a but it would be bad practice
 


    



    
