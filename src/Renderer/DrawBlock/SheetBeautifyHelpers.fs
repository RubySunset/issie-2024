module SheetBeautifyHelpers

open CommonTypes
open DrawModelType
open Symbol

//-----------------Module for beautify Helper functions--------------------------//
// Typical candidates: all individual code library functions.
// Other helpers identified by Team



module B1 =
    let B1R (symbol : SymbolT.Symbol) = (symbol.LabelBoundingBox.H,symbol.LabelBoundingBox.W)
    let B1W (symbol:SymbolT.Symbol) (H:float) (W:float) = {symbol with LabelBoundingBox.H = H ; LabelBoundingBox.W = W }


module B2 =
    let B2W (symbol : SymbolT.Symbol) (x:float) (y:float) =
        {symbol with Pos.X =x ;Pos.Y  = y }

module B3 =

    let B3R (symbol : SymbolT.Symbol) (edge : Edge) = symbol.PortMaps.Order[edge]

    let B3W (symbol:SymbolT.Symbol) (edge : Edge) (ports : List<string>) =
        {symbol with PortMaps.Order = Map [edge,ports] }

module B4 =
    let B4R (symbol :SymbolT.Symbol) = symbol.ReversedInputPorts
    let B4W (symbol :SymbolT.Symbol) =
        {symbol with ReversedInputPorts = Some true }
module B5 =
    let B5R (port : Port) (symbol : SymbolT.Symbol) =
         symbol.Pos + getPortPos symbol port

module B6 =
    let B6R (symbol :SymbolT.Symbol) = symbol.LabelBoundingBox

module B7 =

    let B7R (symbol :SymbolT.Symbol) = symbol.STransform.Rotation

    let B7W (symbol :SymbolT.Symbol) (rotation : Rotation) = {symbol with SymbolT.Symbol.STransform.Rotation= rotation }

module B8 =

    let B8R (symbol :SymbolT.Symbol) = symbol.STransform.Flipped
    let B8W (symbol :SymbolT.Symbol) (flip : bool) = {symbol with SymbolT.Symbol.STransform.Flipped = flip }

module T1 =

    let T1R (sheet :SheetT.Model) =
        let boundingBoxes =  List.ofSeq (Map.values sheet.BoundingBoxes)
        let boundingBoxesIndexed= List.mapi (fun i bb ->(i,bb)) boundingBoxes
        let boundingBoxPairs = List.allPairs boundingBoxesIndexed boundingBoxesIndexed
        List.length (List.filter (fun ((i1,bb1), (i2,bb2)) -> ((BlockHelpers.overlap2DBox bb1 bb2) && i1<>i2))  boundingBoxPairs)

module T2 =

    let T2R (sheet :SheetT.Model) =




