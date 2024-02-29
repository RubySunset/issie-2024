module SheetBeautifyHelpers
//-----------------Module for beautify Helper functions--------------------------//
// Typical candidates: all individual code library functions.
// Other helpers identified by Team

open CommonTypes
open DrawModelType.SymbolT
open Optics
open Operators

module IndividualPhaseWork =
    // B1RW Reads or Writes the dimensions (H and W) of a custom component symbol

    // Bad as would need to use both get and set for both vars if only needed to set one of either W or H
    // Also the getter will always return both H and W sometimes unnecessarily

    let private initial_CustomCompHW_' =
        // can take these out at just use customCompH_ and customCompW_
        let opticH = (component_ >-> h_)
        let opticW = (component_ >-> w_)
        Lens.create
            // Original Code
            // (fun sym -> match sym.Component.Type with | Custom _ -> getRotatedHAndW sym | _ -> 0, 0)
            // (fun (h, w) sym -> setCustomCompHW h w sym)
            // (fun symbol -> symbol.Component.H, symbol.Component.W)
            (fun symbol -> Optic.get opticH symbol, Optic.get opticW symbol)
            (fun (h, w) symbol -> symbol |> Optic.set opticH h |> Optic.set opticW w)

    // Better as can be nicely pipelined if both need to be set and code is easier to read
    // Although a little more verbose if wanting to get both
    // e.g. let h, w = Optic.get customCompH_ symbol, Optic.get customCompW_ symbol

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
    /// |> Optic.set customCompW_ w
    /// ```
    let symCompH_ = component_ >-> h_
        // let optic = component_ >-> h_
        // Lens.create
        //     // (fun symbol -> symbol.Component.H)
        //     (fun symbol -> Optic.get optic symbol)
        //     (fun h symbol -> Optic.set optic h symbol)

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
    /// |> Optic.set customCompH_ h
    /// |> Optic.set customCompW_ w
    /// ```
    let symCompW_ = component_ >-> w_
        // let optic = component_ >-> w_
        // Lens.create
        //     // (fun symbol -> symbol.Component.H)
        //     (fun symbol -> Optic.get optic symbol)
        //     (fun w symbol -> Optic.set optic w symbol)

    // Use both based on need

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
    /// Optic.set customCompHW_ (newH, w) symbol
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

    // portOrder_ used from SymbolInfo
    /// Lens for the port order of a symbol
    /// ```(Symbol.PortMaps.Order)```
    let symPortOrder_ = portMaps_ >-> order_
        // symbol.PortMaps.Order[side]
        // let optic = portMaps_ >-> order_
        // Lens.create
        //     (fun symbol -> Optic.get optic symbol)
        //     (fun order symbol -> Optic.set optic order symbol)

    /// Returns the port order for the given side of a symbol
    let getSymbolSidePortOrder (symbol: Symbol) (side: Edge) =
        // symbol.PortMaps.Order[side]
        Optic.get symPortOrder_ symbol
        |> Map.tryFind side // Fairly sure tryFind is unnecessary but as explained below
        |> Option.defaultValue [] // Bad code in future/ other areas may require this

    // Can't use Lens even if symbol is last parameter as Lens.get only takes one input
    // i.e. expecting (Edge -> list<string>) but got Edge -> list<string> in Lens.set
    /// Overwrites the port order for the given side of a symbol
    let setSymbolSidePortOrder (symbol: Symbol) (side: Edge) (ports: string list) =
        Optic.get symPortOrder_ symbol
        |> Map.add side ports
        |> Optic.set symPortOrder_ <| symbol

    /// Lens for the reversed input ports of a symbol
    /// ```(Symbol.ReversedInputPorts)```
    let revInPorts_ =
        Lens.create
            (fun symbol -> symbol.ReversedInputPorts)
            (fun r symbol -> { symbol with ReversedInputPorts = r })

    /// Returns the position of a port on the sheet
    let getSheetPortPos (symbol: Symbol) (port: Port) =
        (Symbol.getPortPos symbol port) + symbol.Pos

    /// Returns the bouding box of a symbol
    let getSymbolBoundingBox (symbol: Symbol) =
        Symbol.getSymbolBoundingBox symbol

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

    /// Lens for the rotation of a symbol
    /// ```(Symbol.STransform.Rotation)```
    let symRotation_ = sTransform_ >-> rotation_

    /// Lens for the flip of a symbol
    /// ```(Symbol.STransform.Flipped)```
    let symFlipped_ = sTransform_ >-> flipped_