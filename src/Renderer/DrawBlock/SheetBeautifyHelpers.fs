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

    /// Lens for Symbol.Component.H and Symbol.Component.W
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
    let customCompHW_ =
        Lens.create
            // Original Code
            // (fun sym -> match sym.Component.Type with | Custom _ -> getRotatedHAndW sym | _ -> 0, 0)
            // (fun (h, w) sym -> setCustomCompHW h w sym)
            (fun symbol -> symbol.Component.H, symbol.Component.W)
            (fun (h, w) symbol -> symbol |> Optic.set (component_ >-> h_) h |> Optic.set (component_ >-> w_) w)

    // Better as can be nicely pipelined if both need to be set and code is easier to read
    // Although a little more verbose if wanting to get both 
    // e.g. let h, w = Optic.get customCompH_ symbol, Optic.get customCompW_ symbol

    /// Lens for Symbol.Component.H
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
    let customCompH_ =
        Lens.create
            (fun symbol -> symbol.Component.H)
            (fun h symbol -> symbol |> Optic.set (component_ >-> h_) h)

    /// Lens for Symbol.Component.W
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
    let customCompW_ =
        Lens.create
            (fun symbol -> symbol.Component.W)
            (fun w symbol -> symbol |> Optic.set (component_ >-> w_) w)
