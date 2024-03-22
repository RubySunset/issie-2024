module sheetbd1
open DrawModelType
open DrawModelType.BusWireT
open Microsoft.FSharp.Collections
open BlockHelpers
open SheetBeautifyHelpers.SegmentHelpers
open Optics.Optic

let getParallelWires (sheet: SheetT.Model) =
    sheet.Wire.Wires
    |> Map.values
    |> Seq.toList
    |> List.filter (fun wire ->
        (List.length (visibleSegments wire.WId sheet))= 3)

let getSymbolsList (sheet: SheetT.Model) =
    sheet.Wire.Symbol.Symbols
    |> Map.values
    |> Seq.toList

let getSinglyConnectedSymbols (sheet: SheetT.Model) =
    getSymbolsList sheet
    |> List.groupBy id
    |> List.filter (fun (_, group) -> List.length group = 1)
    |> List.map fst
    |> Set.ofList

let straightenWiresOnce (sheet: SheetT.Model) =
    let parallelWires = getParallelWires sheet
    let singlyConnectedSymbols = getSinglyConnectedSymbols sheet
    let sourceSymbols =
        List.map (fun wire -> (getSourceSymbol sheet.Wire wire), wire) parallelWires

    let targetSymbols =
        List.map (fun wire -> (getTargetSymbol sheet.Wire wire), wire) parallelWires

    let sourceTargetConnections =
        List.allPairs sourceSymbols targetSymbols
        |> List.filter (fun ((_s1, w1), (_s2, w2)) -> w1 = w2)
        |> List.map (fun ((s1, w), (s2, _w2)) -> (s1, s2, visibleSegments w.WId sheet))

    let updatedSymbols =
        sourceTargetConnections
        |> List.map (fun (s1, s2, vl) -> moveSymbol (vl[1]) s1)
        |> List.map (fun sym -> (sym.Id, sym))

        |> Map.ofList
        |> Map.fold (fun acc key value -> Map.add key value acc) sheet.Wire.Symbol.Symbols

    sheet
    |> set SheetT.symbols_ updatedSymbols
    |> map SheetT.wire_ (BusWireSeparate.reRouteWiresFrom (updatedSymbols.Keys |> Seq.toList))


let D1sheetAlignScaleStarter (sheet: SheetT.Model) =


    let rec straightenWires (sheet: SheetT.Model) =
        if (getParallelWires sheet |> List.length) = 0 then
            sheet
        else
            let newSheet = straightenWiresOnce sheet
            if (getParallelWires sheet |> List.length)  <= (getParallelWires newSheet |> List.length) then
                sheet
            else
                straightenWires newSheet

    straightenWires sheet





// sourceTargetConnections
// |> List.map (fun (s1, s2) ->
//     if (Set.contains s1 singlyConnectedSymbols) then
//         Some(updateSymPos s1 (s2.Pos.X + segmentVector.X) (s2.Pos.Y + segmentVector.Y))
//     elif Set.contains s2 singlyConnectedSymbols then
//         Some(updateSymPos s2 (s1.Pos.X + segmentVector.X) (s1.Pos.Y + segmentVector.Y))
//     else
//         None)

//
// module SheetBeautifyD1
// // open modules likely to be used
// open CommonTypes
// open DrawHelpers
// open DrawModelType
// open DrawModelType.SymbolT
// open DrawModelType.BusWireT
// open DrawModelType.SheetT
// open SymbolUpdate
// open SheetUpdateHelpers
// open SheetBeautifyHelpers
// open Optics
// open BlockHelpers
// open SegmentHelpers
//
// /// <summary>
// /// Filters parallel wires from the model based on the number of visible segments.
// /// </summary>
// /// <param name="model">The sheet model containing wires and segments.</param>
// /// <returns>List of wires with exactly three visible segments.</returns>
// let parallelWires (model: SheetT.Model) =
//     Map.values model.Wire.Wires
//     |> List.ofSeq
//     |> List.filter (fun wire -> List.length (visibleSegments wire.WId model) >= 3)
//
// /// <summary>
// /// Gets all single-connected symbols associated with a list of wires in the model.
// /// </summary>
// /// <param name="model">The sheet model containing wires and symbols.</param>
// /// <param name="wires">List of wires to consider for symbol connections.</param>
// /// <returns>List of single-connected symbols.</returns>
// let getAllSingleConnectedSymbols (model: SheetT.Model) (wires: Wire list) =
//     let sourceTargetSymbols =
//         List.map (fun wire -> (getSourceSymbol model.Wire wire, getTargetSymbol model.Wire wire, wire)) wires
//
//     let getSingleConnectedSymbols (symbolList: (Symbol * Symbol * Wire) list) =
//         symbolList
//         |> List.groupBy (fun sym -> sym)
//         |> List.filter (fun (_, symList) -> List.length symList = 1)
//         |> List.map (fun (sym, _) -> sym)
//         // |> (fun symList ->
//         //     match symList with
//         //     | [] -> []
//         //     | (s1,t1,w1)::(s2,t2,w2)::rest -> (s1,t1,w1, t1=s1)
//
//         // )
//     let singleConnected = getSingleConnectedSymbols sourceTargetSymbols
//     let sourceSymbols = List.map (fun (s, _, _) -> s.Id) singleConnected
//     let targetSymbols = List.map (fun (_, t, _) -> t.Id) singleConnected
//
//     let moveSource sl tl s t=
//         (not (List.contains s tl) || List.contains t sl)
//
//
//     singleConnected
//     |> List.map (fun (s, t, w) -> (s, t, w, moveSource sourceSymbols targetSymbols s.Id t.Id))
//
//     // let moveSourceSymbol (symbolList:(Symbol * Symbol * Wire) list) =
//     //     match symbolList with
//     //     | [] -> []  // Base case: Empty list, return empty list
//     //     | (s1, t1, w1)::rest ->
//     //         let firstTupleResult = (s1, t1, w1, true)
//     //         let rec move (prevT : Symbol, rest) =
//     //             match rest with
//     //             | [] -> []
//     //             | (s, t, w) :: tail ->
//     //                 let newTuple = (s, t, w, prevT.Id <> s.Id)
//     //                 newTuple :: move (t, tail)
//     //         firstTupleResult :: move (t1, rest)
//     //     // | [(s1, t1, w1)] -> [(s1, t1, w1, true)]
//
//
//
//
//
//     // getSingleConnectedSymbols sourceTargetSymbols
//     // |> moveSourceSymbol
//
// /// <summary>
// /// Gets all symbols associated with parallel wires in the model.
// /// </summary>
// /// <param name="model">The sheet model containing wires and symbols.</param>
// /// <returns>List of symbols connected by parallel wires.</returns>
// let getAllSymbols (model: SheetT.Model) =
//     getAllSingleConnectedSymbols model (parallelWires model)
//
// /// <summary>
// /// Gets the bounding boxes of symbols associated with parallel wires in the model.
// /// </summary>
// /// <param name="model">The sheet model containing wires and symbols.</param>
// /// <returns>List of bounding boxes of symbols connected by parallel wires.</returns>
// let getSymbolsBoundingBox (model: SheetT.Model) =
//     let symbols = getAllSymbols model
//     symbols
//     |> List.map (fun (symS, symT, _, _) -> getSymBoundingBox symS)
//
// /// <summary>
// /// Aligns all singly connected symbols by their target port.
// /// </summary>
// /// <param name="model">The sheet model containing wires and symbols</param>
// /// <returns>Updated model with the moved symbols</returns>
// let alignSingleConnectedSyms (model: SheetT.Model) (syms) =
//     // let syms = getAllSingleConnectedSymbols model (parallelWires model)
//                 //Prefer moving symbols with only one port to have minimal impact on the rest of the sheet
//                 //Need to modify the delta function to account for different directions when moving source/target
//                 // |> List.map (fun (s,t,w) ->
//                 //                 printfn $"{s.PortMaps.Order.Count}, {t.PortMaps.Order.Count}"
//                 //                 match (s.PortMaps.Order.Count, t.PortMaps.Order.Count) with
//                 //                 | (1,1) -> (s,w)
//                 //                 | (_, 1) -> (t,w)
//                 //                 | (_, _) -> (s,w)
//                 //             )
//     let checkXCoor (s1: Symbol) (s2: Symbol) (b: bool) =
//         // printfn "Distance between %s and %s: %f" s1.Component.Label s2.Component.Label (abs(s1.Pos.X - s2.Pos.X))
//         let sub = if b then 1 else -1
//         if abs(s1.Pos.X - s2.Pos.X) < 60 then
//             (-100*sub)
//         else 0
//     let delta (wire:Wire) (b:bool) (x: int) =
//         let sub = if b then -1. else 1.
//         match wire.InitialOrientation with
//         | Horizontal -> {X=x; Y=wire.EndPos.Y - (wire.StartPos.Y)}
//         | Vertical -> {X=wire.EndPos.X - (wire.StartPos.X); Y=0}
//
//     let symbolMap = Optic.get symbols_ model
//     let movedSyms = List.map (fun (s,t,w, b) -> moveSymbol (checkXCoor s t b |> delta w b) (if b then s else t)) syms
//     let symbols' =  (symbolMap, movedSyms)
//                     ||> List.fold (fun s movedSym ->
//                         s |> Map.map (fun _ v ->
//                             match v.Id with
//                             | id when id = movedSym.Id -> movedSym
//                             | _ -> v
//                         ))
//     // let rec moveSourceSymbol_ (symbolList) =
//     //     match symbolList with
//     //     | [] -> []  // Base case: Empty list, return empty list
//     //     | (s1, t1, w1)::(s2, t2, w2)::rest ->
//     //         let firstTupleResult = (s1, t1, w1, true)
//     //         let rec move (prevT, rest) =
//     //             match rest with
//     //             | [] -> []
//     //             | (s, t, w) :: tail ->
//     //                 let newTuple = (s, t, w, prevT <> s)
//     //                 newTuple :: move (t, tail)
//     //         firstTupleResult :: move (t1, rest)
//     //     | [(s1, t1, w1)] -> [(s1, t1, w1, true)]
//
//     // let testList = [(1, 2, "wire1"); (2, 3, "wire2"); (3, 4, "wire3"); (4, 5, "wire4"); (5, 6, "wire5")]
//
//     // let result = moveSourceSymbol_ testList
//
//     // printfn "Result: %A" result
//
//
//
//
//
//
//
//     let NewSymbolModel = Optic.set symbols_ symbols' model //Update the model with the new symbol positions
//
//     //Getting Wire map to update to new wire positions based on updated Symbol postions
//     let wireMap = Optic.get wires_ model
//     let movedWires = wireMap |> Map.values |> List.ofSeq
//                     |> List.map (fun w ->
//                     BusWireUpdateHelpers.autoroute NewSymbolModel.Wire w)
//
//     let wires' =  (wireMap, movedWires)
//                     ||> List.fold (fun w movedWire ->
//                         w |> Map.map (fun _ v ->
//                             match v.WId with
//                             | id when id = movedWire.WId -> movedWire
//                             | _ -> v
//                         ))
//     //Updating the model with the new wire positions
//     // let intersectingPair = numOfIntersectedSymPairs NewSymbolModel
//     // printfn "Number of intersecting pairs: %d" intersectingPair
//     Optic.set wires_ wires' NewSymbolModel
//
//
//
// /// <summary>
// /// Aligns and scales symbols based on the positions and bounding boxes of connected symbols.
// /// </summary>
// /// <param name="model">The sheet model containing wires and symbols.</param>
// let sheetAlignScale (model: SheetT.Model) =
//     // let symbols = getAllSymbols model
//     let syms = getAllSingleConnectedSymbols model (parallelWires model)
//     printfn "Running SheetAlign %A" <| List.map (fun(s,t,w,b)->(s.Component.Label, t.Component.Label, w.WId,b)) syms
//
//     let rec runAlignSingleConnectedSyms model symList count =
//         match symList with
//         | [] -> model
//         | _ ->
//             if List.length symList > 0 && count < 10 then //
//                 let model' = alignSingleConnectedSyms model symList
//                 let updatedSymList = getAllSingleConnectedSymbols model' (parallelWires model')
//                 runAlignSingleConnectedSyms model' updatedSymList (count + 1)
//             else
//                 model
//     //
//     runAlignSingleConnectedSyms model syms 0
//     // model
//     // printfn "Running SheetAlign %A" <| List.map (fun(s,t,w,b)->(s.Component.Label, t.Component.Label, w.WId,b)) syms
//     // alignSingleConnectedSyms model syms
//
//
//
//     // let boundingBoxes = getSymbolsBoundingBox model |> List.choose (fun bb -> Some(bb))
//
//     // let rec groupInPairs list =
//     //     match list with
//     //     | x :: y :: rest -> (x, y) :: groupInPairs rest
//     //     | [x] -> [(x, x)]
//     //     | _ -> []
//
//     // let pairsBoundingBox = groupInPairs boundingBoxes
//     // symbols
//     // |> List.map (fun (symS, symT, _,_) -> updateSymPosInSheet symS.Id symT.Pos)
