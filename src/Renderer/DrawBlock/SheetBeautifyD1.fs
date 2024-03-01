module Renderer.DrawBlock.SheetBeautifyD1


open CommonTypes
open DrawModelType
open Fable.Core.JS
open Microsoft.FSharp.Collections
open SheetBeautifyHelpers
open DrawModelType
open DrawModelType
open Fable.React.ReactiveComponents
open Optics.Compose
open Symbol
open BlockHelpers
open Optics

/// <summary>
/// Aligns and scales symbols based on the connections between parallel wires in the sheet.
/// </summary>
/// <param name="sheet">The sheet model containing wires and symbols.</param>
/// <returns>
/// A list of optional symbol positions after alignment and scaling.
/// Some positions are calculated based on connections between parallel wires.
/// </returns>
let D1sheetAlignScaleStarter (sheet:SheetT.Model) =
    let parallelWires = sheet.Wire.Wires
                        |> Map.values
                        |>Seq.toList
                        |>List.filter (fun wire -> (List.length (getNonZeroAbsSegments wire)) = 3  )

    let filterSingletons (list:List<SymbolT.Symbol*BusWireT.Wire>) =
                list
                |>List.map fst
                |> List.groupBy id
                |> List.filter (fun (_, group) -> List.length group = 1)
                |> List.map fst
                |>Set.ofList


    let sourceSymbols = List.map (fun wire -> (getSourceSymbol sheet.Wire wire),wire ) parallelWires

    let targetSymbols = List.map (fun wire -> (getTargetSymbol sheet.Wire wire),wire ) parallelWires


    let sourceSymbolsSingleConnection  = sourceSymbols
                                        |>filterSingletons
    let targetSymbolsSingleConnection  = targetSymbols
                                        |>filterSingletons

    let sourceTargetCombinations = List.allPairs sourceSymbols targetSymbols
                                    |>List.filter (fun ((_s1,w1),(_s2,w2))->w1=w2)
                                    |>List.map (fun ((s1,w),(s2,_w2))-> (s1,s2, getSegmentVector w 2))

    sourceTargetCombinations
    |> List.map (fun (s1, s2, segmentVector ) ->
            if (Set.contains s1 sourceSymbolsSingleConnection) then
                Some (setPositionOfSymbol s1 (s2.Pos.X+segmentVector.X) (s2.Pos.Y+segmentVector.Y))
            elif Set.contains s2 targetSymbolsSingleConnection then
                Some (setPositionOfSymbol s2 (s1.Pos.X+segmentVector.X) (s1.Pos.Y+segmentVector.Y))
            else
                None
        )








