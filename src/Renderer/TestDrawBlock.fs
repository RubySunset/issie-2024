module TestDrawBlock
open GenerateData
open Elmish


//-------------------------------------------------------------------------------------------//
//--------Types to represent tests with (possibly) random data, and results from tests-------//
//-------------------------------------------------------------------------------------------//
module TestLib =

    /// convenience unsafe function to extract Ok part of Result or fail if value is Error
    let getOkOrFail (res: Result<'a,string>) =
        match res with
        | Ok x -> x
        | Error mess ->
            failwithf "%s" mess


    type TestStatus =
            | Fail of string
            | Exception of string

    type Test<'a> = {
        Name: string
        Samples: Gen<'a>
        StartFrom: int
        /// The 1st argument is the test number: allows assertions that fail on a specific sample
        /// to display just one sample.
        /// The return value is None if test passes, or Some message if it fails.
        Assertion: int -> 'a -> string option
        }

    type TestResult<'a> = {
        TestName: string
        TestData: Gen<'a>
        FirstSampleTested: int
        TestErrors: (int * TestStatus) list
    }

    let catchException name func arg =
        try
            Ok (func arg)
        with
            | e ->
                Error ($"Exception when running {name}\n" + e.StackTrace)
            
    /// Run the Test samples from 0 up to test.Size - 1.
    /// The return list contains all failures or exceptions: empty list => everything has passed.
    /// This will always run to completion: use truncate if text.Samples.Size is too large.
    let runTests (test: Test<'a>) : TestResult<'a>  =
        [test.StartFrom..test.Samples.Size - 1]
        |> List.map (fun n ->
                catchException $"generating test {n} from {test.Name}" test.Samples.Data n
                |> (fun res -> n,res)
           )           
        |> List.collect (function
                            | n, Error mess -> [n, Exception mess]
                            | n, Ok sample ->
                                match catchException $"'test.Assertion' on test {n} from 'runTests'" (test.Assertion n) sample with
                                | Ok None -> []
                                | Ok (Some failure) -> [n,Fail failure]
                                | Error (mess) -> [n,Exception mess])
        |> (fun resL ->                
                {
                    TestName = test.Name
                    FirstSampleTested = test.StartFrom
                    TestData = test.Samples
                    TestErrors =  resL
                })
 
 
            
(******************************************************************************************
   This submodule contains a set of functions that enable random data generation
   for property-based testing of Draw Block wire routing functions.
   basic idea.
   1. Generate, in various ways, random circuit layouts
   2. For each layout apply smartautoroute to regenerate all wires
   3. Apply check functions to see if the resulting wire routing obeys "good layout" rules.
   4. Output any layouts with anomalous wire routing
*******************************************************************************************)
module HLPTick3 =
    open EEExtensions
    open TestLib
    open Optics
    open Optics.Operators
    open Helpers
    open CommonTypes
    open ModelType
    open DrawModelType
    open Sheet.SheetInterface
    open SheetUpdateHelpers
    open SheetUpdate
    open GenerateData
    open DrawHelpers

    /// create an initial empty Sheet Model 
    let initSheetModel = DiagramMainView.init().Sheet

    /// Optic to access SheetT.Model from Issie Model
    let sheetModel_ = sheet_

    /// Optic to access BusWireT.Model from SheetT.Model
    let busWireModel_ = SheetT.wire_

    /// Optic to access SymbolT.Model from SheetT.Model
    let symbolModel_ = SheetT.symbol_

    /// allowed max X or y coord of svg canvas
    let maxSheetCoord = Sheet.Constants.defaultCanvasSize
    let middleOfSheet = {X=maxSheetCoord/2.;Y=maxSheetCoord/2.}

    /// Used throughout to compare labels since these are case invariant "g1" = "G1"
    let caseInvariantEqual str1 str2 =
        String.toUpper str1 = String.toUpper str2
 

    /// Identify a port from its component label and number.
    /// Usually both an input and output port will mathc this, so
    /// the port is only unique if it is known to be input or output.
    /// used to specify the ends of wires, since tehee are known to be
    /// connected to outputs (source) or inputs (target).
    type SymbolPort = { Label: string; PortNumber: int }

    /// convenience function to make SymbolPorts
    let portOf (label:string) (number: int) =
        {Label=label; PortNumber = number}


    //-----------------------------------------------------------------------------------------------
    // visibleSegments is included here as ahelper for info, and because it is needed in project work
    //-----------------------------------------------------------------------------------------------

    /// helper to match even and off integers in patterns (active pattern)
    let (|IsEven|IsOdd|) (n: int) = match n % 2 with | 0 -> IsEven | _ -> IsOdd

    /// The visible segments of a wire, as a list of vectors, from source end to target end.
    /// Note that in a wire with n segments a zero length (invisible) segment at any index [1..n-2] is allowed 
    /// which if present causes the two segments on either side of it to coalesce into a single visible segment.
    /// A wire can have any number of visible segments - even 1.
    let visibleSegments (wId: ConnectionId) (model: SheetT.Model): XYPos list =

        let wire = model.Wire.Wires[wId] // get wire from model

        /// Convert seg into its XY Vector (from start to end of segment).
        /// index must be the index of seg in its containing wire.
        let getSegmentVector (index:int) (seg: BusWireT.Segment) =
            // The implicit horizontal or vertical direction  of a segment is determined by 
            // its index in the list of wire segments and the wire initial direction
            match index, wire.InitialOrientation with
            | IsEven, BusWireT.Vertical | IsOdd, BusWireT.Horizontal -> {X=0.; Y=seg.Length}
            | IsEven, BusWireT.Horizontal | IsOdd, BusWireT.Vertical -> {X=seg.Length; Y=0.}

        /// Return the list of segment vectors with 3 vectors coalesced into one visible equivalent
        /// wherever this is possible
        let rec coalesce (segVecs: XYPos list)  =
            match List.tryFindIndex (fun segVec -> segVec =~ XYPos.zero) segVecs[1..segVecs.Length-2] with          
            | Some zeroVecIndex ->
                let index = zeroVecIndex + 1 // base index as it should be on full segVecs
                segVecs[0..index-2] @
                [segVecs[index-1] + segVecs[index+1]] @
                segVecs[index+2..segVecs.Length - 1]
                |> coalesce
            | None -> segVecs
     
        wire.Segments
        |> List.mapi getSegmentVector
        |> coalesce
                


//------------------------------------------------------------------------------------------------------------------------//
//------------------------------functions to build issue schematics programmatically--------------------------------------//
//------------------------------------------------------------------------------------------------------------------------//
    module Builder =
        /// Place a new symbol with label symLabel onto the Sheet with given position.
        /// Return error if symLabel is not unique on sheet, or if position is outside allowed sheet coordinates (0 - maxSheetCoord).
        /// To be safe place components close to (maxSheetCoord/2.0, maxSheetCoord/2.0).
        /// symLabel - the component label, will be uppercased to make a standard label name
        /// compType - the type of the component
        /// position - the top-left corner of the symbol outline.
        /// model - the Sheet model into which the new symbol is added.
        let placeSymbol (symLabel: string) (compType: ComponentType) (position: XYPos) (model: SheetT.Model) : Result<SheetT.Model, string> =
            let symLabel = String.toUpper symLabel // make label into its standard casing
            let symModel, symId = SymbolUpdate.addSymbol [] (model.Wire.Symbol) position compType symLabel
            let sym = symModel.Symbols[symId]
            match position + sym.getScaledDiagonal with
            | {X=x;Y=y} when x > maxSheetCoord || y > maxSheetCoord ->
                Error $"symbol '{symLabel}' position {position + sym.getScaledDiagonal} lies outside allowed coordinates"
            | _ ->
                model
                |> Optic.set symbolModel_ symModel
                |> SheetUpdateHelpers.updateBoundingBoxes // could optimise this by only updating symId bounding boxes
                |> Ok
        


    
        /// Place a new symbol onto the Sheet with given position and scaling (use default scale if this is not specified).
        /// The ports on the new symbol will be determined by the input and output components on some existing sheet in project.
        /// Return error if symLabel is not unique on sheet, or ccSheetName is not the name of some other sheet in project.
        let placeCustomSymbol
                (symLabel: string)
                (ccSheetName: string)
                (project: Project)
                (scale: XYPos)
                (position: XYPos)
                (model: SheetT.Model)
                    : Result<SheetT.Model, string> =
           let symbolMap = model.Wire.Symbol.Symbols
           if caseInvariantEqual ccSheetName project.OpenFileName then
                Error "Can't create custom component with name same as current opened sheet"        
            elif not <| List.exists (fun (ldc: LoadedComponent) -> caseInvariantEqual ldc.Name ccSheetName) project.LoadedComponents then
                Error "Can't create custom component unless a sheet already exists with smae name as ccSheetName"
            elif symbolMap |> Map.exists (fun _ sym ->  caseInvariantEqual sym.Component.Label symLabel) then
                Error "Can't create custom component with duplicate Label"
            else
                let canvas = model.GetCanvasState()
                let ccType: CustomComponentType =
                    {
                        Name = ccSheetName
                        InputLabels = Extractor.getOrderedCompLabels (Input1 (0, None)) canvas
                        OutputLabels = Extractor.getOrderedCompLabels (Output 0) canvas
                        Form = None
                        Description = None
                    }
                placeSymbol symLabel (Custom ccType) position model
            
        /// Add a (newly routed) wire, source specifies the Output port, target the Input port.
        /// Return an error if either of the two ports specified is invalid, or if the wire duplicates and existing one.
        /// The wire created will be smart routed but not separated from other wires: for a nice schematic
        /// separateAllWires should be run after  all wires are added.
        /// source, target: respectively the output port and input port to which the wire connects.
        let placeWire (source: SymbolPort) (target: SymbolPort) (model: SheetT.Model) : Result<SheetT.Model,string> =
            let symbols = model.Wire.Symbol.Symbols
            let getPortId (portType:PortType) symPort =
                mapValues symbols
                |> Array.tryFind (fun sym -> caseInvariantEqual sym.Component.Label symPort.Label)
                |> function | Some x -> Ok x | None -> Error "Can't find symbol with label '{symPort.Label}'"
                |> Result.bind (fun sym ->
                    match portType with
                    | PortType.Input -> List.tryItem symPort.PortNumber sym.Component.InputPorts
                    | PortType.Output -> List.tryItem symPort.PortNumber sym.Component.OutputPorts
                    |> function | Some port -> Ok port.Id
                                | None -> Error $"Can't find {portType} port {symPort.PortNumber} on component {symPort.Label}")
            
            match getPortId PortType.Input target, getPortId PortType.Output source with
            | Error e, _ | _, Error e -> Error e
            | Ok inPort, Ok outPort ->
                let newWire = BusWireUpdate.makeNewWire (InputPortId inPort) (OutputPortId outPort) model.Wire
                if model.Wire.Wires |> Map.exists (fun wid wire -> wire.InputPort=newWire.InputPort && wire.OutputPort = newWire.OutputPort) then
                        // wire already exists
                        Error "Can't create wire from {source} to {target} because a wire already exists between those ports"
                else
                     model
                     |> Optic.set (busWireModel_ >-> BusWireT.wireOf_ newWire.WId) newWire
                     |> Ok
            

        /// Run the global wire separation algorithm (should be after all wires have been placed and routed)
        let separateAllWires (model: SheetT.Model) : SheetT.Model =
            model
            |> Optic.map busWireModel_ (BusWireSeparate.updateWireSegmentJumpsAndSeparations (model.Wire.Wires.Keys |> Seq.toList))

        /// Copy testModel into the main Issie Sheet making its contents visible
        let showSheetInIssieSchematic (dispatch: Dispatch<Msg>) (testModel: SheetT.Model) =
            let sheetDispatch sMsg = dispatch (Sheet sMsg)
            dispatch <| UpdateModel (Optic.set sheet_ testModel) // set the Sheet component of the Issie model to make a new schematic.
            sheetDispatch <| SheetT.KeyPress SheetT.CtrlW // Centre & scale the schematic to make all components viewable.

        // Applies Beautify to the sheet and displays it in issie
        let applyBeautify beautifyFunc (dispatch: Dispatch<Msg>) (model: ModelType.Model) =
            dispatch <| UpdateDrawBlockTestState(fun _ -> Some {LastTestNumber=0; LastTestSampleIndex=0})
            let sheet = Optic.get sheet_ model
            sheet
            |> beautifyFunc 
            |> fun sheet' -> {sheet' with SheetT.UndoList = appendUndoList sheet.UndoList sheet}
            |> showSheetInIssieSchematic dispatch

        // Undoes a beautify operation and displays the sheet in issie
        let undoBeautify (dispatch: Dispatch<Msg>) (model: ModelType.Model) =
            dispatch <| UpdateDrawBlockTestState(fun _ -> Some {LastTestNumber=1; LastTestSampleIndex=1})
            let sheet = Optic.get sheet_ model
            match sheet.UndoList with
            | [] -> sheet 
            | prevSheet :: lst -> {prevSheet with SheetT.UndoList = appendUndoList sheet.UndoList sheet}
            |> showSheetInIssieSchematic dispatch

        /// 1. Create a set of circuits from Gen<'a> samples by applying sheetMaker to each sample.
        /// 2. Check each ciruit with sheetChecker.
        /// 3. Return a TestResult record with errors those samples for which sheetChecker returns false,
        /// or where there is an exception.
        /// If there are any test errors display the first in Issie, and its error message on the console.
        /// sheetMaker: generates a SheetT.model from the random sample
        /// sheetChecker n model: n is sample number, model is the genrated model. Return false if test fails.
        let runTestOnSheets
            (name: string)
            (sampleToStartFrom: int)
            (samples : Gen<'a>)
            (sheetMaker: 'a -> SheetT.Model)
            (sheetChecker: int -> SheetT.Model -> string option)
            (dispatch: Dispatch<Msg>)
                : TestResult<'a> =
            let generateAndCheckSheet n = sheetMaker >> sheetChecker n
            let result =
                {
                    Name=name;
                    Samples=samples;
                    StartFrom = sampleToStartFrom
                    Assertion = generateAndCheckSheet
                }
                |> runTests
            match result.TestErrors with
            | [] -> // no errors
                printf $"Test {result.TestName} has PASSED."
            | (n,first):: _ -> // display in Issie editor and print out first error
                printf $"Test {result.TestName} has FAILED on sample {n} with error message:\n{first}"
                match catchException "" sheetMaker (samples.Data n) with
                | Ok sheet -> showSheetInIssieSchematic dispatch sheet 
                | Error mess -> ()
            result
//--------------------------------------------------------------------------------------------------//
//----------------------------------------Example Test Circuits using Gen<'a> samples---------------//
//--------------------------------------------------------------------------------------------------//

    open Builder
    /// Sample data based on 11 equidistant points on a horizontal line
    let horizLinePositions =
        fromList [-100..20..100]
        |> map (fun n -> middleOfSheet + {X=float n; Y=0.})

    let randOffsets =
        (randomInt -100 10 100, randomInt -100 10 100)
        ||> product (fun x y -> (x,y))
        |> filter (fun (x,y) -> (abs x >= 60) || (abs y >= 60))
        |> map (fun (n, m) -> {X=float n; Y=float m})

    // Rotate or Flip a symbol
    let flipOrRotateSymbol flipOrRotate model flipOrRotateFunc =
        model
        |> Optic.map symbolModel_ (fun symbModel ->
            flipOrRotateFunc(List.ofSeq symbModel.Symbols.Keys, flipOrRotate)
            |> (fun msg -> SymbolUpdate.update msg symbModel) 
            |> fst)

    // Rotates a symbol by 90, 180 or 270 degrees
    let rotateSymbolN90 n model =
        match n with
        | 1 -> flipOrRotateSymbol Degree90  model SymbolT.RotateAntiClockAng
        | 2 -> flipOrRotateSymbol Degree180 model SymbolT.RotateAntiClockAng
        | 3 -> flipOrRotateSymbol Degree270 model SymbolT.RotateAntiClockAng
        | _ -> model

    // Flips a symbol horizontally or vertically
    let flipSymbolHOrV n model =
        match n with
        | 1 -> flipOrRotateSymbol SymbolT.FlipHorizontal model SymbolT.Flip
        | 2 -> flipOrRotateSymbol SymbolT.FlipVertical   model SymbolT.Flip
        | _ -> model

    // Randomly rotates a symbol by 90, 180 or 270 degrees and then flips it horizontally or vertically
    let randomTransformSymbol model =
        model
        |> rotateSymbolN90 ((randomInt 1 1 4).Data 0)
        |> flipSymbolHOrV  ((randomInt 1 1 3).Data 0)

    /// demo test circuit consisting of a DFF & And gate
    let makeTest1Circuit (andPos:XYPos) =
        initSheetModel
        |> placeSymbol "G1" (GateN(And,2)) andPos
        |> Result.bind (placeSymbol "FF1" DFF middleOfSheet)
        |> Result.bind (placeWire (portOf "G1" 0) (portOf "FF1" 0))
        |> Result.bind (placeWire (portOf "FF1" 0) (portOf "G1" 0) )
        |> getOkOrFail

    /// demo test circuit consisting of a DFF & And gate
    let makeTest2Circuit (offset:XYPos) =
        initSheetModel
        |> placeSymbol "G1" (GateN(And,2)) middleOfSheet
        |> Result.map randomTransformSymbol
        |> Result.bind (placeSymbol "G2" (GateN(And,2)) (middleOfSheet + offset))
        // |> Result.map randomTransformSymbol
        |> Result.bind (placeSymbol "G3" (GateN(And,2)) (middleOfSheet + offset * 2.0))
        // |> Result.map randomTransformSymbol
        |> Result.bind (placeSymbol "G4" (GateN(And,2)) (middleOfSheet + offset * 3.0))
        // |> Result.map randomTransformSymbol
        |> Result.bind (placeSymbol "G5" (GateN(And,2)) (middleOfSheet + offset * 4.0))
        // |> Result.map randomTransformSymbol
        |> Result.bind (placeSymbol "G6" (GateN(And,2)) (middleOfSheet + offset * 5.0))
        // |> Result.map randomTransformSymbol
        |> Result.bind (placeWire (portOf "G1" 0) (portOf "G2" 0))
        |> Result.bind (placeWire (portOf "G2" 0) (portOf "G3" 0) )
        |> Result.bind (placeWire (portOf "G3" 0) (portOf "G4" 0) )
        |> Result.bind (placeWire (portOf "G4" 0) (portOf "G5" 0) )
        |> Result.bind (placeWire (portOf "G5" 0) (portOf "G6" 0) )
        |> getOkOrFail


//------------------------------------------------------------------------------------------------//
//-------------------------Example assertions used to test sheets---------------------------------//
//------------------------------------------------------------------------------------------------//


    module Asserts =

        (* Each assertion function from this module has as inputs the sample number of the current test and the corresponding schematic sheet.
           It returns a boolean indicating (true) that the test passes or 9false) that the test fails. The sample numbr is included to make it
           easy to document tests and so that any specific sampel schematic can easily be displayed using failOnSampleNumber. *)

        /// Ignore sheet and fail on the specified sample, useful for displaying a given sample
        let failOnSampleNumber (sampleToFail :int) (sample: int) _sheet =
            if sampleToFail = sample then
                Some $"Failing forced on Sample {sampleToFail}."
            else
                None

        /// Fails all tests: useful to show in sequence all the sheets generated in a test
        let failOnAllTests (sample: int) _ =
            Some <| $"Sample {sample}"

        /// Fail when sheet contains a wire segment that overlaps (or goes too close to) a symbol outline  
        let failOnWireIntersectsSymbol (sample: int) (sheet: SheetT.Model) =
            let wireModel = sheet.Wire
            wireModel.Wires
            |> Map.exists (fun _ wire -> BusWireRoute.findWireSymbolIntersections wireModel wire <> [])
            |> (function | true -> Some $"Wire intersects a symbol outline in Sample {sample}"
                         | false -> None)

        /// Fail when sheet contains two symbols which overlap
        let failOnSymbolIntersectsSymbol (sample: int) (sheet: SheetT.Model) =
            let wireModel = sheet.Wire
            let boxes =
                mapValues sheet.BoundingBoxes
                |> Array.toList
                |> List.mapi (fun n box -> n,box)
            List.allPairs boxes boxes 
            |> List.exists (fun ((n1,box1),(n2,box2)) -> (n1 <> n2) && BlockHelpers.overlap2DBox box1 box2)
            |> (function | true -> Some $"Symbol outline intersects another symbol outline in Sample {sample}"
                         | false -> None)



//---------------------------------------------------------------------------------------//
//-----------------------------Demo tests on Draw Block code-----------------------------//
//---------------------------------------------------------------------------------------//

    module Tests =

        /// Allow test errors to be viewed in sequence by recording the current error
        /// in the Issie Model (field DrawblockTestState). This contains all Issie persistent state.
        let recordPositionInTest (testNumber: int) (dispatch: Dispatch<Msg>) (result: TestResult<'a>) =
            dispatch <| UpdateDrawBlockTestState(fun _ ->
                match result.TestErrors with
                | [] ->
                    printf "Test finished"
                    None
                | (numb, _) :: _ ->
                    printf $"Sample {numb}"
                    Some { LastTestNumber=testNumber; LastTestSampleIndex= numb})
            
        /// Example test: Horizontally positioned AND + DFF: fail on sample 0
        let test1 testNum firstSample dispatch =
            runTestOnSheets
                "Horizontally positioned AND + DFF: fail on sample 0"
                firstSample
                horizLinePositions
                makeTest1Circuit
                (Asserts.failOnSampleNumber 0)
                dispatch
            |> recordPositionInTest testNum dispatch

        /// Tests Cascaded AND blocks with random rotations and same offsets
        let test2 testNum firstSample dispatch =
            runTestOnSheets
                "Cascaded AND blocks"
                firstSample
                horizLinePositions
                makeTest1Circuit
                (Asserts.failOnSampleNumber 10)
                dispatch
            |> recordPositionInTest testNum dispatch

        /// Example test: Horizontally positioned AND + DFF: fail on symbols intersect
        let test3 testNum firstSample dispatch =
            runTestOnSheets
                "Horizontally positioned AND + DFF: fail on symbols intersect"
                firstSample
                horizLinePositions
                makeTest1Circuit
                Asserts.failOnSymbolIntersectsSymbol
                dispatch
            |> recordPositionInTest testNum dispatch

        /// Example test: Horizontally positioned AND + DFF: fail all tests
        let test4 testNum firstSample dispatch =
            runTestOnSheets
                "Horizontally positioned AND + DFF: fail all tests"
                firstSample
                horizLinePositions
                makeTest1Circuit
                Asserts.failOnAllTests
                dispatch
            |> recordPositionInTest testNum dispatch

        /// List of tests available which can be run ftom Issie File Menu.
        /// The first 9 tests can also be run via Ctrl-n accelerator keys as shown on menu
        let testsToRunFromSheetMenu : (string * (int -> int -> Dispatch<Msg> -> Unit)) list =
            // Change names and test functions as required
            // delete unused tests from list
            [
                "Test1", test1 // example
                "Test2", test2 // example
                "Test3", test3 // example
                "Test4", test4 
                "BeautifyD1", fun _ _ _ -> printf "Running D1 Beautify"; // dummy test - delete line or replace by real test as needed
                "BeautifyD2", fun _ _ _ -> printf "Running D2 Beautify"
                "BeautifyD3", fun _ _ _ -> printf "Running D3 Beautify"
                "Test8", fun _ _ _ -> printf "Test8"
                "Next Test Error", fun _ _ _ -> printf "Next Error:" // Go to the nexterror in a test

            ]

        /// Display the next error in a previously started test
        let nextError (testName, testFunc) firstSampleToTest dispatch =
            let testNum =
                testsToRunFromSheetMenu
                |> List.tryFindIndex (fun (name,_) -> name = testName)
                |> Option.defaultValue 0
            testFunc testNum firstSampleToTest dispatch

        /// common function to execute any test.
        /// testIndex: index of test in testsToRunFromSheetMenuModelType.
        let testMenuFunc (testIndex: int) (dispatch: Dispatch<Msg>) (model: Model) =
            let name,func = testsToRunFromSheetMenu[testIndex] 
            printf "%s" name
            match name, model.DrawBlockTestState with
            | "Next Test Error", Some state ->
                nextError testsToRunFromSheetMenu[state.LastTestNumber] (state.LastTestSampleIndex+1) dispatch
            | "Next Test Error", None ->
                printf "Test Finished"
            | "BeautifyD1", Some state ->
                match state.LastTestNumber with 
                | 0 -> undoBeautify dispatch model
                | _ -> applyBeautify SheetBeautifyD2.sheetOrderFlip dispatch model
                ()
            | "BeautifyD2", Some state ->
                match state.LastTestNumber with 
                | 0 -> undoBeautify dispatch model
                | _ -> applyBeautify SheetBeautifyD2.sheetOrderFlip dispatch model
                // applyBeautify SheetBeautifyD2.sheetOrderFlip dispatch model
                ()
            | "BeautifyD3", Some state ->
                // applyBeautify SheetBeautifyD3.___ dispatch model
                ()
                ()
            | _ ->
                        func testIndex 0 dispatch


/// a module for testing of the sheetOrderFlip function for Sheet Beautification
module TestDrawBlockD2 =
    open Optics
    open Optics.Operators
    open DrawModelType
    open BusWireT
    open CommonTypes
    open HLPTick3
    open HLPTick3.Builder
    open SheetBeautifyHelpers
    (* Design:
    
    - 3 sheet definitions, each hardcoded according to our specifications
        - i.e. will sit down with Roshan to decide what the optimal sheet is

    - 1 sheet maker and 1 sheet checker, 
      to compare the beautified sheets to the exemplar sheets

    - Multiple Sheet distorters, they will get progressively more aggressive
      in the way that they distort the sheet, from just switching two ports
      to then maybe randomising port orders and flipping arbitrary symbols

    Process:
    1. use a sheet maker to generate a circuit from one of the sheet definitions
    2. use a sheet distorter to distort the circuit
    3. apply the sheetOrderFlip function to the distorted circuit
    4. evaluate its performance by seeing how close it got to the original sheet
    5. repeat steps 2-4 for each of the sheet distorters
    6. repeat steps 1-5 for each of the sheet definitions
    *)

    /// source of random numbers
    let random = System.Random()
    
    
    /// provides details on how to generate a component to exact specifications
    type ExactComponent = {
        Name: string;
        Type: ComponentType;
        Position: XYPos;
        Orientation: STransform;
        // TODO: not sure how to define PortOrders and things
    }

    /// provides details on how to construct a wire to exact specifications
    type SimpleWire = {
        Source: HLPTick3.SymbolPort;
        Target: HLPTick3.SymbolPort;
    }
    
    /// a record holding all the information needed by the sheetmaker
    /// to generate a sheet,\
    /// with some extra pre-computed information for use in the evaluation stage
    type SheetDefinition = {
        Name: string;  // must be unique
        Components: List<ExactComponent>;  // to exactly define a component
        Wires: List<SimpleWire>;  // manually defined wire segment lists
        // and any further info that would help evaluation
    }

    let sheetDefinitions: List<SheetDefinition> = [
        {
            Name = "ExemplarSheet1";
            Components = [
                {
                    Name = "Fred";  // should be unique
                    Type = DFF;
                    Position = { X = 5.0; Y = 5.0 };
                    Orientation = { Rotation = Degree0; Flipped = false };
                }
            ];
            Wires = [
                {
                    Source = { Label = "yeet"; PortNumber = 0 };
                    Target = { Label = "Fred"; PortNumber = 0 };
                }
            ];
        }
        // ... more sheet definitions
    ]


    /// essentially a wrapper for `placeWire` that makes it easier to use
    /// in a `List.fold`
    let placeSimpleWire (model: SheetT.Model) (wire: SimpleWire) =
        placeWire wire.Source wire.Target model
        |> TestLib.getOkOrFail
        // ^ unwraps results: any Fail would be a sheet definition error

    /// the sheetmaker that creates the exemplar sheet
    let createSheet (definition: SheetDefinition) : SheetT.Model=
        let placeSymbols (compList: List<ExactComponent>) (model: SheetT.Model) = 
            (model, compList)
            ||> List.fold (  // destructure ExactComponent to access all details
                    fun model 
                        {Name = label; 
                         Orientation = {Rotation = rot; Flipped = flip};
                         Position = pos; Type = compType} -> 
                        placeSymbol label compType pos model
                        |> TestLib.getOkOrFail
                        //|> // TODO: apply exactation, making sure all provided exact values are utilised
                )
        
        let placeWires (wireList:List<SimpleWire>) (model: SheetT.Model) = 
            List.fold (placeSimpleWire) model wireList
        
        // TODO: see if you can change this sheet's name, too
        DiagramMainView.init().Sheet
        |> placeSymbols definition.Components
        |> placeWires definition.Wires


    module Distort =
        // rotate, flip, alter port order
        // but no need to alter position or scales of symbols
        // can merge with work of D1 for that


        /// locally overloads a CommonTypes lens, with a more useful lens 
        /// (in this context)
        let portOrder_ = SymbolT.portMaps_ >-> SymbolT.order_


        /// defines a distortion test-set to be applied to a sheet
        type DistorterTest = {
            // something like "Easy, Medium, Hard, etc."
            Name: string;
            // a function that applies a series of distortions to a sheet
            // they can be chained, since they're designed to be independent
            Distorter: SheetT.Model -> SheetT.Model;
        }  // hard-coded list defined at end of `Distort` module


        /// generates a list of {`number`} indices with values from 
        /// 0 (inclusive) to max (non-inclusive)\
        /// the indices are all unique and random
        let generateUniqueIndices (max: int) (number: int) =
            let max = max - 1  // to make it 0-indexed

            if number > max
            then failwithf "can't generate more unique indices than max"
            // ^ this is a user error and should be called out

            let randList = toList (randomInt 0 1 max)
            
            if number = max
            then randList  // all possible indices in range, shuffled
            else List.take number randList
            // ^ take the first `number` indices from the shuffled list


        /// gets `number` Symbols from `model`
        let getRandomSymbols (number: int) 
                             (symbolMap: Map<ComponentId, SymbolT.Symbol>) =
            let randomIdxs = 
                generateUniqueIndices (Map.count symbolMap) number

            symbolMap
            |> Map.toList
            |> fun symList -> List.map (fun idx -> symList[idx]) randomIdxs

        
        // TODO: add a check somewhere, to ensure that `number` < comp_count
        // could clip/cap at comp_count? since multiple idempotent distortions
        // (all my distortions) equate to just one distortion, anyways

        /// applies the provided distortion to {`number`} random symbols 
        /// in the provided sheet,\
        /// where `number` < number of components in sheet
        let setRandomSymbols (model: SheetT.Model) (number: int) 
                             (distorter: SymbolT.Symbol -> SymbolT.Symbol)=
            let symbolMap = Optic.get SheetT.symbols_ model
            
            symbolMap
            |> getRandomSymbols number
            |> List.map (fun (compId, symbol) -> (compId, distorter symbol))
            |> List.fold (
                fun symMap (compId, symbol) -> 
                    Map.add compId symbol symMap
            ) symbolMap
            |> fun symList -> Optic.set SheetT.symbols_ symList model
        

        /// picks a random, existing edge, to target for port order distortions
        let getRandomEdge (portOrder: Map<Edge, string list>) = 
            portOrder.Keys
                |> Seq.toList
                |> fun edgeList ->  // picks a random edge that has ports
                    random.Next() % edgeList.length
                    |> fun targetIndex -> edgeList[targetIndex]


        // TODO: check that rotation and flipping can be safely applied
        // directly to symbols

        /// rotates a symbol to a random orientation\
        /// (helper for `rotateAndFlipSymbols`)
        let rotateSymbol (symbol: SymbolT.Symbol) =
            let rotation =
                match random.Next() % 4 with
                | 0 -> Degree0
                | 1 -> Degree90
                | 2 -> Degree180
                | 3 -> Degree270
                | _ -> Degree0  // this shouldn't happen
            SymbolResizeHelpers.rotateSymbol rotation symbol

        /// flips a symbol to a random orientation\
        /// (helper for `rotateAndFlipSymbols`)
        let flipSymbol (symbol: SymbolT.Symbol) =
            let orientation =  // TODO: should I expand to % 3, to include none?
                match random.Next() with
                | IsEven -> SymbolT.FlipHorizontal
                | IsOdd -> SymbolT.FlipVertical
            SymbolResizeHelpers.flipSymbol orientation symbol

        /// combines the rotate and flip functions
        /// to rotate and flip `number` symbols
        let rotateAndFlipSymbols (model: SheetT.Model) (number: int) =
            /// applies a random rotation and flip to the symbol
            let rotateFlipper (symbol: SymbolT.Symbol) =
                symbol
                |> rotateSymbol
                |> flipSymbol
            
            setRandomSymbols model number rotateFlipper
        

        /// a gatekeeping function that just reverses the port order
        /// if the symbol is a MUX, because MUXs are special...\
        /// and applies the desired distorter, otherwise\
        /// (helper for `switchPortOrders` and `portMesserUpper`)
        let portDistort (portDistorter: SymbolT.Symbol -> SymbolT.Symbol) 
                        (symbol: SymbolT.Symbol) =
            match symbol.ReversedInputPorts with
            | Some reversed ->  // is a mux-type symbol
                Optic.set reversedInputPorts_ (Some (not reversed)) symbol
            | None -> portDistorter symbol


        /// randomly picks two ports on a symbol and switches their order\
        /// obviously switches if the symbol has only two ports\
        /// (helper for `switchPortOrders`)
        let switchPortOrder (symbol: SymbolT.Symbol) =
            let portOrder = Optic.get portOrder_ symbol
            
            let targetEdge = getRandomEdge portOrder

            portOrder
            |> Map.find targetEdge  // key obtained above, so it's valid
            |> fun portList ->
                let portIndices = generateUniqueIndices portList.Length 2
                
                // switches the two ports
                List.mapi (fun idx portId ->  // TODO: still work with 1 port?
                    match idx with
                    | i when i = portIndices[0] -> portList[portIndices[1]]
                    | i when i = portIndices[1] -> portList[portIndices[0]]
                    | _ -> portId
                ) portList 
            |> fun newPortList ->  // update target edge's port list in symbol
                (Map.add targetEdge newPortList portOrder, symbol)
                ||> Optic.set portOrder_ 
                // TODO: not sure if this `||>` makes it more readable or not
            
        /// randomly picks `number` symbols and switches their ports' orders
        let switchPortOrders (model: SheetT.Model) (number: int) =
            // TODO: this is kinda ugly, though
            // reformulate this gatekeeping behaviour?
            portDistort switchPortOrder
            |> setRandomSymbols model number


        /// effective on many-port symbols, where a list can be generated,
        /// randomised, and then applied\
        /// to yield a fully randomised port edge\
        /// (helper for `portMesserUpper`)
        let randomisePortOrder (symbol: SymbolT.Symbol) =
            let portOrder = Optic.get portOrder_ symbol
            
            let targetEdge = getRandomEdge portOrder

            portOrder
            |> Map.find targetEdge
            |> fun portList ->
                generateUniqueIndices portList.Length portList.Length
                |> List.map (fun idx -> portList[idx])
            |> fun newPortList ->  // update target edge's port list in symbol
                (Map.add targetEdge newPortList portOrder, symbol)
                ||> Optic.set portOrder_ 
            // TODO: check implementation
        
        /// randomly picks `number` symbols and randomises their ports' orders
        let portMesserUpper (model: SheetT.Model) (number: int) =
            portDistort randomisePortOrder
            |> setRandomSymbols model number 


        /// a list of tests to be applied by Executioner
        let distortionLevels: List<DistorterTest> = [
            {
                Name = "Easy";
                Distorter = 
                    let fred = 5
                    fun john -> john
                    // TODO: create distortion chain
            };
            {
                Name = "Medium";
                Distorter = 
                    let fred = 5
                    fun john -> john
                    // TODO: create distortion chain
            };
            {
                Name = "Hard";
                Distorter = 
                    let fred = 5
                    fun john -> john
                    // TODO: create distortion chain
            }
        ]


    /// a module to evaluate the performance of the beautification function\
    /// for the samples that fail the initial tests, 
    /// we can still evaluate how close they came to their target\
    /// (majority of code, courtesy of Roshan)
    module Evaluate =
        (* Criteria:
            Primarily checking for Wire Crossings
            
            Can also check for:
            - Wire Usage
                - total wire usage should be similar to the examplar sheet,
                  with a little tolerance, of course
                - a symbol in a suboptimal orientation will use more wire
                  than a symbol in an optimal orientation
            - Symbol Orientation
                - some symbol orientations are better than others
                - for most symbols, the default orientation is the best
                - logic gates are more forgiven in other orientations
                - for most symbol the 180 degree rotation is the worst
            // dropped spacing checks, because it's too complex for this
            - Wire-Symbol spacing
                - the distance between a wire and any given symbol
                  affects how cluttered that area of the sheet looks
                - the wires from a symbol in a suboptimal orientation
                  may be routed too close to another symbol
                - of course, this must be balanced with Wire Usage
                  to yield a sweet spot for symbol orientation and port order
        *)

        // TODO: implement this, into pipeline
        /// goes through all symbols, marking their orientations.\
        /// default orientation is best, logic gates have fewer marks deducted
        /// for rotation and flipping than other components
        let orientationMarker (sheet: SheetT.Model) =
            failwithf "Not Implemented"  // TODO: implement


        /// Sheet evaluation metric hyperparameters
        type SheetEvalParamT = {
            WireCrossingsNorm: float;
            VisibleLengthNorm: float;
            RightAnglesNorm: float;
            RetracedSegsNorm: float;
            SegSymIntersectNorm: float;
        }

        let SheetEvalParam = {
            WireCrossingsNorm = 250_000.0;
            VisibleLengthNorm = 1.0;
            RightAnglesNorm = 15_000.0;
            RetracedSegsNorm = 1_000_000.0;
            SegSymIntersectNorm = 1_500_000.0;
        }

        /// Returns the score of a sheet.
        let evaluateSheet
                (sheet: SheetT.Model)
                : float =

            let wireCrossingsSq = float (SheetBeautifyHelpers2.numRightAngleSegCrossings sheet) ** 2.0
            let visibleLengthSq = float (SheetBeautifyHelpers2.visibleWireLength sheet) ** 2.0
            let rightAnglesSq = float (SheetBeautifyHelpers2.numWireRightAngles sheet) ** 2.0
            let retracedSegsSq = float (SheetBeautifyD2.numRetracedSegs sheet) ** 2.0
            let segSymIntersectSq = float (SheetBeautifyD2.numSymsIntersectedBySeg sheet) ** 2.0

            wireCrossingsSq * SheetEvalParam.WireCrossingsNorm +
            visibleLengthSq * SheetEvalParam.VisibleLengthNorm +
            rightAnglesSq * SheetEvalParam.RightAnglesNorm +
            retracedSegsSq * SheetEvalParam.RetracedSegsNorm +
            segSymIntersectSq * SheetEvalParam.SegSymIntersectNorm

        /// Returns a pair of scores for a sheet before and after beautification.
        let evaluateBeforeAndAfter
                (sheetBefore: SheetT.Model)
                (sheetAfter: SheetT.Model)
                : (float * float) =
            evaluateSheet sheetBefore, evaluateSheet sheetAfter


        /// Print all results.
        let printAllResults
                (result: (float * float) list)
                : unit =
            result
            |> List.map (fun (before, after) ->
                let diff = (before - after)/before * 100.
                printf "Score before = %.2e, Score after = %.2e, Improvement = %.1f%%" before after diff
            )
            |> fun _ -> () // Return unit
            
        /// Print aggregated results.
        let printAggregatedResults
                (results: (float * float) list)
                : unit =
            let scoreDiffRel =
                results
                |> List.map (fun (before, after) -> (before - after)/before * 100.)
            printf "Relative metrics:"
            printf "Min score improvement = %.1f%%" <| List.min scoreDiffRel
            printf "Max score improvement = %.1f%%" <| List.max scoreDiffRel
            printf "Average score improvement = %.1f%%" <| List.average scoreDiffRel


        /// The main function that evaluates the performance
        /// of the beautification function,\
        /// by aggregating the results of lots of different metrics
        let Jury (exemplars: List<SheetT.Model>) 
                 (sheetsUnderTest: List<SheetT.Model> list) =
            // match the 2D `sheetsUnderTest` lists to its exemplar counterpart
            (exemplars, sheetsUnderTest)
            ||> List.map2 (
                fun exemplar levelsUnderTest ->
                    List.collect (
                        fun levelUnderTest -> 
                            [evaluateBeforeAndAfter exemplar levelUnderTest]
                    ) levelsUnderTest
            )
            |> List.concat
            |> fun results -> 
                printAllResults results;
                printAggregatedResults results


        // NOT IMPLEMENTING JUDGE, for lack of time :(
        
        /// disassembles a sheet into a sheet definition
        let disassembleSheet (sheet: SheetT.Model) : SheetDefinition =
            failwithf "Not Implemented"  // TODO: implement
        
        /// the sheetchecker that compares the beautified sheet 
        /// against the exemplar
        let practicallyEquivalent (exemplar: SheetDefinition) 
                                  (sheetUnderTest: SheetT.Model) =
            (*
            1. Use disassembleSheet to strip the sheetUnderTest down
            2. Compare the two SD's, based on change-able parameters
                - Port Orderings <- Wire port numbers
                - Symbol Orientation
            *)
            failwithf "Not Implemented"  // TODO: Implement

        /// performs a holistic judgement of how close the produced sheet is 
        /// to the original exemplar sheet's definition
        let Judge (exemplars: List<SheetDefinition>) 
                  (sheetsUnderTest: List<SheetT.Model>) =
            List.zip exemplars sheetsUnderTest
            |> List.map (
                fun (exemplar, sheetUnderTest) -> 
                    practicallyEquivalent exemplar sheetUnderTest
            )

    
    /// executes the building and testing pipeline for the whole D2 block
    module Execute =
        open SheetBeautifyD2
        open Distort
        open Evaluate

        
        // TODO: `showSheetInIssieSchematic` as a blocking, pipelined operation
        // this should allow us to see the testifying process,\
        // since beautification is a visual and subjective art
        // ^ so, the visual aspect is also important for testing
        /// the top-level, pipeline-execution function:\
        /// creates sheets, distorts them, beautifies them, and then evaluates
        let Executioner (dispatch: Dispatch<Msg>) 
                        (distortionTests: List<DistorterTest>) 
                        (beautifyFunction) =
            /// local name, to allow for easier swapping
            let sheetDefs = sheetDefinitions
            
            // these exemplar sheets don't have to be fixed
            let exemplarSheets = List.map createSheet sheetDefs

            exemplarSheets
            |> List.map (fun sheet ->  // for each defined sheet
                List.map (
                    fun distortionTest -> // for each distortion level
                        // printf $"Applying {distortionTests.Name} test"?
                        // TODO: oops, sheet naming not yet implemented
                        distortionTest.Distorter sheet
                ) distortionTests
            )
            |> List.map (List.map (beautifyFunction))
            |> Jury exemplarSheets  // all results will be printed
            // ^ TODO: when Judge implemented, have Judge be called first and
            // get print it's results, returning the SUT's, unharmed, for Jury


        /// redefinition to replace one in HLPTick3 module,
        /// since that's the one called by `Renderer`\
        /// Ideally, the other tests would be added here, 
        /// so that they can be run via the hotkeys
        let testMenuFunc (testIndex: int) (dispatch: Dispatch<Msg>) (model: Model) =
            match testIndex with
            | 2 -> 
                printf "Running D2 Test"
                (distortionLevels, sheetOrderFlip)
            | _ -> failwithf "for other tests"
            ||> Executioner dispatch
            
    // TODO: push asap, git commit -m "plan execution pipelining"