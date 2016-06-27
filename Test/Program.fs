namespace Test


open System
open System.Diagnostics
open System.IO
open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.LinearAlgebra.Double
open Muster.DataStructuresAndAlgorithms


module Program =


    let sW = Stopwatch()


    let rnd = Random()


    let tf1 kDT dim maxVal b distFunc diffFunc vecsArr : unit =
        let testVec1 = KDTree.genRandTestInt16Vec dim maxVal b
        let testVec2 = KDTree.genRandTestInt16Vec dim maxVal b
        sW.Start()
        let matches =
            KDTree.runNNQuery
                kDT
                distFunc
                diffFunc
                (KDTree.Query.Just testVec1)
                // (KDTree.Query.And [KDTree.Query.Just testVec1; KDTree.Query.Just testVec2])
                // (KDTree.Query.Or [KDTree.Query.Just testVec1; KDTree.Query.Just testVec2])
                // (KDTree.Query.Diff [KDTree.Query.Just testVec1; KDTree.Query.Just testVec2])
        sW.Stop()
        let t1 = sW.ElapsedMilliseconds
        printfn "KDTree num of matches = %A; time taken = %A ms" (Set.count matches) t1
        sW.Reset()
        sW.Start()
        let matches2 = KDTree.getBestMatchesByLinearScanning vecsArr testVec1 distFunc diffFunc
        sW.Stop()
        let t2 = sW.ElapsedMilliseconds
        printfn "Linear scan num of matches = %A; time taken = %A ms" matches2.Length t2
        printfn "Speed-up ratio = %A" ((float t2) / (float t1))
        sW.Reset()


    let tf2 kDT dim maxVal b vecsArr : unit =
        let testVec1 = KDTree.genRandTestInt16Vec dim maxVal b
        let testVec2 =
            let t = KDTree.genRandTestInt16Vec dim maxVal b
            (Array.zip testVec1 t)
            |> Array.map (fun s ->
                match s with
                | Some u, Some v -> Some (u + v)
                | None, Some u -> Some u
                | _ -> None)
        sW.Start()
        let matches = KDTree.runRangeQuery kDT (KDTree.Query.ClosedBounds(testVec1, testVec2))
        sW.Stop()
        let t1 = sW.ElapsedMilliseconds
        printfn "KDTree num in range = %A; time taken = %A ms" (Set.count matches) t1
        sW.Reset()
        sW.Start()
        let matches2 = KDTree.getClosedRangeMatchesByLinearScanning vecsArr testVec1 testVec2
        sW.Stop()
        let t2 = sW.ElapsedMilliseconds
        printfn "Linear scan num in range = %A; time taken = %A ms" matches2.Length t2
        printfn "Speed-up ratio = %A" ((float t2) / (float t1))
        sW.Reset()


    let tf3 kDT dim maxVal b distFunc diffFunc : unit =
        let vec = (KDTree.genRandInt16VecsArr dim 1 maxVal).[0]
        let kDT2 = KDTree.addVec kDT vec
        let testVec1 = KDTree.genRandTestInt16Vec dim maxVal b
        sW.Start()
        let matches =
            KDTree.runNNQuery
                kDT2
                distFunc
                diffFunc
                (KDTree.Query.Just testVec1)
        sW.Stop()
        printfn "KDTree num of matches = %A; time taken = %A ms" (Set.count matches) sW.ElapsedMilliseconds
        sW.Reset()


    let tf4 kDT dim maxVal b distFunc diffFunc : unit =
        let n = 100
        sW.Start()
        [1 .. n]
        |> List.map (fun _ ->
            let testVec1 = KDTree.genRandTestInt16Vec dim maxVal b
            sW.Start()
            let matches =
                KDTree.runNNQuery
                    kDT
                    distFunc
                    diffFunc
                    (KDTree.Query.Just testVec1)
            Set.count matches)
        |> List.sum
        |> printfn "Total number of matches found in %A queries: %A" n
        sW.Stop()
        printfn "Total time for %A queries: %A ms" n sW.ElapsedMilliseconds
        sW.Reset()


    let tf5 kDT dim maxVal b : unit =
        let n = 1000
        sW.Start()
        [1 .. n]
        |> List.map (fun _ ->
            let testVec1 = KDTree.genRandTestInt16Vec dim maxVal b
            let testVec2 =
                let t = KDTree.genRandTestInt16Vec dim maxVal b
                (Array.zip testVec1 t)
                |> Array.map (fun s ->
                    match s with
                    | Some u, Some v -> Some (u + v)
                    | None, Some u -> Some u
                    | _ -> None)
            let matches = KDTree.runRangeQuery kDT (KDTree.Query.ClosedBounds(testVec1, testVec2))
            Set.count matches)
        |> List.sum
        |> printfn "Total number of matches found in %A queries: %A" n
        sW.Stop()
        printfn "Total time for %A queries: %A ms" n sW.ElapsedMilliseconds
        sW.Reset()


    let tf6 kDT : unit =
        printfn "Start removing the vector"
        sW.Start()
        // let kDT2 = (KDTree.removeVec kDT kDT.Vec).Value
        let kDT2 = (KDTree.removeVec kDT (kDT.RightChild.Value.RightChild.Value.LeftChild.Value.Vec)).Value
        sW.Stop()
        printfn "Finished removing the vector; time taken = %A ms" sW.ElapsedMilliseconds
        sW.Reset()


    let testSpecificANN (fnName : string) (inputDim : int) (outputDim : int) : unit =
        let layerConfig = [inputDim; 5; 10; 4; outputDim]
        let learningParam = 0.05
        let aNN =
            ANN.init
                layerConfig
                ANN.defaultInputActivation
                ANN.defaultDInputActivation
                ANN.defaultOutputActivation
                ANN.defaultDOutputActivation
                learningParam
        let trainingSet = ANN.importTrainingSetFromFile
                            (@"..\..\..\Muster\SampleData\ANN\" + fnName + ".txt")
                            inputDim
                            outputDim
        let numOfEpochs = 10000
        let trainingMode = ANN.TrainingMode.Single
        printfn "Start training aNN"
        sW.Start()
        let tANN = ANN.train aNN trainingSet numOfEpochs trainingMode
        sW.Stop()
        printfn "Training complete; time taken = %A ms" sW.ElapsedMilliseconds
        let rec loop () : unit =
            let inpVec, outVec =
                let n = 1 + (rnd.Next() % ((Matrix.rowCount trainingSet.InputVecsMat) - 1))
                (trainingSet.InputVecsMat |> Matrix.toRowSeq |> Seq.skip n |> Seq.head),
                (trainingSet.OutputVecsMat |> Matrix.toRowSeq |> Seq.skip n |> Seq.head)
            printfn "inpVec = %A" inpVec
            let pred = ANN.getPrediction tANN inpVec
            printfn "pred = %A" pred
            printfn "outVec = %A" outVec
            printf "Check another training vector ([Y]/N): "
            match stdin.ReadLine().ToLower() with
            | "n" -> ()
            | _ -> loop()
        loop()
        sW.Reset()


    let tf7 () : unit =
        let fnName = "squareOfSum"
        printfn "Function name: %A" fnName
        testSpecificANN fnName 3 1


    let tf8 () : unit =
        let fnName = "sineOfSum"
        printfn "Function name: %A" fnName
        testSpecificANN fnName 3 1


    let tf9 () : unit =
        let fnName = "and3Fn"
        printfn "Function name: %A" fnName
        testSpecificANN fnName 3 1


    let tf10 () : unit =
        let fnName = "or3Fn"
        printfn "Function name: %A" fnName
        testSpecificANN fnName 3 1


    let tf11 () : unit =
        let fnName = "xor2Fn"
        printfn "Function name: %A" fnName
        testSpecificANN fnName 2 1


    let tf12 kDT dim maxVal b distFunc diffFunc vecsArr : unit =
        let testVec1 = KDTree.genRandTestDblVec dim maxVal b
        sW.Start()
        let matches =
            KDTree.runNNQuery
                kDT
                distFunc
                diffFunc
                (KDTree.Query.Just testVec1)
        sW.Stop()
        let t1 = sW.ElapsedMilliseconds
        printfn "KDTree num of matches = %A; time taken = %A ms" (Set.count matches) t1
        sW.Reset()
        sW.Start()
        let matches2 = KDTree.getBestMatchesByLinearScanning vecsArr testVec1 distFunc diffFunc
        sW.Stop()
        let t2 = sW.ElapsedMilliseconds
        printfn "Linear scan num of matches = %A; time taken = %A ms" matches2.Length t2
        printfn "Speed-up ratio = %A" ((float t2) / (float t1))
        sW.Reset()


    let tf13 kDT dim maxVal b vecsArr : unit =
        let testVec1 = KDTree.genRandTestDblVec dim maxVal b
        let testVec2 =
            let t = KDTree.genRandTestDblVec dim maxVal b
            (Array.zip testVec1 t)
            |> Array.map (fun s ->
                match s with
                | Some u, Some v -> Some (u + v)
                | None, Some u -> Some u
                | _ -> None)
        sW.Start()
        let matches = KDTree.runRangeQuery kDT (KDTree.Query.ClosedBounds(testVec1, testVec2))
        sW.Stop()
        let t1 = sW.ElapsedMilliseconds
        printfn "KDTree num in range = %A; time taken = %A ms" (Set.count matches) t1
        sW.Reset()
        sW.Start()
        let matches2 = KDTree.getClosedRangeMatchesByLinearScanning vecsArr testVec1 testVec2
        sW.Stop()
        let t2 = sW.ElapsedMilliseconds
        printfn "Linear scan num in range = %A; time taken = %A ms" matches2.Length t2
        printfn "Speed-up ratio = %A" ((float t2) / (float t1))
        sW.Reset()


    let tf14 kDT dim maxVal b distFunc diffFunc : unit =
        let n = 100
        sW.Start()
        [1 .. n]
        |> List.map (fun _ ->
            async {
                let testVec1 = KDTree.genRandTestInt16Vec dim maxVal b
                let matches =
                    KDTree.runNNQuery
                        kDT
                        distFunc
                        diffFunc
                        (KDTree.Query.Just testVec1)
                return Set.count matches
            })
        |> Async.Parallel
        |> Async.RunSynchronously
        |> Array.sum
        |> printfn "Total number of matches found in %A queries: %A" n
        sW.Stop()
        printfn "Total time for %A queries: %A ms" n sW.ElapsedMilliseconds
        sW.Reset()


    let tf15 kDT dim maxVal b : unit =
        let n = 1000
        sW.Start()
        [1 .. n]
        |> List.map (fun _ ->
            async {
                let testVec1 = KDTree.genRandTestInt16Vec dim maxVal b
                let testVec2 =
                    let t = KDTree.genRandTestInt16Vec dim maxVal b
                    (Array.zip testVec1 t)
                    |> Array.map (fun s ->
                        match s with
                        | Some u, Some v -> Some (u + v)
                        | None, Some u -> Some u
                        | _ -> None)
                let matches = KDTree.runRangeQuery kDT (KDTree.Query.ClosedBounds(testVec1, testVec2))
                return Set.count matches
            })
        |> Async.Parallel
        |> Async.RunSynchronously
        |> Array.sum
        |> printfn "Total number of matches found in %A queries: %A" n
        sW.Stop()
        printfn "Total time for %A queries: %A ms" n sW.ElapsedMilliseconds
        sW.Reset()


    let tf16 () : unit =
        let tbl = DecisionTree.parseDataTableFromFile @"..\..\..\Muster\SampleData\DecisionTree\SampleID3Data.txt"
        let impurityFn = DecisionTree.entropy
        let c45Tree = DecisionTree.buildC45 tbl impurityFn None
        let inputMap =
            seq [
                (
                DecisionTree.DataType.Cat(DecisionTree.CatType.Str "GENDER"),
                DecisionTree.DataType.Cat(DecisionTree.CatType.Str "female"));
//                (
//                DecisionTree.DataType.Cat(DecisionTree.CatType.Str "TRAVEL COST"),
//                DecisionTree.DataType.Cat(DecisionTree.CatType.Str "cheap"));
                (
                DecisionTree.DataType.Cat(DecisionTree.CatType.Str "CAR OWNERSHIP"),
                DecisionTree.DataType.Cat(DecisionTree.CatType.Str "0"))
            ]
            |> Map.ofSeq
        printfn "inputMap = %A" inputMap
        let prediction = DecisionTree.getPrediction c45Tree inputMap
        printfn "prediction = %A" prediction


    let tf17 () : unit =
        let tbl = DecisionTree.parseDataTableFromFile @"..\..\..\Muster\SampleData\DecisionTree\SampleC45Data.txt"
        let impurityFn = DecisionTree.entropy
        let splitStopCriterion = DecisionTree.defSplitStopCriterion
        let c45Tree = DecisionTree.buildC45 tbl impurityFn (Some splitStopCriterion)
        let inputMap =
            seq [(DecisionTree.DataType.Cat(DecisionTree.CatType.Str "HUMIDITY"), DecisionTree.DataType.Cont 65.0)]
            |> Map.ofSeq
        printfn "inputMap = %A" inputMap
        let prediction = DecisionTree.getPrediction c45Tree inputMap
        printfn "prediction = %A" prediction


    let tf18 () : unit =
        let tbl = RandomForest.parseDataTableFromFile @"..\..\..\Muster\SampleData\DecisionTree\SampleC45Data.txt"
        printfn "Length of tbl = %A" (List.length tbl)
        let numOfTrees = 4
        let forest = RandomForest.buildDefault tbl numOfTrees
        printfn "forest = %A" forest
        let inputMap =
            seq [(DecisionTree.DataType.Cat(DecisionTree.CatType.Str "HUMIDITY"), DecisionTree.DataType.Cont 65.0)]
            |> Map.ofSeq
        printfn "inputMap = %A" inputMap
        let prediction = RandomForest.getPrediction forest inputMap
        printfn "prediction = %A" prediction


    let tf19 () : unit =
        let tbl = DecisionTree.parseDataTableFromFile @"..\..\..\Muster\SampleData\DecisionTree\SampleC45Data_2.txt"
        let impurityFn = DecisionTree.stdDevError
        let splitStopCriterion = DecisionTree.defSplitStopCriterion
        let c45Tree =
            DecisionTree.buildC45
                tbl
                impurityFn
                (Some(DecisionTree.splitStopCriterionGen (float(List.length tbl)) 0.05))
        let inputMap =
            seq [
                (
                DecisionTree.DataType.Cat(DecisionTree.CatType.Str "HUMIDITY"),
                DecisionTree.DataType.Cat(DecisionTree.CatType.Str "high"));
                (
                DecisionTree.DataType.Cat(DecisionTree.CatType.Str "TEMP"),
                DecisionTree.DataType.Cat(DecisionTree.CatType.Str "cool")
                )
            ]
            |> Map.ofSeq
        printfn "inputMap = %A" inputMap
        let prediction = DecisionTree.getPrediction c45Tree inputMap
        printfn "prediction = %A" prediction


    let testInt16KDTree () : unit =
        let dim = 8
        let numOfVecs = 1024000 * 4
        let (maxVal : int16) = 128s
        let b = 7
        let vecsArr = KDTree.genRandInt16VecsArr dim numOfVecs maxVal
        let diffFunc = KDTree.int16DiffFunc
        let distFunc = KDTree.euclDistFunc
        let kDT =
            sW.Start()
            printfn "Start building the k-d tree"
            let t = (KDTree.build vecsArr).Value
            sW.Stop()
            printfn "Finished; time taken to build the k-d tree: %A ms" sW.ElapsedMilliseconds
            sW.Reset()
            t
        let rec loop () : unit =
            printf "Function to run: "
            match stdin.ReadLine().ToLower() with
            | "tf1" -> tf1 kDT dim maxVal b distFunc diffFunc vecsArr
            | "tf2" -> tf2 kDT dim maxVal b vecsArr
            | "tf3" -> tf3 kDT dim maxVal b distFunc diffFunc
            | "tf4" -> tf4 kDT dim maxVal b distFunc diffFunc
            | "tf5" -> tf5 kDT dim maxVal b
            | "tf6" -> tf6 kDT
            | "tf14" -> tf14 kDT dim maxVal b distFunc diffFunc
            | "tf15" -> tf15 kDT dim maxVal b
            | _ -> ()
            printf "Continue testing Int16KDTree? [Y/N]: "
            match stdin.ReadLine().ToLower() with "n" -> () | _ -> loop ()
        loop ()


    let testDblKDTree () : unit =
        let dim = 8
        let numOfVecs = 1024000 * 2
        let (maxVal : double) = 16.0
        let b = 10
        let vecsArr = KDTree.genRandDblVecsArr dim numOfVecs maxVal
        let diffFunc = KDTree.dblDiffFunc 2
        let distFunc = KDTree.euclDistFunc
        let kDT =
            sW.Start()
            printfn "Start building the k-d tree"
            let t = (KDTree.build vecsArr).Value
            sW.Stop()
            printfn "Finished; time taken to build the k-d tree: %A ms" sW.ElapsedMilliseconds
            sW.Reset()
            t
        let rec loop () : unit =
            printf "Function to run: "
            match stdin.ReadLine().ToLower() with
            | "tf12" -> tf12 kDT dim maxVal b distFunc diffFunc vecsArr
            | "tf13" -> tf13 kDT dim maxVal b vecsArr
            | _ -> ()
            printf "Continue testing DblKDTree? [Y/N]: "
            match stdin.ReadLine().ToLower() with "n" -> () | _ -> loop ()
        loop ()


    let testANN () : unit =
        let rec loop () : unit =
            printf "Function to run: "
            match stdin.ReadLine().ToLower() with
            | "tf7" -> tf7()
            | "tf8" -> tf8()
            | "tf9" -> tf9()
            | "tf10" -> tf10() 
            | "tf11" -> tf11()
            | _ -> ()
            printf "Continue testing ANN? [Y/N]: "
            match stdin.ReadLine().ToLower() with "n" -> () | _ -> loop ()
        loop ()


    let testDecisionTree () : unit =
        let rec loop () : unit =
            printf "Function to run: "
            match stdin.ReadLine().ToLower() with
            | "tf16" -> tf16()
            | "tf17" -> tf17()
            | "tf19" -> tf19()
            | _ -> ()
            printf "Continue testing DecisionTree? [Y/N]: "
            match stdin.ReadLine().ToLower() with "n" -> () | _ -> loop()
        loop ()


    let testRandomForest () : unit =
        let rec loop () : unit =
            printf "Function to run: "
            match stdin.ReadLine().ToLower() with
            | "tf18" -> tf18()
            | _ -> ()
            printf "Continue testing RandomForest? [Y/N]: "
            match stdin.ReadLine().ToLower() with "n" -> () | _ -> loop()
        loop()


    [<EntryPoint>]
    let main argv =
        printfn "Entering main()"
        let rec loop () : unit =
            printf "Module to test: "
            match "test" + stdin.ReadLine() with
            | "testInt16KDTree" -> testInt16KDTree()
            | "testDblKDTree" -> testDblKDTree()
            | "testANN" -> testANN()
            | "testDecisionTree" -> testDecisionTree()
            | "testRandomForest" -> testRandomForest()
            | _ -> ()
            printf "Test another module? [Y/N]: "
            match stdin.ReadLine().ToLower() with "n" -> () | _ -> loop ()
        loop ()
        printfn "Exiting main()"
        0

