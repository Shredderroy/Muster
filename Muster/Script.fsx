#load "../packages/MathNet.Numerics.FSharp.3.11.0/MathNet.Numerics.fsx"
#load "Utils.fs"
#load "ListExtensions.fs"
#load "StringExtensions.fs"
#load "KDTree.fs"
#load "ANN.fs"
#load "PCA.fs"
#load "DecisionTree.fs"

open System
open System.Diagnostics
open System.IO
open System.Text.RegularExpressions
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.LinearAlgebra.Double
open Muster.Utils
open Muster.Extensions
open Muster.DataStructuresAndAlgorithms
open Muster.DataStructuresAndAlgorithms.DecisionTree


let rnd = Random()


//let createSquareOfSumDataFile (outputFileLoc : string) : unit =
//    let numOfPairs, inputDim, maxVal = 500, 3, 10
//    let genFn = fun _ -> let x = (rnd.NextDouble() * (double(rnd.Next() % maxVal))) in if rnd.Next() % 2 = 0 then x else -x
//    let squareOfSum = fun (s : array<double>) -> let t = Array.sum s in [|t * t|]
//    [|1 .. numOfPairs|]
//    |> Array.map (fun _ -> let s = ([|1 .. inputDim|] |> Array.map genFn) in [| s; squareOfSum s|])
//    |> Array.map (fun s -> s |> Array.map (fun t -> t |> Array.map (string) |> (fun u -> String.Join(", ", u))))
//    |> Array.map (fun s -> String.Join(", ", s))
//    |> (fun s -> File.WriteAllLines(outputFileLoc, s))
//    printfn "Done"
//
//
//let createSineOfSumDataFile (outputFileLoc : string) : unit =
//    let numOfPairs, inputDim, maxVal = 1000, 3, 10
//    let genFn = fun _ -> let x = (rnd.NextDouble() * (double(rnd.Next() % maxVal))) in if rnd.Next() % 2 = 0 then x else -x
//    let sineOfSum = fun (s : array<double>) -> let t = Array.sum s in [|sin t|]
//    [|1 .. numOfPairs|]
//    |> Array.map (fun _ -> let s = ([|1 .. inputDim|] |> Array.map genFn) in [| s; sineOfSum s|])
//    |> Array.map (fun s -> s |> Array.map (fun t -> t |> Array.map (string) |> (fun u -> String.Join(", ", u))))
//    |> Array.map (fun s -> String.Join(", ", s))
//    |> (fun s -> File.WriteAllLines(outputFileLoc, s))
//    printfn "Done"
//
//
//let createAND3 (outputFileLoc : string) : unit =
//    let numOfPairs, inputDim = 250, 3
//    let genFn = fun _ -> double (rnd.Next() % 2)
//    let and3Fn = fun (s : array<double>) -> let t = Array.sum s in [|(if t = 3.0 then 1.0 else 0.0)|]
//    [|1 .. numOfPairs|]
//    |> Array.map (fun _ -> let s = ([|1 .. inputDim|] |> Array.map genFn) in [| s; and3Fn s|])
//    |> Array.map (fun s -> s |> Array.map (fun t -> t |> Array.map (string) |> (fun u -> String.Join(", ", u))))
//    |> Array.map (fun s -> String.Join(", ", s))
//    |> (fun s -> File.WriteAllLines(outputFileLoc, s))
//    printfn "Done"
//
//
//let createOR3 (outputFileLoc : string) : unit =
//    let numOfPairs, inputDim = 250, 3
//    let genFn = fun _ -> double (rnd.Next() % 2)
//    let or3Fn = fun (s : array<double>) -> let t = Array.sum s in [|(if t = 0.0 then 0.0 else 1.0)|]
//    [|1 .. numOfPairs|]
//    |> Array.map (fun _ -> let s = ([|1 .. inputDim|] |> Array.map genFn) in [| s; or3Fn s|])
//    |> Array.map (fun s -> s |> Array.map (fun t -> t |> Array.map (string) |> (fun u -> String.Join(", ", u))))
//    |> Array.map (fun s -> String.Join(", ", s))
//    |> (fun s -> File.WriteAllLines(outputFileLoc, s))
//    printfn "Done"
//
//
//let createXOR2 (outputFileLoc : string) : unit =
//    let numOfPairs, inputDim = 250, 2
//    let genFn = fun _ -> double (rnd.Next() % 2)
//    let xor2Fn = fun (s : array<double>) -> let t = Array.sum s in [|(if t = 1.0 then 1.0 else 0.0)|]
//    [|1 .. numOfPairs|]
//    |> Array.map (fun _ -> let s = ([|1 .. inputDim|] |> Array.map genFn) in [| s; xor2Fn s|])
//    |> Array.map (fun s -> s |> Array.map (fun t -> t |> Array.map (string) |> (fun u -> String.Join(", ", u))))
//    |> Array.map (fun s -> String.Join(", ", s))
//    |> (fun s -> File.WriteAllLines(outputFileLoc, s))
//    printfn "Done"
//
//
//let kDTreeTest () : unit =
//    let dim = 3
//    let numOfVecs = 8
//    let (maxVal : int16) = 8s
//    let b = 10
//    let vecsArr = KDTree.genRandInt16VecsArr dim numOfVecs maxVal
//    let diffFunc = KDTree.int16DiffFunc
//    let distFunc = KDTree.euclDistFunc
//    let sW = Stopwatch()
//    let kDT =
//        sW.Start()
//        printfn "Start building the k-d tree"
//        let t = (KDTree.build vecsArr).Value
//        sW.Stop()
//        printfn "Finished; time taken to build the k-d tree: %A ms" sW.ElapsedMilliseconds
//        sW.Reset()
//        t
//    printfn "%A" kDT
//    let kDT2 = KDTree.removeVec kDT (kDT.LeftChild.Value.Vec)
//    printfn "%A" kDT2
//
//
//let aNNTest (trainingSetFileLoc : string) : unit =
//    let trainingSet = ANN.importTrainingSetFromFile trainingSetFileLoc 3 1
//    let inputVecDim = trainingSet.InputVecsMat |> Matrix.columnCount
//    let outputVecsDim = trainingSet.OutputVecsMat |> Matrix.columnCount
//    let layerConfig = [inputVecDim; 5; 10; 4; outputVecsDim]
//    let learningParam = 0.05
//    let aNN =
//        ANN.init
//            layerConfig
//            ANN.defaultInputActivation
//            ANN.defaultDInputActivation
//            ANN.defaultOutputActivation
//            ANN.defaultDOutputActivation
//            learningParam
//    let numOfEpochs = 10000
//    let trainingMode = ANN.TrainingMode.Single
//    printfn "Start training aNN"
//    let sW = Stopwatch()
//    sW.Start()
//    let tANN = ANN.train aNN trainingSet numOfEpochs trainingMode
//    sW.Stop()
//    printfn "Training complete; time taken = %A ms" sW.ElapsedMilliseconds
//    sW.Reset()
//    // let rmsd = ANN.getRMSDForTrainingSet tANN trainingSet
//    // printfn "rmsd = %A" rmsd
//    let inpVec, outVec =
//        let n = 1 + (rnd.Next() % ((Matrix.rowCount trainingSet.InputVecsMat) - 1))
//        (trainingSet.InputVecsMat |> Matrix.toRowSeq |> Seq.skip n |> Seq.head),
//        (trainingSet.OutputVecsMat |> Matrix.toRowSeq |> Seq.skip n |> Seq.head)
//    let pred = ANN.getPrediction tANN inpVec
//    printfn "inpVec = %A" inpVec
//    printfn "pred = %A" pred
//    printfn "outVec = %A" outVec
//
//
//let kaggleCompetitionOttoGroup (trainingSetFileLoc : string) : unit =
//    let trainingSet =
//        (File.ReadAllLines(trainingSetFileLoc)).[1..]
//        |> Array.map (fun s -> let t = s.Split([|','|], StringSplitOptions.RemoveEmptyEntries).[1..] in (t.[..(t.Length - 2)], t.[t.Length - 1]))
//        |> Array.map (fun (s, t) -> (s, (t.Split [|'_'|]).[1]))
//        |> Array.map (fun (s, t) -> (s |> Array.map (double)), (double t))
//        |> Array.fold (fun (s, t) (u, v) -> ((List.ofArray u) :: s), ([v] :: t)) ([], [])
//        |> (fun (s, t) -> {ANN.TrainingSet.InputVecsMat = matrix (List.rev s); ANN.TrainingSet.OutputVecsMat = matrix (List.rev t)})
//    let inputVecDim = trainingSet.InputVecsMat.ColumnCount
//    let outputVecsDim = trainingSet.OutputVecsMat.ColumnCount
//    let layerConfig = [inputVecDim; 100; 75; 50; outputVecsDim]
//    let learningParam = 0.05
//    let aNN =
//        ANN.init
//            layerConfig
//            ANN.defaultInputActivation
//            ANN.defaultDInputActivation
//            ANN.defaultOutputActivation
//            ANN.defaultDOutputActivation
//            learningParam
//    let numOfEpochs = 2
//    let trainingMode = ANN.TrainingMode.Single
//    printfn "Start training aNN"
//    let sW = Stopwatch()
//    sW.Start()
//    let tANN = ANN.train aNN trainingSet numOfEpochs trainingMode
//    sW.Stop()
//    printfn "Training complete; time taken = %A ms" sW.ElapsedMilliseconds
//    sW.Reset()
//    // let rmsd = ANN.getRMSDForTrainingSet tANN trainingSet
//    // printfn "rmsd = %A" rmsd
//    let inpVec, outVec =
//        let n = 1 + (rnd.Next() % ((Matrix.rowCount trainingSet.InputVecsMat) - 1))
//        (trainingSet.InputVecsMat |> Matrix.toRowSeq |> Seq.skip n |> Seq.head),
//        (trainingSet.OutputVecsMat |> Matrix.toRowSeq |> Seq.skip n |> Seq.head)
//    let pred = ANN.getPrediction tANN inpVec
//    printfn "inpVec = %A" inpVec
//    printfn "pred = %A" pred
//    printfn "outVec = %A" outVec
//
//
//let pcaTest () : unit =
//    // Set the dimensions of the matrix
//    let numOfRows, numOfCols = 7, 3
//    // Generate a matrix with a dependent column (the second column)
//    let x =
//        let y =
//            DenseMatrix.init
//                numOfRows
//                numOfCols
//                (fun _ _ -> let s = rnd.NextDouble() in if rnd.Next() % 2 = 0 then s else -s)
//        let z = y |> Matrix.toColSeq |> (vector << List.ofSeq << Seq.head)
//        y
//        |> Matrix.insertCol (numOfCols - 2) (2.0 * z)
//        |> Matrix.insertCol (numOfCols - 2) (4.0 * z)
//        |> Matrix.insertCol (numOfCols - 2) (6.0 * z)
//        |> Matrix.insertCol (numOfCols - 2) (8.0 * z)
//        |> Matrix.insertCol (numOfCols - 2) (10.0 * z)
//    printfn "x = %A" x
//    // Get the change of basis matrix
//    let newBasisMat = PCA.changeBasis x
//    printfn "newBasisMat = %A" newBasisMat
//    // Reduce the dimension of the original matrix
//    let rMat = PCA.reduceMatDim 0.01 newBasisMat x
//    printfn "rMat = %A" rMat
//    // Generate a random vector
//    let vec = DenseVector.init (Matrix.columnCount x) (fun _ -> let s = rnd.NextDouble() in if rnd.Next() % 2 = 0 then s else -s)
//    printfn "vec = %A" vec
//    let rVec = PCA.reduceVecDim ((Matrix.columnCount x) - (Matrix.columnCount rMat)) newBasisMat vec
//    printfn "%A" rVec
//
//
//let cartTest1 (inputFileLoc : string) : unit =
//    let tbl = parseDataTableFromFile inputFileLoc
//    let impurityFn = entropy
//    let iD3Tree = buildC45 tbl impurityFn None
//    printfn "iD3Tree = %A" iD3Tree
//    let inputMap =
//        seq[
//            (DataType.Cat(CatType.Str "Gender"), DataType.Cat(CatType.Str "female"));
//            (DataType.Cat(CatType.Str "Travel cost"), DataType.Cat(CatType.Str "cheap"))
//        ]
//        |> Map.ofSeq
//    printfn "inputMap = %A" inputMap
//    let prediction = getPrediction iD3Tree inputMap
//    printfn "prediction = %A" prediction
//
//
//let cartTest2 (inputFileLoc : string) : unit =
//    let tbl = parseDataTableFromFile inputFileLoc
//    let impurityFn = entropy
//    let splitStopCriterion = defSplitStopCriterion
//    let c45Tree = buildC45 tbl impurityFn (Some splitStopCriterion)
//    printfn "c45Tree = %A" c45Tree
//    let inputMap =
//        seq [
//            // (DataType.Cat(CatType.Str "TEMPERATURE"), DataType.Cont(65.0))
//            (DataType.Cat(CatType.Str "HUMIDITY"), DataType.Cont(65.0))
//        ]
//        |> Map.ofSeq
//    printfn "inputMap = %A" inputMap
//    let prediction = getPrediction c45Tree inputMap
//    printfn "prediction = %A" prediction
//    ()
//
//
//let sw = System.Diagnostics.Stopwatch()
//sw.Start()
//let lstLen = 20
//let idxLen = 6
//let idxLst = Muster.Utils.Misc.getDistinctRandomIntList 0 (lstLen - 1) idxLen
//printfn "idxLst = %A" (idxLst)
//let lst = Muster.Utils.Misc.getDistinctRandomIntList 1 100 lstLen
//printfn "lst = %A" lst
//let extrLst = Muster.Extensions.ListExtensions.pickFromList lst idxLst
//printfn "extrLst = %A" extrLst
//sw.Stop()
//printfn "Time taken = %A ms" sw.ElapsedMilliseconds
//
//
//let lst = [1 .. 1000000]
//printfn "Length of lst = %A" (List.length lst)
//let sw = System.Diagnostics.Stopwatch()
//sw.Start()
//let lstRev = List.rev lst
//sw.Stop()
//printfn "Elapsed time = %A ms" sw.ElapsedMilliseconds
//sw.Reset()


//printfn "%A" (StringExtensions.getMaximalItems ["the"; "there"; "and"; "androgynous"])


//printfn "%A" (StringExtensions.removeNonAlphaNumChars @"an238nsdfg&93&*&@#H--=ASijfb")


//printfn "%A" ((int << floor << sqrt) 17.0)


//printfn "%A" (ListExtensions.isExtensionOf<int> [1; 2; 3] [1; 2; 5; 6])
//let s = DataType.Cat(CatType.Int 0)
//let t = DataType.Cont(0.0)
//let u = DataType.Cont(0.00)
//printfn "%A" (t = u)


let getPrediction2 (c45Tree : Node) (inputMap : Map<DataType, DataType>) : list<int * DataType> =
    let rec helper (currC45Tree : Node) (colAcc : list<DataType>) : list<list<DataType> * DataType> =
        match currC45Tree with
        | Node.Leaf v -> [(List.rev colAcc), v]
        | Node.LeafList lst -> lst |> List.map (fun s -> (List.rev colAcc), s)
        | Node.Internal internalMap ->
            let internalMapSq = internalMap |> Map.toSeq
            let internalMapKeys = internalMapSq |> Seq.map fst
            let colHdr = internalMapKeys |> Seq.head |> fst
            if (Map.containsKey colHdr inputMap) then
                let inputVal = inputMap.[colHdr]
                let newColAcc = colHdr :: colAcc
                match inputVal with
                | DataType.Cat _ -> helper internalMap.[colHdr, inputVal] newColAcc
                | DataType.Cont _ ->
                    let maxKey = Seq.maxBy snd internalMapKeys
                    let minKey = Seq.minBy snd internalMapKeys
                    if inputVal < snd maxKey then helper internalMap.[minKey] newColAcc
                    else helper internalMap.[maxKey] newColAcc
            else
                internalMapSq
                |> Seq.collect (fun ((s, _), t) -> helper t (s :: colAcc))
                |> List.ofSeq
//    c45Tree
//    |> helper
//    |> Seq.groupBy id
//    |> Seq.map (fun (s, t) -> Seq.length t, s)
//    |> List.ofSeq
    let inputColHdrs = inputMap |> Map.toSeq |> Seq.map fst |> Set.ofSeq
    (c45Tree, [])
    ||> helper
    |> (fun s ->
        List.fold
            (fun t (u, v) ->
                let w = Set.count(Set.intersect (Set.ofList u) inputColHdrs)
                let x = t |> List.head |> fst
                if w > x then [w, v]
                elif w = x then (w, v) :: t
                else t)
            (let (t, u) = List.head s in [Set.count(Set.intersect(Set.ofList t) inputColHdrs), u])
            (List.tail s))
    |> List.map snd
    |> Seq.groupBy id
    |> Seq.map (fun (s, t) -> Seq.length t, s)
    |> List.ofSeq


let cartTest3 (inputFileLoc : string) : unit =
    let tbl = parseDataTableFromFile inputFileLoc
    let impurityFn = stdDevError
    let c45Tree = buildC45 tbl impurityFn (Some(splitStopCriterionGen (float(List.length tbl)) 0.05))
    printfn "c45Tree = %A" c45Tree
    let inputMap =
        seq[
            // (DataType.Cat(CatType.Str "OUTLOOK"), DataType.Cat(CatType.Str "overcast"));
            // (DataType.Cat(CatType.Str "TEMP"), DataType.Cat(CatType.Str "cool"));
            (DataType.Cat(CatType.Str "HUMIDITY"), DataType.Cat(CatType.Str "high"));
            (DataType.Cat(CatType.Str "OUTLOOK"), DataType.Cat(CatType.Str "sunny"))
        ]
        |> Map.ofSeq
    // printfn "inputMap = %A" inputMap
    let prediction = getPrediction2 c45Tree inputMap
    printfn "prediction = %A" prediction

cartTest3(@"C:\Users\aroy\OneDrive\Repositories\Muster\Muster\SampleData\DecisionTree\SampleC45Data_2.txt")

