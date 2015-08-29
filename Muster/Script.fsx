// #r "../packages/MathNet.Numerics.FSharp.3.6.0/lib/net40/MathNet.Numerics.FSharp.dll"
#load "../packages/MathNet.Numerics.FSharp.3.7.0/MathNet.Numerics.fsx"
#load "Extensions.fs"
#load "KDTree.fs"
#load "ANN.fs"
#load "PCA.fs"
#load "CART.fs"

open System
open System.Diagnostics
open System.IO
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.LinearAlgebra.Double
open Muster.Extensions
open Muster.DataStructuresAndAlgorithms
open Muster.DataStructuresAndAlgorithms.CART


let rnd = Random()


let createSquareOfSumDataFile (outputFileLoc : string) : unit =
    let numOfPairs, inputDim, maxVal = 500, 3, 10
    let genFn = fun _ -> let x = (rnd.NextDouble() * (double(rnd.Next() % maxVal))) in if rnd.Next() % 2 = 0 then x else -x
    let squareOfSum = fun (s : array<double>) -> let t = Array.sum s in [|t * t|]
    [|1 .. numOfPairs|]
    |> Array.map (fun _ -> let s = ([|1 .. inputDim|] |> Array.map genFn) in [| s; squareOfSum s|])
    |> Array.map (fun s -> s |> Array.map (fun t -> t |> Array.map (string) |> (fun u -> String.Join(", ", u))))
    |> Array.map (fun s -> String.Join(", ", s))
    |> (fun s -> File.WriteAllLines(outputFileLoc, s))
    printfn "Done"


let createSineOfSumDataFile (outputFileLoc : string) : unit =
    let numOfPairs, inputDim, maxVal = 1000, 3, 10
    let genFn = fun _ -> let x = (rnd.NextDouble() * (double(rnd.Next() % maxVal))) in if rnd.Next() % 2 = 0 then x else -x
    let sineOfSum = fun (s : array<double>) -> let t = Array.sum s in [|sin t|]
    [|1 .. numOfPairs|]
    |> Array.map (fun _ -> let s = ([|1 .. inputDim|] |> Array.map genFn) in [| s; sineOfSum s|])
    |> Array.map (fun s -> s |> Array.map (fun t -> t |> Array.map (string) |> (fun u -> String.Join(", ", u))))
    |> Array.map (fun s -> String.Join(", ", s))
    |> (fun s -> File.WriteAllLines(outputFileLoc, s))
    printfn "Done"


let createAND3 (outputFileLoc : string) : unit =
    let numOfPairs, inputDim = 250, 3
    let genFn = fun _ -> double (rnd.Next() % 2)
    let and3Fn = fun (s : array<double>) -> let t = Array.sum s in [|(if t = 3.0 then 1.0 else 0.0)|]
    [|1 .. numOfPairs|]
    |> Array.map (fun _ -> let s = ([|1 .. inputDim|] |> Array.map genFn) in [| s; and3Fn s|])
    |> Array.map (fun s -> s |> Array.map (fun t -> t |> Array.map (string) |> (fun u -> String.Join(", ", u))))
    |> Array.map (fun s -> String.Join(", ", s))
    |> (fun s -> File.WriteAllLines(outputFileLoc, s))
    printfn "Done"


let createOR3 (outputFileLoc : string) : unit =
    let numOfPairs, inputDim = 250, 3
    let genFn = fun _ -> double (rnd.Next() % 2)
    let or3Fn = fun (s : array<double>) -> let t = Array.sum s in [|(if t = 0.0 then 0.0 else 1.0)|]
    [|1 .. numOfPairs|]
    |> Array.map (fun _ -> let s = ([|1 .. inputDim|] |> Array.map genFn) in [| s; or3Fn s|])
    |> Array.map (fun s -> s |> Array.map (fun t -> t |> Array.map (string) |> (fun u -> String.Join(", ", u))))
    |> Array.map (fun s -> String.Join(", ", s))
    |> (fun s -> File.WriteAllLines(outputFileLoc, s))
    printfn "Done"


let createXOR2 (outputFileLoc : string) : unit =
    let numOfPairs, inputDim = 250, 2
    let genFn = fun _ -> double (rnd.Next() % 2)
    let xor2Fn = fun (s : array<double>) -> let t = Array.sum s in [|(if t = 1.0 then 1.0 else 0.0)|]
    [|1 .. numOfPairs|]
    |> Array.map (fun _ -> let s = ([|1 .. inputDim|] |> Array.map genFn) in [| s; xor2Fn s|])
    |> Array.map (fun s -> s |> Array.map (fun t -> t |> Array.map (string) |> (fun u -> String.Join(", ", u))))
    |> Array.map (fun s -> String.Join(", ", s))
    |> (fun s -> File.WriteAllLines(outputFileLoc, s))
    printfn "Done"


let kDTreeTest () : unit =
    let dim = 3
    let numOfVecs = 8
    let (maxVal : int16) = 8s
    let b = 10
    let vecsArr = KDTree.genRandInt16VecsArr dim numOfVecs maxVal
    let diffFunc = KDTree.int16DiffFunc
    let distFunc = KDTree.euclDistFunc
    let sW = Stopwatch()
    let kDT =
        sW.Start()
        printfn "Start building the k-d tree"
        let t = (KDTree.build vecsArr).Value
        sW.Stop()
        printfn "Finished; time taken to build the k-d tree: %A ms" sW.ElapsedMilliseconds
        sW.Reset()
        t
    printfn "%A" kDT
    let kDT2 = KDTree.removeVec kDT (kDT.LeftChild.Value.Vec)
    printfn "%A" kDT2


let aNNTest (trainingSetFileLoc : string) : unit =
    let trainingSet = ANN.importTrainingSetFromFile trainingSetFileLoc 3 1
    let inputVecDim = trainingSet.InputVecsMat |> Matrix.columnCount
    let outputVecsDim = trainingSet.OutputVecsMat |> Matrix.columnCount
    let layerConfig = [inputVecDim; 5; 10; 4; outputVecsDim]
    let learningParam = 0.05
    let aNN =
        ANN.init
            layerConfig
            ANN.defaultInputActivation
            ANN.defaultDInputActivation
            ANN.defaultOutputActivation
            ANN.defaultDOutputActivation
            learningParam
    let numOfEpochs = 10000
    let trainingMode = ANN.TrainingMode.Single
    printfn "Start training aNN"
    let sW = Stopwatch()
    sW.Start()
    let tANN = ANN.train aNN trainingSet numOfEpochs trainingMode
    sW.Stop()
    printfn "Training complete; time taken = %A ms" sW.ElapsedMilliseconds
    sW.Reset()
    // let rmsd = ANN.getRMSDForTrainingSet tANN trainingSet
    // printfn "rmsd = %A" rmsd
    let inpVec, outVec =
        let n = 1 + (rnd.Next() % ((Matrix.rowCount trainingSet.InputVecsMat) - 1))
        (trainingSet.InputVecsMat |> Matrix.toRowSeq |> Seq.skip n |> Seq.head),
        (trainingSet.OutputVecsMat |> Matrix.toRowSeq |> Seq.skip n |> Seq.head)
    let pred = ANN.getPrediction tANN inpVec
    printfn "inpVec = %A" inpVec
    printfn "pred = %A" pred
    printfn "outVec = %A" outVec


let kaggleCompetitionOttoGroup (trainingSetFileLoc : string) : unit =
    let trainingSet =
        (File.ReadAllLines(trainingSetFileLoc)).[1..]
        |> Array.map (fun s -> let t = s.Split([|','|], StringSplitOptions.RemoveEmptyEntries).[1..] in (t.[..(t.Length - 2)], t.[t.Length - 1]))
        |> Array.map (fun (s, t) -> (s, (t.Split [|'_'|]).[1]))
        |> Array.map (fun (s, t) -> (s |> Array.map (double)), (double t))
        |> Array.fold (fun (s, t) (u, v) -> ((List.ofArray u) :: s), ([v] :: t)) ([], [])
        |> (fun (s, t) -> {ANN.TrainingSet.InputVecsMat = matrix (List.rev s); ANN.TrainingSet.OutputVecsMat = matrix (List.rev t)})
    let inputVecDim = trainingSet.InputVecsMat.ColumnCount
    let outputVecsDim = trainingSet.OutputVecsMat.ColumnCount
    let layerConfig = [inputVecDim; 100; 75; 50; outputVecsDim]
    let learningParam = 0.05
    let aNN =
        ANN.init
            layerConfig
            ANN.defaultInputActivation
            ANN.defaultDInputActivation
            ANN.defaultOutputActivation
            ANN.defaultDOutputActivation
            learningParam
    let numOfEpochs = 2
    let trainingMode = ANN.TrainingMode.Single
    printfn "Start training aNN"
    let sW = Stopwatch()
    sW.Start()
    let tANN = ANN.train aNN trainingSet numOfEpochs trainingMode
    sW.Stop()
    printfn "Training complete; time taken = %A ms" sW.ElapsedMilliseconds
    sW.Reset()
    // let rmsd = ANN.getRMSDForTrainingSet tANN trainingSet
    // printfn "rmsd = %A" rmsd
    let inpVec, outVec =
        let n = 1 + (rnd.Next() % ((Matrix.rowCount trainingSet.InputVecsMat) - 1))
        (trainingSet.InputVecsMat |> Matrix.toRowSeq |> Seq.skip n |> Seq.head),
        (trainingSet.OutputVecsMat |> Matrix.toRowSeq |> Seq.skip n |> Seq.head)
    let pred = ANN.getPrediction tANN inpVec
    printfn "inpVec = %A" inpVec
    printfn "pred = %A" pred
    printfn "outVec = %A" outVec


let pcaTest () : unit =
    // Set the dimensions of the matrix
    let numOfRows, numOfCols = 7, 3
    // Generate a matrix with a dependent column (the second column)
    let x =
        let y =
            DenseMatrix.init
                numOfRows
                numOfCols
                (fun _ _ -> let s = rnd.NextDouble() in if rnd.Next() % 2 = 0 then s else -s)
        let z = y |> Matrix.toColSeq |> (vector << List.ofSeq << Seq.head)
        Matrix.insertCol (numOfCols - 2) (2.0 * z) y
    // let x = matrix [[1.0; 2.0]; [2.0; 3.0]; [3.0; 4.0]; [4.0; 5.0]]
    printfn "x = %A" x
    // Get the change of basis matrix
    let newBasisMat = PCA.changeBasis x
    printfn "newBasisMat = %A" newBasisMat
    // Reduce the dimension of the original matrix
    let rMat = PCA.reduceMatDim 0.01 newBasisMat x
    printfn "rMat = %A" rMat
    // Generate a random vector
    let vec = DenseVector.init (numOfCols + 1) (fun _ -> let s = rnd.NextDouble() in if rnd.Next() % 2 = 0 then s else -s)
    printfn "vec = %A" vec
    let rVec = PCA.reduceVecDim ((numOfCols + 1) - (Matrix.columnCount rMat)) newBasisMat vec
    printfn "%A" rVec


let cartTest () : unit =
    let (tbl : DataTable) =
        [
        [|DataType.Cat(CatType.Str "G"); DataType.Cat(CatType.Str "CO"); DataType.Cat(CatType.Str "TC"); DataType.Cat(CatType.Str "IL"); DataType.Cat(CatType.Str "T")|];
        [|DataType.Cat(CatType.Str "M"); DataType.Cat(CatType.Str "0"); DataType.Cat(CatType.Str "ch"); DataType.Cat(CatType.Str "lw"); DataType.Cat(CatType.Str "bus")|];
        [|DataType.Cat(CatType.Str "M"); DataType.Cat(CatType.Str "1"); DataType.Cat(CatType.Str "ch"); DataType.Cat(CatType.Str "md"); DataType.Cat(CatType.Str "bus")|];
        [|DataType.Cat(CatType.Str "F"); DataType.Cat(CatType.Str "1"); DataType.Cat(CatType.Str "ch"); DataType.Cat(CatType.Str "md"); DataType.Cat(CatType.Str "trn")|];
        [|DataType.Cat(CatType.Str "F"); DataType.Cat(CatType.Str "0"); DataType.Cat(CatType.Str "ch"); DataType.Cat(CatType.Str "lw"); DataType.Cat(CatType.Str "bus")|];
        [|DataType.Cat(CatType.Str "M"); DataType.Cat(CatType.Str "1"); DataType.Cat(CatType.Str "ch"); DataType.Cat(CatType.Str "md"); DataType.Cat(CatType.Str "bus")|];
        [|DataType.Cat(CatType.Str "M"); DataType.Cat(CatType.Str "0"); DataType.Cat(CatType.Str "st"); DataType.Cat(CatType.Str "md"); DataType.Cat(CatType.Str "trn")|];
        [|DataType.Cat(CatType.Str "F"); DataType.Cat(CatType.Str "1"); DataType.Cat(CatType.Str "st"); DataType.Cat(CatType.Str "md"); DataType.Cat(CatType.Str "trn")|];
        [|DataType.Cat(CatType.Str "F"); DataType.Cat(CatType.Str "1"); DataType.Cat(CatType.Str "ex"); DataType.Cat(CatType.Str "hh"); DataType.Cat(CatType.Str "car")|];
        [|DataType.Cat(CatType.Str "M"); DataType.Cat(CatType.Str "2"); DataType.Cat(CatType.Str "ex"); DataType.Cat(CatType.Str "md"); DataType.Cat(CatType.Str "car")|];
        [|DataType.Cat(CatType.Str "F"); DataType.Cat(CatType.Str "2"); DataType.Cat(CatType.Str "ex"); DataType.Cat(CatType.Str "hh"); DataType.Cat(CatType.Str "car")|]
        ]
    let tblDat = List.tail tbl
    let impurityFn = entropy
    let datSetImpurity = impurityFn (tblDat |> List.map (fun s -> s.[4]))
    let infoGainVals =
        [|1 .. ((Array.length << List.head) tbl)|]
        |> Array.collect (fun s -> getInfoGain tblDat (s - 1) impurityFn datSetImpurity)
    let idx = 2
    let tblsLst = getTblDatSplits tblDat idx None
    let prunedTblsLst =
        getPrunedTbls
            tblsLst
            idx
            [||]
            (fun _ -> true)
    printfn "%A" (Array.ofList prunedTblsLst).[2]
    ()


cartTest()

