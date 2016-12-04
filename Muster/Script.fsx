#load "../packages/MathNet.Numerics.FSharp.3.13.1/MathNet.Numerics.fsx"
#load "../MusterLib/ListExtensions.fs"
#load "../MusterLib/StringExtensions.fs"
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
open MusterLib
open Muster.DataStructuresAndAlgorithms
open Muster.DataStructuresAndAlgorithms.DecisionTree


//let rnd = Random()
//
//
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
//let idxLst = ListExtensions.getDistinctRandomIntList 0 (lstLen - 1) idxLen
//printfn "idxLst = %A" (idxLst)
//let lst = ListExtensions.getDistinctRandomIntList 1 100 lstLen
//printfn "lst = %A" lst
//let extrLst = ListExtensions.pickFromList lst idxLst
//printfn "extrLst = %A" extrLst
//sw.Stop()
//printfn "Time taken = %A ms" sw.ElapsedMilliseconds
//
//
//let lst2 = [1 .. 1000000]
//printfn "Length of lst = %A" (List.length lst)
//let sw2 = System.Diagnostics.Stopwatch()
//sw2.Start()
//let lstRev = List.rev lst2
//sw2.Stop()
//printfn "Elapsed time = %A ms" sw2.ElapsedMilliseconds
//sw2.Reset()
//
//
//printfn "%A" (StringExtensions.getMaximalItems ["the"; "there"; "and"; "androgynous"])
//
//
//printfn "%A" (StringExtensions.removeNonAlphaNumChars @"an238nsdfg&93&*&@#H--=ASijfb")
//
//
//printfn "%A" ((int << floor << sqrt) 17.0)
//
//
//printfn "%A" (ListExtensions.isExtensionOf<int> [1; 2; 3] [1; 2; 5; 6])
//let s = DataType.Cat(CatType.Int 0)
//let t = DataType.Cont(0.0)
//let u = DataType.Cont(0.00)
//printfn "%A" (t = u)
//
//
//let cartTest3 (inputFileLoc : string) : unit =
//    let tbl = parseDataTableFromFile inputFileLoc
//    let impurityFn = stdDevError
//    let c45Tree = buildC45 tbl impurityFn (Some(splitStopCriterionGen (float(List.length tbl)) 0.05))
//    printfn "c45Tree = %A" c45Tree
//    let inputMap =
//        seq [
//            // (DataType.Cat(CatType.Str "OUTLOOK"), DataType.Cat(CatType.Str "overcast"));
//            // (DataType.Cat(CatType.Str "TEMP"), DataType.Cat(CatType.Str "cool"));
//            (DataType.Cat(CatType.Str "HUMIDITY"), DataType.Cat(CatType.Str "high"));
//            (DataType.Cat(CatType.Str "OUTLOOK"), DataType.Cat(CatType.Str "sunny"))
//        ]
//        |> Map.ofSeq
//    // printfn "inputMap = %A" inputMap
//    let prediction = getPrediction c45Tree inputMap
//    printfn "prediction = %A" prediction
//
//cartTest3(@"C:\Users\aroy\OneDrive\Repositories\Muster\Muster\SampleData\DecisionTree\SampleC45Data_2.txt")


/////


[<RequireQualifiedAccess>]
type TreeNode<'A> =
    | Leaf of 'A
    | Internal of option<'A> * list<TreeNode<'A>>


    static member map (f : 'A -> 'B) (node : TreeNode<'A>) : TreeNode<'B> =
        let rec helper (currNode : TreeNode<'A>) : TreeNode<'B> =
            match currNode with
            | TreeNode.Leaf v -> TreeNode.Leaf(f v)
            | TreeNode.Internal(v', c) -> TreeNode.Internal(Option.map f v', c |> List.map helper)
        helper node


    static member foldDfti (f : int -> int -> 'B -> TreeNode<'A> -> 'B) (initVal : 'B) (node : TreeNode<'A>) : 'B =
        let rec helper currDepth sibNum (currAcc : 'B) (currNode : TreeNode<'A>) : 'B =
            match currNode with
            | TreeNode.Leaf _ -> f currDepth sibNum currAcc currNode
            | TreeNode.Internal(_, c) ->
                (f currDepth sibNum currAcc currNode, c |> List.zip [1 .. List.length c])
                ||> List.fold (fun s (t, u) -> helper (currDepth + 1) t s u)
        helper 1 1 initVal node


    static member foldDft (f : 'B -> TreeNode<'A> -> 'B) (initVal : 'B) (node : TreeNode<'A>) : 'B =
        let g _ _ s t = f s t in (initVal, node) ||> TreeNode.foldDfti g


    static member foldValDfti (f : int -> int -> 'B -> 'A -> 'B) (initVal : 'B) (node : TreeNode<'A>) : 'B =
        let g i j s t = match t with TreeNode.Leaf v | TreeNode.Internal(Some v, _) -> f i j s v | _ -> s
        (initVal, node) ||> TreeNode.foldDfti g


    static member foldValDft (f : 'B -> 'A -> 'B) (initVal : 'B) (node : TreeNode<'A>) : 'B =
        let g s t = match t with TreeNode.Leaf v | TreeNode.Internal(Some v, _) -> f s v | _ -> s
        (initVal, node) ||> TreeNode.foldDft g


    static member accumulateValDft (f : 'A -> 'B) (node : TreeNode<'A>) : list<'B> =
        let g s t = (f t) :: s in ([], node) ||> TreeNode.foldValDft g |> List.rev


    static member tryFindDft (f : TreeNode<'A> -> bool) (node : TreeNode<'A>) : option<TreeNode<'A>> =
        let rec helper (currAcc : list<TreeNode<'A>>) (currNode : TreeNode<'A>) : option<TreeNode<'A>> =
            match currAcc, currNode with
            | _ when f currNode -> Some currNode
            | _, TreeNode.Internal(v', c) -> helper ((List.tail c) @ currAcc) (List.head c)
            | h :: t, TreeNode.Leaf _ -> helper t h
            | _ -> None
        helper [] node


    static member foldBfti (f : int -> int -> 'B -> TreeNode<'A> -> 'B) (initVal : 'B) (node : TreeNode<'A>) : 'B =
        let rec helper currDepth (currAcc : 'B) (us : list<int * TreeNode<'A>>) (ts : list<TreeNode<'A>>) : 'B =
            match us, ts with
            | [], [] -> currAcc
            | [], _ ->
                helper
                    (currDepth + 1)
                    currAcc
                    (ts |> List.rev
                    |> List.collect (function TreeNode.Leaf _ -> [] | TreeNode.Internal(_, c) -> c)
                    |> (fun s -> s |> List.zip [1 .. List.length s]))
                    []
            | (h1, h1') :: t, _ -> helper currDepth (f currDepth h1 currAcc h1') t (h1' :: ts)
        helper 1 initVal [1, node] []


    static member foldBft (f : 'B -> TreeNode<'A> -> 'B) (initVal : 'B) (node : TreeNode<'A>) : 'B =
        let g _ _ s t = f s t in (initVal, node) ||> TreeNode.foldBfti g


    static member foldValBfti (f : int -> int -> 'B -> 'A -> 'B) (initVal : 'B) (node : TreeNode<'A>) : 'B =
        let g i j s t = match t with TreeNode.Leaf v | TreeNode.Internal(Some v, _) -> f i j s v | _ -> s
        (initVal, node) ||> TreeNode.foldBfti g


    static member foldValBft (f : 'B -> 'A -> 'B) (initVal : 'B) (node : TreeNode<'A>) : 'B =
        let g s t = match t with TreeNode.Leaf v | TreeNode.Internal(Some v, _) -> f s v | _ -> s
        (initVal, node) ||> TreeNode.foldBft g


    static member accumulateValBft (f : 'A -> 'B) (node : TreeNode<'A>) : list<'B> =
        let g s t = (f t) :: s in ([], node) ||> TreeNode.foldValBft g |> List.rev


    static member tryFindBft (f : TreeNode<'A> -> bool) (node : TreeNode<'A>) : option<TreeNode<'A>> =
        let rec helper (us : list<TreeNode<'A>>) (ts : list<TreeNode<'A>>) : option<TreeNode<'A>> =
            match us, ts with
            | [], [] -> None
            | [], _ ->
                helper
                    (ts |> List.rev |> List.collect (function TreeNode.Leaf _ -> [] | TreeNode.Internal(_, c) -> c))
                    []
            | h :: t, _ -> if f h then Some h else helper t (h :: ts)
        helper [node] []


    static member prettyPrint (node : TreeNode<'A>) : unit =
        let c, d = " . ", 4
        let f i j (s : list<int * int * string>) t =
            (i, j, match t with TreeNode.Leaf v | TreeNode.Internal(Some v, _) -> v.ToString() | _ -> "MISSING") :: s
        ([], node) ||> TreeNode.foldDfti f
        |> List.map (fun (s, _, t) -> String.concat "" [for i in 1 .. d * (s - 1) -> c] + t)
        |> List.rev
        |> String.concat Environment.NewLine
        |> printfn "%s"


//    static member prettyPrint (node : TreeNode<'A>) : unit =
//        let f i j s t =
//            match t with 
//        ()


// type Path = ChildNum of int


//[<RequireQualifiedAccess>]
//type Zipper<'A> =
//    | Start
//    | Internal of TreeNode<'A> * Path * Zipper<'A>
//    | End of TreeNode<'A> * Zipper<'A>
//    static member apply
//        (node : TreeNode<'A>)
//        (pFn : TreeNode<'A> -> 'B -> Path)
//        (eFn : TreeNode<'A> -> 'B -> bool)
//        (mFn : TreeNode<'A> -> 'B -> TreeNode<'A>)
//        (fPars : 'B)
//        : TreeNode<'A> =
//        let rec unzip (currNode : TreeNode<'A>) (currZipper : Zipper<'A>) : Zipper<'A> =
//            Zipper.Start
//        let rec zip (currZipper : Zipper<'A>) : option<TreeNode<'A>> =
//            None
//        node


printfn "FINISHED LOADING DEFINITIONS"


let treeRnd =
    let rnd, lim = System.Random(), 64
    TreeNode.Internal(
        Some(rnd.Next() % lim), [
            TreeNode.Internal(
                Some(rnd.Next() % lim), [
                    TreeNode.Leaf(rnd.Next() % lim);
                    TreeNode.Internal(
                        Some(rnd.Next() % lim),
                        [TreeNode.Leaf(rnd.Next() % lim)])]);
            TreeNode.Internal(
                Some(rnd.Next() % lim), [
                    TreeNode.Internal(
                        Some(rnd.Next() % lim), [
                            TreeNode.Leaf(rnd.Next() % lim)]);
                    TreeNode.Internal(
                        Some(rnd.Next() % lim),
                        [TreeNode.Leaf(rnd.Next() % lim)])])])


let treeSeq =
    TreeNode.Internal(
        Some 0, [
            TreeNode.Internal(
                Some 1, [
                    TreeNode.Leaf 5;
                    TreeNode.Internal(
                        Some 3,
                        [TreeNode.Leaf 4])]);
            TreeNode.Internal(
                Some 5, [
                    TreeNode.Internal(
                        Some 6, [
                            TreeNode.Leaf 7]);
                    TreeNode.Internal(
                        Some 8,
                        [TreeNode.Leaf 9])])])


printfn "FINISHED LOADING VARIABLES"


///////


// treeSeq |> TreeNode.prettyPrint

//printfn "%A" (
//    (None, treeRnd)
//    ||> TreeNode.foldDft (fun (s : option<TreeNode<int>>) (t : TreeNode<int>) ->
//        match s with
//        | Some _ -> s
//        | None -> let u = TreeNode.foldValDft (+) 0 t in if (u < 8) && (u > 2) then (Some t) else None
//    )
//)

//printfn "%d" (treeSeq |> TreeNode.foldValDft (+) 0)

//printfn "%A" (treeSeq |> TreeNode.accumulateValDft id)

//printfn "%A" (
//    let f = (<) 4
//    let g = function TreeNode.Leaf v -> f v | TreeNode.Internal(v', _) -> Option.exists f v'
//    (treeSeq |> TreeNode.tryFindDft g, treeSeq |> TreeNode.tryFindBft g)
//)

//printfn "%A" (treeSeq |> TreeNode.foldValBft (+) 0)

//printfn "%A" (treeSeq |> TreeNode.accumulateValBft id)

//printfn "%A" (
//    let f = (<) 4
//    let g = function TreeNode.Leaf v -> f v | TreeNode.Internal(v', _) -> Option.exists f v'
//    treeRnd |> TreeNode.tryFindBft g
//)

treeSeq |> TreeNode.prettyPrint
treeRnd |> TreeNode.prettyPrint

