namespace Muster.DataStructuresAndAlgorithms


open System
open Muster.Extensions
open Muster.Utils


module RandomForest =


    type DataType = DecisionTree.DataType


    type Node = DecisionTree.Node


    type DataTable = DecisionTree.DataTable


    type ImpurityFn = DecisionTree.ImpurityFn


    type SplitStopCriterion = DecisionTree.SplitStopCriterion


    type Forest = list<Node>


    [<RequireQualifiedAccess>]
    type SampleSize =
        | Int of int
        | Pct of float


    let errorMsgs =
        seq [
            ("sampleSizeErrorMsg", "Invalid specification of SampleSize")]
        |> Map.ofSeq


    let parseDataTableFromFile (filePath : string) : DataTable = DecisionTree.parseDataTableFromFile filePath


    let entropy = DecisionTree.entropy


    let giniIndex = DecisionTree.giniIndex


    let classificationError = DecisionTree.classificationError


    let defSplitStopCriterion = DecisionTree.defSplitStopCriterion


    let buildWithParams
        (tbl : DataTable)
        (numOfTrees : int)
        (sampleSize: SampleSize)
        (impurityFn : ImpurityFn)
        (splitStopCriterionOpt : option<SplitStopCriterion>)
        : Forest =
        let sampleSize =
            match sampleSize with
            | SampleSize.Int v -> v
            | SampleSize.Pct p when (p > 0.0) && (p < 1.0) ->
                tbl
                |> List.length
                |> float
                |> ( * ) p
                |> Math.Floor
                |> int
            | _ -> failwith errorMsgs.["sampleSizeErrorMsg"]
        let tblDatLen = (List.length << List.tail) tbl
        let colHdrs = List.head tbl
        let colHdrsLen = Array.length colHdrs
        (List.init numOfTrees (fun _ -> Misc.getDistinctRandomIntList 1 tblDatLen sampleSize))
        |> List.map (fun s -> colHdrs :: (s |> ListExtensions.pickFromList tbl))
        |> List.map (fun s ->
            let rndLst =
                (Misc.getDistinctRandomIntList 0 (colHdrsLen - 2) ((int << floor << sqrt << float) (colHdrsLen - 1))) @
                    [colHdrsLen - 1]
            s |> List.map (fun t -> rndLst |> List.map (Array.get t) |> Array.ofList))
        |> List.map (fun (s) -> DecisionTree.buildC45 s impurityFn splitStopCriterionOpt)


    let buildDefault (tbl : DataTable) (numOfTrees : int): Forest =
        let sampleSize = SampleSize.Int(((List.length << List.tail) tbl) / numOfTrees)
        buildWithParams tbl numOfTrees sampleSize entropy (Some defSplitStopCriterion)


    let getPrediction (forest : Forest) (inputMap : Map<DataType, DataType>) : list<int * DataType> =
        forest
        |> List.map (fun (s) -> async {return DecisionTree.getPrediction s inputMap})
        |> Async.Parallel
        |> Async.RunSynchronously
        |> List.ofArray
        |> List.collect id
        |> Seq.groupBy snd
        |> Seq.map (fun (s, t) -> (t |> Seq.sumBy fst), s)
        |> List.ofSeq

