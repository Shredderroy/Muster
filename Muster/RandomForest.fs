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


    let entropy = DecisionTree.entropy


    let giniIndex = DecisionTree.giniIndex


    let classificationError = DecisionTree.classificationError


    let defSplitStopCriterion = DecisionTree.defSplitStopCriterion


    let buildWithParams
        (tbl : DataTable)
        (b : int)
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
        let tblLenPred = (List.length tbl) - 1
        (List.init b (fun _ -> Misc.getDistinctRandomIntList 0 tblLenPred sampleSize))
        |> List.map (fun s -> s |> List.sort |> ListExtensions.pickFromList tbl)
        |> List.map (fun s -> DecisionTree.buildC45 s impurityFn splitStopCriterionOpt)


    let buildDefault (tbl : DataTable) (b : int) (sampleSize : SampleSize) : Forest =
        buildWithParams tbl b sampleSize entropy (Some defSplitStopCriterion)


    // let getPrediction (forest : Forest) (inputMap : Map)

