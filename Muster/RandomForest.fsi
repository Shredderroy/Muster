namespace Muster.DataStructuresAndAlgorithms


module RandomForest =


    type DataType = DecisionTree.DataType


    type Node = DecisionTree.Node


    type DataTable = DecisionTree.DataTable


    type ImpurityFn = DecisionTree.ImpurityFn


    type SplitStopCriterion = DecisionTree.SplitStopCriterion


    type Forest = list<DecisionTree.Node>


    [<RequireQualifiedAccess>]
    type SampleSize =
        | Int of int
        | Pct of float


    val parseDataTableFromFile : string -> DataTable


    val entropy : ImpurityFn


    val giniIndex : ImpurityFn


    val classificationError : ImpurityFn


    val defSplitStopCriterion : SplitStopCriterion


    val buildWithParams : DataTable -> int -> SampleSize -> ImpurityFn -> option<SplitStopCriterion> -> Forest


    val buildDefault : DataTable -> int -> Forest


    val buildDefaultFromFile : string -> int -> Forest


    val getPrediction : Forest -> Map<DataType, DataType> -> list<int * DataType>

