namespace Muster.DataStructuresAndAlgorithms


module DecisionTree =


    [<RequireQualifiedAccess; StructuralComparison; StructuralEquality>]
    type CatType =
        | Int of int
        | Str of string
        | Bool of bool


    [<RequireQualifiedAccess; StructuralComparison; StructuralEquality>]
    type DataType =
        | Cat of CatType
        | Cont of float


    type DataTable = list<array<DataType>>


    [<RequireQualifiedAccess>]
    type Node =
        | Leaf of DataType
        | LeafList of list<DataType>
        | Internal of Map<DataType * DataType, Node>


    type ImpurityFn = list<DataType> -> float


    type SplitStopCriterion = seq<seq<DataType>> -> bool


    val parseDataTableFromFile : string -> DataTable


    val entropy : ImpurityFn


    val giniIndex : ImpurityFn


    val classificationError : ImpurityFn


    val stdDevError : ImpurityFn


    val defSplitStopCriterion : SplitStopCriterion


    val splitStopCriterionGen : float -> float -> SplitStopCriterion


    val buildC45 : DataTable -> ImpurityFn -> option<SplitStopCriterion> -> Node


    val getPrediction : Node -> Map<DataType, DataType> -> list<int * DataType>

