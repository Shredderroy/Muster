namespace Muster.DataStructuresAndAlgorithms


module CART =


    [<RequireQualifiedAccess; StructuralComparison; StructuralEquality>]
    type CatType =
        | Int of int
        | Str of string
        | Bool of bool


    [<RequireQualifiedAccess; StructuralComparison; StructuralEquality>]
    type ContType = | Flt of float


    [<RequireQualifiedAccess; StructuralComparison; StructuralEquality>]
    type DataType =
        | Cat of CatType
        | Cont of ContType


    type DataTable = list<array<DataType>>


    [<RequireQualifiedAccess>]
    type DecisionTreeNode =
        | Leaf of DataType
        | LeafList of list<DataType>
        | Internal of Map<DataType * DataType, DecisionTreeNode>


    val defSplitStopCriterion : seq<seq<DataType>> -> bool


    val buildC45 : DataTable -> (list<DataType> -> float) -> option<seq<seq<DataType>> -> bool> -> DecisionTreeNode


    val getPrediction : DecisionTreeNode -> Map<DataType, DataType> -> list<int * DataType>

