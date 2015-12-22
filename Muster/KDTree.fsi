namespace Muster.DataStructuresAndAlgorithms


open System


module KDTree =


    type Vec<'A when 'A : comparison> = array<option<'A>>


    [<RequireQualifiedAccess; CustomEquality; CustomComparison>]
    type Node<'A when 'A : comparison> =
        {LeftChild : option<Node<'A>>;
        RightChild : option<Node<'A>>;
        SplittingAxis : int;
        ID : int;
        Vec : array<'A>}
        interface IComparable


    [<RequireQualifiedAccess>]
    type Query<'A when 'A : comparison> =
        | Just of Vec<'A>
        | JustInRadius of Vec<'A> * int
        | ClosedBounds of Vec<'A> * Vec<'A>
        | ClosedOpenBounds of Vec<'A> * Vec<'A>
        | OpenClosedBounds of Vec<'A> * Vec<'A>
        | OpenBounds of Vec<'A> * Vec<'A>
        | And of list<Query<'A>>
        | Or of list<Query<'A>>
        | Diff of list<Query<'A>>


    type DiffFunc<'A when 'A : comparison> = option<'A> -> 'A -> int


    type DistFunc<'A when 'A : comparison> = DiffFunc<'A> -> Vec<'A> -> array<'A> -> int


    val genRandTestIntVec : int -> int -> int -> Vec<int>


    val genRandIntVecsArr : int -> int -> int -> array<array<int>>


    val genRandTestInt16Vec : int -> int16 -> int -> Vec<int16>


    val genRandInt16VecsArr : int -> int -> int16 -> array<array<int16>>


    val genRandTestCharVec : int -> int -> Vec<char>


    val genRandCharVecsArr : int -> int -> array<array<char>>


    val genRandTestDblVec : int -> double -> int -> Vec<double>


    val genRandDblVecsArr : int -> int -> double -> array<array<double>>


    val build : array<array<'A>> -> option<Node<'A>>


    val addVec : Node<'A> -> array<'A> -> Node<'A>


    val removeVec : Node<'A> -> array<'A> -> option<Node<'A>>


    val rebuild : Node<'A> -> option<Node<'A>>


    val intDiffFunc : DiffFunc<int>


    val int16DiffFunc : DiffFunc<int16>


    val charDiffFunc : DiffFunc<char>


    val dblDiffFunc : int -> DiffFunc<double>


    val euclDistFunc : DiffFunc<'A> -> Vec<'A> -> array<'A> -> int


    val runNNQuery : Node<'A> -> DistFunc<'A> -> DiffFunc<'A> -> Query<'A> -> Set<Node<'A>>


    val runNNQueryAsync : Node<'A> -> DistFunc<'A> -> DiffFunc<'A> -> Query<'A> -> Async<Set<Node<'A>>>


    val runRangeQuery : Node<'A> -> Query<'A> -> Set<Node<'A>>


    val runRangeQueryAsync : Node<'A> -> Query<'A> -> Async<Set<Node<'A>>>


    val getBestMatchesByLinearScanning : array<array<'A>> -> Vec<'A> -> DistFunc<'A> -> DiffFunc<'A> -> list<array<'A>>


    val getClosedRangeMatchesByLinearScanning : array<array<'A>> -> Vec<'A> -> Vec<'A> -> array<array<'A>>


    val getClosedOpenRangeMatchesByLinearScanning : array<array<'A>> -> Vec<'A> -> Vec<'A> -> array<array<'A>>


    val getOpenClosedRangeMatchesByLinearScanning : array<array<'A>> -> Vec<'A> -> Vec<'A> -> array<array<'A>>


    val getOpenRangeMatchesByLinearScanning : array<array<'A>> -> Vec<'A> -> Vec<'A> -> array<array<'A>>

