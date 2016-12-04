// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "ListExtensions.fs"
#load "CSV.fs"
#load "Tree.fs"

open MusterLib

//let mx = 1000000
//let lst1 = [for i in 1 .. mx -> i]
//let lst2 = [for i in 1 .. (mx + 1) -> i]
//
//printfn "Starting"
//
//lst2
//|> ListExtensions.isExtensionOf lst1
//|> printfn "%A"
//
//printfn "Done"

/////

[@"This, is, a, ""nice, string"", yep";
@"This, is, another, ""nice, string"", aye"]
|> CSV.parse ',' false
|> Array.head
|> (fun s ->
    printfn "%s" s.["col1"]
    printfn "%s" s.["col2"]
    printfn "%s" s.["col3"]
    printfn "%s" s.["col4"]
    printfn "%s" s.["col5"]
)

/////

// type Path = ChildNum of int

//[<RequireQualifiedAccess>]
//type Zipper<'A> =
//    | Start
//    | Internal of Tree.Node<'A> * Path * Zipper<'A>
//    | End of Tree.Node<'A> * Zipper<'A>
//    static member apply
//        (node : Tree.Node<'A>)
//        (pFn : Tree.Node<'A> -> 'B -> Path)
//        (eFn : Tree.Node<'A> -> 'B -> bool)
//        (mFn : Tree.Node<'A> -> 'B -> Tree.Node<'A>)
//        (fPars : 'B)
//        : Tree.Node<'A> =
//        let rec unzip (currNode : Tree.Node<'A>) (currZipper : Zipper<'A>) : Zipper<'A> =
//            Zipper.Start
//        let rec zip (currZipper : Zipper<'A>) : option<Tree.Node<'A>> =
//            None
//        node

/////

let treeRnd =
    let rnd, lim = System.Random(), 64
    Tree.Node.Internal(
        Some(rnd.Next() % lim), [
            Tree.Node.Internal(
                Some(rnd.Next() % lim), [
                    Tree.Node.Leaf(rnd.Next() % lim);
                    Tree.Node.Internal(
                        Some(rnd.Next() % lim),
                        [Tree.Node.Leaf(rnd.Next() % lim)])]);
            Tree.Node.Internal(
                Some(rnd.Next() % lim), [
                    Tree.Node.Internal(
                        Some(rnd.Next() % lim), [
                            Tree.Node.Leaf(rnd.Next() % lim)]);
                    Tree.Node.Internal(
                        Some(rnd.Next() % lim),
                        [Tree.Node.Leaf(rnd.Next() % lim)])])])


let treeSeq =
    Tree.Node.Internal(
        Some 0, [
            Tree.Node.Internal(
                Some 1, [
                    Tree.Node.Leaf 5;
                    Tree.Node.Internal(
                        Some 3,
                        [Tree.Node.Leaf 4])]);
            Tree.Node.Internal(
                Some 5, [
                    Tree.Node.Internal(
                        Some 6, [
                            Tree.Node.Leaf 7]);
                    Tree.Node.Internal(
                        Some 8,
                        [Tree.Node.Leaf 9])])])


let xDep, bLeaf, xCh, flg = 4, 2, 4, true
let f (i : int) (j : int) : int = i + j
let treeGen = Tree.genRandTree xDep bLeaf xCh flg f |> Option.get


printfn "treeSeq ="; treeSeq |> Tree.prettyPrint
printfn "treeRnd ="; treeRnd |> Tree.prettyPrint
printfn "treeGen ="; treeGen |> Tree.prettyPrint


printfn "FINISHED LOADING TREES"

// treeSeq |> Tree.prettyPrint

//printfn "%A" (
//    (None, treeRnd)
//    ||> Tree.foldDft (fun (s : option<Tree.Node<int>>) (t : Tree.Node<int>) ->
//        match s with
//        | Some _ -> s
//        | None -> let u = Tree.Node.foldValDft (+) 0 t in if (u < 8) && (u > 2) then (Some t) else None
//    )
//)

//printfn "%d" (treeSeq |> Tree.Node.foldValDft (+) 0)

//printfn "%A" (treeSeq |> Tree.Node.accumulateValDft id)

//printfn "%A" (
//    let f = (<) 4
//    let g = function Tree.Node.Leaf v -> f v | Tree.Node.Internal(v', _) -> Option.exists f v'
//    (treeSeq |> Tree.Node.tryFindDft g, treeSeq |> Tree.Node.tryFindBft g)
//)

//printfn "%A" (treeSeq |> Tree.Node.foldValBft (+) 0)

//printfn "%A" (treeSeq |> Tree.Node.accumulateValBft id)

//printfn "%A" (
//    let f = (<) 4
//    let g = function Tree.Node.Leaf v -> f v | Tree.Node.Internal(v', _) -> Option.exists f v'
//    treeRnd |> Tree.Node.tryFindBft g
//)
