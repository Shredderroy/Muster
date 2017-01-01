// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "ListExtensions.fs"
#load "CSV.fs"
#load "Tree.fs"

open MusterLib

/////

//[@"This, is, a, ""nice, string"", yep";
//@"This, is, another, ""nice, string"", aye"]
//|> CSV.parse ',' false
//|> Array.head
//|> (fun s ->
//    printfn "%s" s.["col1"]
//    printfn "%s" s.["col2"]
//    printfn "%s" s.["col3"]
//    printfn "%s" s.["col4"]
//    printfn "%s" s.["col5"]
//)

/////

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


let treeGen =
    let xDep, bLeaf, xCh, flg = 4, 2, 4, true
    let f (i : int) (j : int) = i, j
    Tree.genRandTree xDep bLeaf xCh flg f |> Option.get


let treeFull = [2; 3; 3; 2] |> Tree.genFullTree (fun d i -> (pown 2 d) + i)


printfn "FINISHED LOADING TREES"

/////

[<RequireQualifiedAccess>]
type Zipper<'A> =
    | Start
    | Internal of Tree.Node<'A> * int * Zipper<'A>
    | End of Tree.Node<'A> * Zipper<'A>
    static member apply
        (node : Tree.Node<'A>)
        (path : Tree.Path)
        (mf : Tree.Node<'A> -> 'B -> Tree.Node<'A>)
        (prms : 'B)
        : Tree.Node<'A> =
        let rec unzip (currNode : Tree.Node<'A>) (currZipper : Zipper<'A>) : Zipper<'A> =
            Zipper.Start
        let rec zip (currZipper : Zipper<'A>) : option<Tree.Node<'A>> =
            None
        match zip(unzip node Zipper.Start) with None -> node | Some newNode -> newNode

/////

//module Graph =
//
//
//    type Node<'A> =
//        | Leaf of nodeVal : 'A
//        | Internal of nodeVal : option<'A> * parent : option<Node<'A>> * childNodes : list<Node<'A>>
//
//
//    let tryFindDft (f : Node<'A> -> bool) (node : Node<'A>) : option<Node<'A>> =
//        None
//
//
//    let tryFindBft (f : Node<'A> -> bool) (node : Node<'A>) : option<Node<'A>> =
//        None

/////

treeSeq |> Tree.prettyPrint
treeFull |> Tree.prettyPrint

treeFull |> Tree.traverse [0; 8; 8; 8; 8; 8] |> Option.map Tree.prettyPrint

treeFull
|> Tree.tryFindPathBft ((Tree.foldValDft (+) 0) >> ((=) 31))
|> (function None -> printfn "NONE" | Some(s, t) -> printfn "%A" s; Tree.prettyPrint t)

/////

printfn "STARTED"
#time
let treeFull2 = [for i in 1 .. 23 -> 2] |> Tree.genFullTree (fun i s -> i, s) |> Option.get
#time
printfn "DONE"

treeFull2 |> Tree.traverse [for i in 1 .. 20 -> 0] |> Option.get |> Tree.prettyPrint

#time
treeFull2 |> Tree.foldValDft (fun s (t, u)-> s + (int64 t) + (int64 u)) (int64 0) |> printfn "%d"
#time
