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


let treeGen =
    let xDep, bLeaf, xCh, flg = 4, 2, 4, true
    let f (i : int) (j : int) = i, j
    Tree.genRandTree xDep bLeaf xCh flg f |> Option.get


let treeFull = [2; 3; 3; 2] |> Tree.genFullTree (fun d i -> (pown 2 d) + i) |> Option.get


printfn "FINISHED LOADING TREES"

// treeSeq |> Tree.prettyPrint

//printfn "%d" (treeSeq |> Tree.foldValDft (+) 0)
//
//printfn "%A" (treeSeq |> Tree.accumulateValDft id)

//([], treeSeq)
//||> Tree.foldValDfti (fun d i s t -> (d, i, t) :: s)
//|> List.rev
//|> List.iter (printfn "%A")


//printfn "%A" (
//    let f = (<) 4
//    let g = function Tree.Node.Leaf v -> f v | Tree.Node.Internal(v', _) -> Option.exists f v'
//    (treeSeq |> Tree.Node.tryFindDft g, treeSeq |> Tree.Node.tryFindBft g)
//)

//printfn "%A" (treeSeq |> Tree.foldValBft (+) 0)
//
//printfn "%A" (treeSeq |> Tree.accumulateValBft id)
//
//([], treeSeq)
//||> Tree.foldValBfti (fun d i s t -> (d, i, t) :: s)
//|> List.rev
//|> List.iter (printfn "%A")

//printfn "%A" (
//    let f = (<) 4
//    let g = function Tree.Node.Leaf v -> f v | Tree.Node.Internal(v', _) -> Option.exists f v'
//    treeRnd |> Tree.Node.tryFindBft g
//)

/////

//let genFullTree (f : int -> int -> 'A) (cd : list<int>) : option<Tree.Node<'A>> =
//    if List.isEmpty cd then None
//    else
//        let rec g a c l = match l with [] -> List.rev a | _ -> g ((List.take c l) :: a) c (List.skip c l)
//        ((1, 1), cd) ||> List.scan (fun (s, _) t -> s * t, t) |> List.mapi (fun i s -> i, s)
//        |> (List.tail >> List.rev)
//        |> (fun s ->
//            let d, (n, m) = s |> List.head
//            ([for i in 0 .. (n / m) - 1 -> [for j in 0 .. m - 1 -> Tree.Node.Leaf(f d (i * m + j))]], s |> List.tail)
//            ||> List.fold (fun t (u, (_, v)) -> g [] v (List.mapi (fun i w -> Tree.Node.Internal(Some(f u i), w)) t))
//            |> (fun t -> Some(Tree.Node.Internal(Some(f 0 0), t |> List.collect id)))
//        )
//
//printfn "LOADED genFullTree"

/////

//((1, 1), [2; 4; 1; 3])
//||> List.scan (fun (s, _) t -> s * t, t) |> List.mapi (fun i s -> i, s)
//|> (List.tail >> List.rev)
//|> (fun s ->
//    let rec g a c l = match l with [] -> List.rev a | _ -> g ((List.take c l) :: a) c (List.skip c l)
//    let d, (n, m) = s |> List.head
//    ([for i in 0 .. (n / m) - 1 -> [for j in 0 .. m - 1 -> Tree.Node.Leaf(d, (i * m) + j)]], s |> List.tail)
//    ||> List.fold (fun t (u, (_, v)) -> g [] v (List.mapi (fun i w -> Tree.Node.Internal(Some(u, i), w)) t))
//    |> (fun t -> Some(Tree.Node.Internal(Some(0, 0), t |> List.collect id)))
//) |> Option.get |> Tree.prettyPrint

/////

//[2; 4; 1; 3]
//|> genFullTree (fun i j -> i, j)
//|> Option.get
//|> Tree.prettyPrint

/////

[<RequireQualifiedAccess>]
type Zipper<'A> =
    | Start
    | Internal of Tree.Node<'A> * int * Zipper<'A>
    | End of Tree.Node<'A> * Zipper<'A>
    static member apply
        (node : Tree.Node<'A>)
        (path : list<int>)
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

treeFull
|> Tree.tryFindPathBft ((Tree.foldValDft (+) 0) >> ((=) 31))
|> (function None -> printfn "NONE" | Some(s, t) -> printfn "%A" s; Tree.prettyPrint t)

/////

let lst =
    [(-1, 0, 0);
    (0, 1, 0); (0, 1, 1);
    (0, 2, 0); (0, 2, 1); (1, 2, 2); (1, 2, 3);
    (0, 3, 0); (0, 3, 1); (1, 3, 2); (1, 3, 3); (2, 3, 4); (2, 3, 5)]
    |> List.rev

let g (lst : list<int * int * int>) : int * int =
    let (p, d, i) = lst |> List.head
    lst |> List.takeWhile (fun (s, t, _) -> s = p && t = d)
    |> (List.rev >> List.head) |> (fun (_, _, s) -> p, i - s)

let g' (lst : list<int * int * int>) : list<int> =
    (([], lst), [(lst |> List.head |> fun (_, s, _) -> s - 1) .. -1 .. 0])
    ||> List.fold (fun (s, t) u ->
        let p, i = g t in i :: s, t |> List.skipWhile (fun (_, d', i') -> i' <> p || d' <> u)
    ) |> fst

lst |> g' |> printfn "%A"

/////
