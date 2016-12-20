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


let xDep, bLeaf, xCh, flg = 4, 2, 4, true
let f (i : int) (j : int) : int = i + j
let treeGen = Tree.genRandTree xDep bLeaf xCh flg f |> Option.get


//printfn "treeSeq ="; treeSeq |> Tree.prettyPrint
//printfn "treeRnd ="; treeRnd |> Tree.prettyPrint
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

/////

/////

//let genFullTree (f : int -> int -> 'A) (dlst : list<int>) : option<Tree.Node<'A>> =
//    let g i j k = ((i - 1) * k) + j
//    match dlst with
//    | [] -> None
//    | _ ->
//        ((1, 1), dlst) ||> List.scan (fun (s, t) u -> s * t, u)
//        |> List.tail |> List.mapi (fun i (s, t) -> i + 1, s, t) |> List.rev
//        |> (fun s ->
//            let t, u, v = List.head s
//            [for i in 1 .. u -> [for j in 1 .. v -> Tree.Node.Leaf(f t (g i j v))]], List.tail s
//        ) ||> List.fold (fun s (t, u, v) ->
////            t |> Seq.windowed s |> Seq.map (List.ofArray) |> List.ofSeq
////            |> List.zip [for i in 1 .. v do for j in 1 .. w -> f u (((i - 1) * w) + j)]
////            |> List.map (fun (x, y) -> Tree.Node.Internal(Some x, y))
////            |> (fun x -> w, x)
//            [for i in 1 .. u ->
//                [for j in 1 .. v ->
//                    f t (((i - 1) * v) + j)]]
//            |> ignore
//            []
//        ) |> (fun s -> Tree.Node.Internal(Some(f 0 1), List.collect id s) |> Some)

let genFullTree (f : int -> int -> 'A) (cd : #seq<int>) : option<Tree.Node<'A>> =
    let g i j k = ((i - 1) * k) + j
    if Seq.isEmpty cd then None
    else
        let cd' =
            (1, cd) ||> Seq.scan (fun s t -> s * t)
            |> Seq.mapi (fun i s -> i, s)
            |> (Seq.tail >> Seq.rev)
        (let s, t = cd' |> Seq.head in seq {for i in 1 .. t -> Tree.Node.Leaf(f s i)}, cd' |> Seq.tail)
        ||> Seq.fold (fun s (t, u) ->
            let v = (Seq.length s) / u
            Seq.empty
        )
        |> ignore
        None

/////

((1, 1, 1), seq [2; 4; 1; 3])
||> Seq.scan (fun (s, t, u) v -> let w = s * v in w, v, w / v)
|> Seq.mapi (fun i s -> i, s)
|> (Seq.tail >> Seq.rev)
|> (fun s ->
    let d, (n, m, p) = s |> Seq.head
    seq {for i in 1 .. p -> seq {for j in 1 .. m -> Tree.Node.Leaf(d, ((i - 1) * m) + j)}}
)
// |> Seq.iter (printfn "%A")
