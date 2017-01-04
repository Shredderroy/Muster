// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "ListExtensions.fs"
#load "CSV.fs"
#load "Tree.fs"

open MusterLib.Tree

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

let treeFull = [2; 3; 3; 2] |> genFullTree (fun d i -> (pown 2 d) + i)

printfn "FINISHED LOADING TREES"

treeFull |> prettyPrint

// treeFull |> traverse [0; 2] |> Option.map prettyPrint

treeFull |> removeChild [0; 1; 2] |> Option.map prettyPrint

/////

treeFull |> modify [0] (fun _ _ -> Node.Leaf (-1)) 0 |> Option.map prettyPrint

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

treeFull
|> tryFindPathBft ((foldValDft (+) 0) >> ((=) 31))
|> (function None -> printfn "NONE" | Some(s, t) -> printfn "%A" s; prettyPrint t)

/////

//printfn "STARTED"
//#time
//let treeFull2 = [for i in 1 .. 23 -> 2] |> genFullTree (fun i s -> i, s)
//#time
//printfn "DONE"
//
//treeFull2 |> traverse [for i in 1 .. 20 -> 0] |> Option.get |> prettyPrint
//
//#time
//treeFull2 |> foldValDft (fun s (t, u)-> s + (int64 t) + (int64 u)) (int64 0) |> printfn "%d"
//#time
