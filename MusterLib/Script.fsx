// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "MapExtensions.fs"
#load "ListExtensions.fs"
#load "StringExtensions.fs"
#load "CSV.fs"
#load "Tree.fs"

open MusterLib.MapExtensions
open MusterLib.StringExtensions
open MusterLib.Tree
open MusterLib

/////

printfn "Initialisation complete"

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

printfn "BUILDING FULL TREE"
let treeFull = [2; 3; 3; 2] |> genFullTree (fun d i -> (pown 2 d) + i)
printfn "DONE"

treeFull |> prettyPrint

treeFull |> traverse [0; 2] |> Option.map prettyPrint

treeFull
|> removeChild [0; 1]
|> Option.bind (appendChild [0] (Node.Leaf (-1)))
|> Option.map prettyPrint

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
|> tryFindPathBft ((foldValDft (+) 0) >> ((=) 76))
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

/////

let str = "GFEFEEFGHFFEGCHFDFBCAGGGBBGDGADDEDFBDBAGCDAAHFDGDEHCGCDHGGDDGCHG"
let str2 = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let str3 = "ABABABABABABABABZABABABABABABABAB"
let str4 = "AAAAAABBB"
let str5 = "AAAAABBBCCCCCDDD"

(List.iter (printfn "%s") (getAllLongestSubstrings 2 false str5)) // ["AAAAABBB"; "BBBCCCCC"; "CCCCCDDD"]
getAllLongestSubstrings 3 true str5 // ["AAAAABBBCCCCC"]
(List.iter (printfn "%s")) (getAllLongestSubstrings 3 false str5) // ["AAAAABBBCCCCC"]
getAllLongestSubstrings 4 true str5 // ["AAAAABBBCCCCCDDD"]
getAllLongestSubstrings 5 true str5 // ["AAAAABBBCCCCCDDD"]
(List.iter (printfn "%s") (getAllLongestSubstrings 5 false str5)) // []

StringExtensions.getPronounceableWord (System.Random()) 2 5
|> printfn "%s"

/////

let m0 = [for i in 1 .. 5 -> i, i + 1] |> Map.ofList
let m1 = [for i in 2 .. 6 -> i, 2 * i] |> Map.ofList

let f (i' : option<int>) (j' : option<int>) : list<int> =
    match i', j' with
    | Some i, Some j -> [i; j]
    | Some i, None -> [i]
    | None, Some j -> [j]
    | _ -> []

m0 |> MapExtensions.mergeWith m1 f
|> printfn "%A"
