namespace MusterLib


open System


module Tree =


    [<RequireQualifiedAccess>]
    type Node<'A> =
        | Leaf of 'A
        | Internal of option<'A> * list<Node<'A>>


    let map (f : 'A -> 'B) (node : Node<'A>) : Node<'B> =
        let rec helper (currNode : Node<'A>) : Node<'B> =
            match currNode with
            | Node.Leaf v -> Node.Leaf(f v)
            | Node.Internal(v', c) -> Node.Internal(Option.map f v', c |> List.map helper)
        helper node


    let foldDfti (f : int -> int -> 'B -> Node<'A> -> 'B) (initVal : 'B) (node : Node<'A>) : 'B =
        let rec helper currDepth sibNum (currAcc : 'B) (currNode : Node<'A>) : 'B =
            match currNode with
            | Node.Leaf _ -> f currDepth sibNum currAcc currNode
            | Node.Internal(_, c) ->
                (f currDepth sibNum currAcc currNode, c |> List.mapi (fun i s -> i, s))
                ||> List.fold (fun s (t, u) -> helper (currDepth + 1) t s u)
        helper 0 0 initVal node


    let foldDft (f : 'B -> Node<'A> -> 'B) (initVal : 'B) (node : Node<'A>) : 'B =
        let g _ _ s t = f s t in (initVal, node) ||> foldDfti g


    let foldValDfti (f : int -> int -> 'B -> 'A -> 'B) (initVal : 'B) (node : Node<'A>) : 'B =
        let g i j s t = match t with Node.Leaf v | Node.Internal(Some v, _) -> f i j s v | _ -> s
        (initVal, node) ||> foldDfti g


    let foldValDft (f : 'B -> 'A -> 'B) (initVal : 'B) (node : Node<'A>) : 'B =
        let g s t = match t with Node.Leaf v | Node.Internal(Some v, _) -> f s v | _ -> s
        (initVal, node) ||> foldDft g


    let accumulateValDft (f : 'A -> 'B) (node : Node<'A>) : list<'B> =
        let g s t = (f t) :: s in ([], node) ||> foldValDft g |> List.rev


    let tryFindDft (f : Node<'A> -> bool) (node : Node<'A>) : option<Node<'A>> =
        let rec helper (currAcc : list<Node<'A>>) (currNode : Node<'A>) : option<Node<'A>> =
            match currAcc, currNode with
            | _ when f currNode -> Some currNode
            | _, Node.Internal(_, c) -> helper ((List.tail c) @ currAcc) (List.head c)
            | h :: t, Node.Leaf _ -> helper t h
            | _ -> None
        helper [] node


    let foldBfti (f : int -> int -> 'B -> Node<'A> -> 'B) (initVal : 'B) (node : Node<'A>) : 'B =
        let rec helper currDepth (currAcc : 'B) (us : list<int * Node<'A>>) (ts : list<Node<'A>>) : 'B =
            match us, ts with
            | [], [] -> currAcc
            | [], _ ->
                helper
                    (currDepth + 1)
                    currAcc
                    (ts |> List.rev
                    |> List.collect (function Node.Leaf _ -> [] | Node.Internal(_, c) -> c)
                    |> List.mapi (fun i s -> i, s))
                    []
            | (h1, h1') :: t, _ -> helper currDepth (f currDepth h1 currAcc h1') t (h1' :: ts)
        helper 0 initVal [0, node] []


    let foldBft (f : 'B -> Node<'A> -> 'B) (initVal : 'B) (node : Node<'A>) : 'B =
        let g _ _ s t = f s t in (initVal, node) ||> foldBfti g


    let foldValBfti (f : int -> int -> 'B -> 'A -> 'B) (initVal : 'B) (node : Node<'A>) : 'B =
        let g i j s t = match t with Node.Leaf v | Node.Internal(Some v, _) -> f i j s v | _ -> s
        (initVal, node) ||> foldBfti g


    let foldValBft (f : 'B -> 'A -> 'B) (initVal : 'B) (node : Node<'A>) : 'B =
        let g s t = match t with Node.Leaf v | Node.Internal(Some v, _) -> f s v | _ -> s
        (initVal, node) ||> foldBft g


    let accumulateValBft (f : 'A -> 'B) (node : Node<'A>) : list<'B> =
        let g s t = (f t) :: s in ([], node) ||> foldValBft g |> List.rev


    let tryFindBft (f : Node<'A> -> bool) (node : Node<'A>) : option<Node<'A>> =
        let rec helper (us : list<Node<'A>>) (ts : list<Node<'A>>) : option<Node<'A>> =
            match us, ts with
            | [], [] -> None
            | [], _ ->
                helper (ts |> List.rev |> List.collect (function Node.Leaf _ -> [] | Node.Internal(_, c) -> c)) []
            | h :: t, _ -> if f h then Some h else helper t (h :: ts)
        helper [node] []


    let prettyPrint (node : Node<'A>) : unit =
        let c, d = " . ", 2
        let f i j (s : list<int * int * string>) t =
            (i, j, match t with Node.Leaf v | Node.Internal(Some v, _) -> v.ToString() | _ -> " ") :: s
        ([], node) ||> foldDfti f
        |> List.map (fun (s, _, t) -> String.concat "" [for i in 0 .. (d * s) - 1 -> c] + t)
        |> (List.rev >> (String.concat System.Environment.NewLine))
        |> printfn "%s"


    let genRandTree (xDep : int) (bLeaf : int) (xCh : int) (flg : bool) (f : int -> int -> 'A) : option<Node<'A>> =
        let rnd, vb = Random(), 8
        let rec helper (i : int) (j : int) : Node<'A> =
            match (i < xDep), ((rnd.Next() % 10) < bLeaf) with
            | true, false ->
                Node.Internal(
                    (if (rnd.Next() % 10 < vb) then Some(f i j) else None),
                    [for k in 1 .. (if flg then (1 + rnd.Next() % xCh) else xCh) -> helper (i + 1) k])
            | _ -> Node.Leaf(f i j)
        if xDep >= 0 then Some(helper 0 0) else None


    let genFullTree (f : int -> int -> 'A) (cd : list<int>) : option<Node<'A>> =
        if List.isEmpty cd then None
        else
            let rec g a c l = match l with [] -> List.rev a | _ -> g ((List.take c l) :: a) c (List.skip c l)
            ((1, 1), cd) ||> List.scan (fun (s, _) t -> s * t, t) |> List.mapi (fun i s -> i, s)
            |> (List.tail >> List.rev)
            |> (fun s ->
                let d, (n, m) = s |> List.head
                ([for i in 0 .. (n / m) - 1 -> [for j in 0 .. m - 1 -> Node.Leaf(f d (i * m + j))]], s |> List.tail)
                ||> List.fold (fun t (u, (_, v)) -> g [] v (List.mapi (fun i w -> Node.Internal(Some(f u i), w)) t))
                |> (fun t -> Some(Node.Internal(Some(f 0 0), t |> List.collect id))))

