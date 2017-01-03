namespace MusterLib


open System


module Tree =


    [<RequireQualifiedAccess>]
    type Node<'A> =
        | Leaf of nodeVal : 'A
        | Internal of nodeVal : option<'A> * childNodes : list<Node<'A>>


    type Path = list<int>


    [<RequireQualifiedAccess>]
    type Zipper<'A> =
        | Start
        | Internal of option<'A> * list<Node<'A>> * list<Node<'A>> * Zipper<'A>


    let traverse (p : Path) (node : Node<'A>) : option<Node<'A>> =
        let f (i : int) (n : Node<'A>) : option<Node<'A>> =
            match n with
            | Node.Internal(_, c) when (List.length c) > i -> Some(c |> List.skip i |> List.head)
            | _ -> None
        match p with [] -> Some node | _ -> (Some node, p) ||> List.fold (fun s t -> Option.bind (f t) s)


    let mapi (f : Path -> 'A -> 'B) (node : Node<'A>) : Node<'B> =
        let rec helper (p : Path) (currNode : Node<'A>) : Node<'B> =
            match currNode with
            | Node.Leaf v -> Node.Leaf(f p v)
            | Node.Internal(v', c) -> Node.Internal(Option.map (f p) v', List.mapi (fun i s -> helper (i :: p) s) c)
        helper [] node


    let map (f : 'A -> 'B) (node : Node<'A>) : Node<'B> = let g _ s = f s in node |> mapi g


    let foldDfti (f : Path -> 'B -> Node<'A> -> 'B) (initVal : 'B) (node : Node<'A>) : 'B =
        let rec helper (currPath : Path) (currAcc : 'B) (currNode : Node<'A>) : 'B =
            match currNode with
            | Node.Leaf _ -> f currPath currAcc currNode
            | Node.Internal(_, c) ->
                (f currPath currAcc currNode, c |> List.mapi (fun i s -> i, s))
                ||> List.fold (fun s (t, u) -> helper (t :: currPath) s u)
        helper [] initVal node


    let foldDft (f : 'B -> Node<'A> -> 'B) (initVal : 'B) (node : Node<'A>) : 'B =
        let g _ s t = f s t in (initVal, node) ||> foldDfti g


    let foldValDfti (f : Path -> 'B -> 'A -> 'B) (initVal : 'B) (node : Node<'A>) : 'B =
        let g p s t = match t with Node.Leaf v | Node.Internal(Some v, _) -> f p s v | _ -> s
        (initVal, node) ||> foldDfti g


    let foldValDft (f : 'B -> 'A -> 'B) (initVal : 'B) (node : Node<'A>) : 'B =
        let g s t = match t with Node.Leaf v | Node.Internal(Some v, _) -> f s v | _ -> s
        (initVal, node) ||> foldDft g


    let accumulateValDft (f : 'A -> 'B) (node : Node<'A>) : list<'B> =
        let g s t = (f t) :: s in ([], node) ||> foldValDft g |> List.rev


    let tryFindPathDft (f : Node<'A> -> bool) (node : Node<'A>) : option<Path * Node<'A>> =
        let rec helper (acc : list<int * int * Node<'A>>) (pathAcc : list<int * int>) (currNode : Node<'A>) =
            match acc, pathAcc, currNode with
            | _ when f currNode -> Some(pathAcc |> (List.rev >> List.tail >> List.map snd), currNode)
            | _, (d', _) :: _, Node.Internal(_, c) ->
                let d = d' + 1 in
                helper ((c |> List.tail |> List.mapi (fun i s -> d, i + 1, s)) @ acc) ((d, 0) :: pathAcc) (List.head c)
            | (d, i, n) :: t, (d', _) :: _, Node.Leaf _ -> helper t ((d, i) :: (pathAcc |> List.skip (d' - d + 1))) n
            | _ -> None
        helper [] [0, 0] node


    let tryFindDft (f : Node<'A> -> bool) (node : Node<'A>) = node |> tryFindPathDft f |> Option.map snd


    let foldBfti (f : int -> int -> 'B -> Node<'A> -> 'B) (initVal : 'B) (node : Node<'A>) : 'B =
        let g = List.rev >> (List.collect (function Node.Leaf _ -> [] | Node.Internal(_, c) -> c))
        let rec helper currDepth (currAcc : 'B) (us : list<int * Node<'A>>) (ts : list<Node<'A>>) : 'B =
            match us, ts with
            | [], [] -> currAcc
            | [], _ -> helper (currDepth + 1) currAcc (ts |> g |> List.mapi (fun i s -> i, s)) []
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


    let tryFindPathBft (f : Node<'A> -> bool) (node : Node<'A>) : option<Path * Node<'A>> =
        let g (lst : list<int * int * int>) : int * int =
            let (p, d, i) = lst |> List.head
            lst |> List.takeWhile (fun (s, t, _) -> s = p && t = d)
            |> (List.rev >> List.head) |> (fun (_, _, s) -> p, i - s)
        let g' (lst : list<int * int * int>) : list<int> =
            (([], lst), [(lst |> List.head |> fun (_, s, _) -> s - 1) .. -1 .. 0])
            ||> List.fold (fun (s, t) u ->
                let p, i = g t in i :: s, t |> List.skipWhile (fun (_, d', i') -> i' <> p || d' <> u)
            ) |> fst
        let rec helper us ts acc : option<Path * Node<'A>> =
            match us, ts with
            | [], [] -> None
            | [], _ ->
                ((ts |> List.rev
                |> List.collect (function
                    | _, d, i, Node.Internal(_, c) -> c |> List.map (fun s -> i, d + 1, s)
                    | _ -> []
                )) |> List.mapi (fun j (p, d, n) -> p, d, j, n), [], acc) |||> helper
            | (p, d, i, n) :: t, _ ->
                let acc' = (p, d, i) :: acc in if f n then Some(g' acc', n) else helper t ((p, d, i, n) :: ts) acc'
        helper [-1, 0, 0, node] [] []


    let tryFindBft (f : Node<'A> -> bool) (node : Node<'A>) : option<Node<'A>> =
        let rec helper (us : list<Node<'A>>) (ts : list<Node<'A>>) : option<Node<'A>> =
            match us, ts with
            | [], [] -> None
            | [], _ -> helper (ts |> List.rev |> List.collect (function Node.Internal(_, c) -> c | _ -> [])) []
            | h :: t, _ -> if f h then Some h else helper t (h :: ts)
        helper [node] []


    let prettyPrint (node : Node<'A>) : unit =
        let c, d = " . ", 2
        let f p (s : list<int * int * string>) t =
            let i, j = match p with [] -> 0, 0 | _ -> List.length p, List.head p
            (i, j, match t with Node.Leaf v | Node.Internal(Some v, _) -> v.ToString() | _ -> "NULL") :: s
        ([], node) ||> foldDfti f
        |> List.map (fun (s, _, t) -> String.concat "" [for i in 0 .. (d * s) - 1 -> c] + t)
        |> (List.rev >> (String.concat Environment.NewLine))
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


    let genFullTree (f : int -> int -> 'A) (cd : list<int>) : Node<'A> =
        if List.isEmpty cd then Node.Leaf(f 0 0)
        else
            let rec g a c l = match l with [] -> List.rev a | _ -> g ((List.take c l) :: a) c (List.skip c l)
            ((1, 0), cd) ||> List.scan (fun (s, _) t -> s * t, t) |> List.mapi (fun i s -> i, s)
            |> (List.tail >> List.rev)
            |> (fun s ->
                let d, (n, m) = s |> List.head
                ([for i in 0 .. (n / m) - 1 -> [for j in 0 .. m - 1 -> Node.Leaf(f d (i * m + j))]], s |> List.tail)
                ||> List.fold (fun t (u, (_, v)) -> g [] v (List.mapi (fun i w -> Node.Internal(Some(f u i), w)) t))
                |> (fun t -> Node.Internal(Some(f 0 0), t |> List.collect id)))


    let modify (path : Path) (mf : Node<'A> -> 'B -> Node<'A>) (prms : 'B) (node : Node<'A>) : option<Node<'A>> =
        let f (s : int) (t : Node<'A>) =
            t |> traverse [s]
            |> Option.bind (fun u ->
                match t with
                | Node.Internal(v', c) -> Some(v', List.take s c, u, List.skip (s + 1) c)
                | _ -> None
            )
        let unzip () : option<Zipper<'A> * Node<'A>> =
            (Some(Zipper.Start, node), path)
            ||> List.fold (fun s' t ->
                s' |> Option.bind (fun (s1, s2) ->
                        s2 |> f t |> Option.map (fun (u', v, w, x) ->  Zipper.Internal(u', v, x, s1), w))
            ) |> Option.map (fun (s, t) -> s, mf t prms)
        let rec zip (currZipper : Zipper<'A>, currNode : Node<'A>) : Node<'A> =
            match currZipper with
            | Zipper.Internal(s, t, u, v) -> (v, Node.Internal(s, t @ [currNode] @ u)) |> zip
            | Zipper.Start -> currNode
        () |> unzip |> Option.map zip


    let removeAt (path : Path) (node : Node<'A>) : option<Node<'A>> =
        let mf (n : Node<'A>) (i : int) : Node<'A> =
            match n with
            | Node.Internal(v', c) when (List.length c) > i ->
                let s = let t, u = List.splitAt i c in t @ (List.tail u)
                match s, v' with | h :: [], Some v -> Node.Leaf v | h :: _, _ -> Node.Internal(v', s) | _ -> n
            | _ -> n
        match path with
        | [] -> None
        | _ -> let s, t = (let u = List.rev path in List.head u, u |> List.tail |> List.rev) in modify t mf s node

