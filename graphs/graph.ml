open Core.Result.Monad_infix

let (>>|) = Core.Option.(>>|)
let (|<<) = Core.Fn.flip (>>|)
let (<<) = Core.Fn.compose
let (>>) = Core.Fn.flip (<<)

module EdgeSet = Core.Set.Make(Core.Int)

module EdgeMap = Core.Map.Make(Core.Int)

module Graph = struct
    type t = EdgeSet.t EdgeMap.t

    exception Duplicate_vertex of EdgeMap.Key.t

    let empty =
        EdgeMap.empty

    let return graph : (t, string) result =
        Ok graph

    let add_vertex vertex graph : (t, string) result =
        match EdgeMap.mem graph vertex with
        | true  ->
            Error (Printf.sprintf "Duplicate vertex %d" vertex)
        | false ->
            Ok (EdgeMap.set ~key:vertex ~data:EdgeSet.empty graph)

    let has_edge (target : int) (source : int) (graph : t) : bool =
        EdgeMap.exists graph ~f:((Core.Fn.flip EdgeSet.mem) target)

    let add_edge target source graph : (t, string) result =
        match EdgeMap.mem graph target && EdgeMap.mem graph source with
        | true  ->
            let link left right graph =
                EdgeMap.change graph left
                    ~f:((|<<) @@ (Core.Fn.flip EdgeSet.add) right) in
            graph
            |> link target source
            |> link source target
            |> Core.Result.return
        | false -> Error "Missing vertex"

    let degree vertex graph =
        EdgeMap.find graph vertex
        >>| EdgeSet.length
end

module Graphviz = struct
    let of_graph (graph : Graph.t) =
        "graph {\n" ^ EdgeMap.fold graph ~init:"}\n" ~f:(fun ~key ~data ->
            (^) @@ EdgeSet.fold data
                ~init:""
                ~f:(fun acc -> (^) acc << (Printf.sprintf "\t_%d -- _%d\n" key)))
end

module Eulerian_path = struct
    let odd_vertices (graph : Graph.t) =
        EdgeMap.filter_keys graph ~f:(fun key ->
            (Graph.degree key graph
            >>| fun degree -> degree mod 2 <> 0)
            |> Core.Option.value ~default:false)
end

let x =
    let graph = Graph.return @@ Graph.empty
    >>= Graph.add_vertex 1
    >>= Graph.add_vertex 2
    >>= Graph.add_vertex 3
    >>= Graph.add_edge 1 2
    >>= Graph.add_edge 2 3
    >>= Graph.add_edge 3 1
    in
    match graph with
    | Ok graph ->
        Graphviz.of_graph graph
        |> print_endline
    | Error message -> print_endline message