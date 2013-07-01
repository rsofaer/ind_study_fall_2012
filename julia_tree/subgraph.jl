using Graphs
import  Graphs.implements_vertex_list,
        Graphs.implements_edge_list,
        Graphs.implements_vertex_map,
        Graphs.implements_edge_map,
        Graphs.implements_adjacency_list,
        Graphs.implements_incidence_list,
        Graphs.implements_bidirectional_adjacency_list,
        Graphs.implements_bidirectional_incidence_list,
        Graphs.implements_adjacency_matrix
type Subgraph{V, E} <: AbstractGraph{V, E}
  full_graph::AbstractGraph{V,E}
  vertex_set::IntSet
end

@graph_implements Subgraph vertex_list vertex_map edge_map adjacency_list incidence_list

function subgraph{V,E}(g::AbstractGraph{V, E}, s)
  i = IntSet()
  for v in s
    add!(i, vertex_index(v, g))
  end
  Subgraph{V,E}(g, i)
end

Graphs.num_vertices(g::Subgraph) = length(g.vertex_set)
Graphs.num_edges(g::Subgraph) = num_vertices(g.full_graph)
Graphs.is_directed(g::Subgraph) = is_directed(g.full_graph)
Graphs.out_edges{V, E}(v::V, g::Subgraph{V, E}) = filter(x -> valid_edge(g, x), out_edges(v, g.full_graph))
Graphs.out_neighbors{V, E}(v::V, g::Subgraph{V, E}) = filter(x -> valid_vertex(g, x), out_neighbors(v, g.full_graph))
Graphs.vertex_index{V}(v::V, g::Subgraph{V}) = searchsortedfirst(vertices(g), v)
Graphs.edge_index{V, E}(e::E, g::Subgraph{V, E}) = searchsortedfirst(edges(g), e)

valid_vertex(g::Subgraph, v) = contains(g.vertex_set, vertex_index(v, g.full_graph))
valid_edge(g::Subgraph, e) = contains(g.vertex_set, vertex_index(target(e), g.full_graph)) && 
                              contains(g.vertex_set, vertex_index(source(e), g.full_graph))

function Graphs.vertices(g::Subgraph)
  filter(x -> valid_vertex(g, x), vertices(g.full_graph))
end

function Graphs.edges(g::Subgraph)
  filter(x -> valid_edge(g, x), edges(g.full_graph))
end
