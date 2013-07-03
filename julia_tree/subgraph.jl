using Graphs
using Memoize

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
  edge_set::IntSet
end

@graph_implements Subgraph vertex_list vertex_map edge_list edge_map adjacency_list incidence_list

function subgraph{V,E}(g::AbstractGraph{V, E}, vertex_set)
  i = IntSet()
  for v in vertex_set
    add!(i, vertex_index(v, g))
  end

  j = IntSet()
  for e in edges(g)
    if contains(i, vertex_index(target(e), g)) && 
        contains(i, vertex_index(source(e), g))
      add!(j, edge_index(e, g))
    end
  end
  
  Subgraph{V,E}(g, i, j)
end

function subgraph{V, E}(g::Subgraph{V, E}, vertex_set)
  i = IntSet()
  for v in vertex_set
    if included_vertex(g, v)
      add!(i, vertex_index(v, g.full_graph))
    else
      error("Trying to make a subgraph of subgraph $g with vertices not in $g.
        $vertex_set
        $(vertices(g))")
    end
  end

  j = IntSet()
  for e in edges(g)
    if contains(i, vertex_index(target(e), g.full_graph)) && 
        contains(i, vertex_index(source(e), g.full_graph))
      add!(j, edge_index(e, g.full_graph))
    end
  end
  
  Subgraph{V,E}(g.full_graph, i, j)
end

Graphs.num_vertices(g::Subgraph) = length(g.vertex_set)
Graphs.num_edges(g::Subgraph) = length(g.edge_set)
Graphs.is_directed(g::Subgraph) = is_directed(g.full_graph)
Graphs.out_edges{V, E}(v::V, g::Subgraph{V, E}) = filter(x -> included_edge(g, x), out_edges(v, g.full_graph))
Graphs.out_neighbors{V, E}(v::V, g::Subgraph{V, E}) = filter(x -> included_vertex(g, x), out_neighbors(v, g.full_graph))
Graphs.vertex_index{V}(v::V, g::Subgraph{V}) = searchsortedfirst(vertices(g), v)
Graphs.edge_index{V, E}(e::E, g::Subgraph{V, E}) = searchsortedfirst(edges(g), e)

included_vertex(g::Subgraph, v) = contains(g.vertex_set, vertex_index(v, g.full_graph))
included_edge(g::Subgraph, v) = contains(g.edge_set, edge_index(v, g.full_graph))

full_graph(g::AbstractGraph) = g
full_graph(g::Subgraph) = g.full_graph

function Graphs.vertices(g::Subgraph)
  filter(x -> included_vertex(g, x), vertices(g.full_graph))
end

function Graphs.edges(g::Subgraph)
  filter(x -> included_edge(g, x), edges(g.full_graph))
end

function combine(g::Subgraph, h::Subgraph)
  if g.full_graph != h.full_graph
    error("Subgraphs $g and $h are not subgraphs of the same graph.")
  end
  return Subgraph(g.full_graph, union(g.vertex_set, h.vertex_set), union(g.edge_set, h.edge_set))
end

function add_edges{V,E}(g::Subgraph{V,E}, s::Vector{E})
  result = Subgraph(g.full_graph, copy(g.vertex_set), copy(g.edge_set))
  for e in s
    if contains(g.vertex_set, vertex_index(target(e), g.full_graph)) && 
        contains(g.vertex_set, vertex_index(source(e), g.full_graph))
      add!(result.edge_set, edge_index(e, g.full_graph))
    else
      error("edge $e is not a valid edge for subgraph $g.
        vertices: $(vertices(g))
        target $(target(e)) included: $(contains(g.vertex_set, vertex_index(target(e), g.full_graph)))
        source $(source(e)) included: $(contains(g.vertex_set, vertex_index(source(e), g.full_graph)))")
    end
  end
  return result
end