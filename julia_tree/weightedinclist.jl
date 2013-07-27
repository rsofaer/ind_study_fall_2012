using Graphs

import Base.show
import  Graphs.implements_vertex_list,
        Graphs.implements_edge_list,
        Graphs.implements_vertex_map,
        Graphs.implements_edge_map,
        Graphs.implements_adjacency_list,
        Graphs.implements_incidence_list,
        Graphs.implements_bidirectional_adjacency_list,
        Graphs.implements_bidirectional_incidence_list,
        Graphs.implements_adjacency_matrix

type GraphAttributes
    vertex_attributes::Vector{AttributeDict}
    edge_attributes::Vector{AttributeDict}
    graph_attributes::AttributeDict
end
GraphAttributes(nv) = GraphAttributes(Array(AttributeDict, nv), AttributeDict[], AttributeDict())

type ExGenericIncidenceList{V, E, VList, IncList} <: AbstractGraph{V, E}
    is_directed::Bool    
    vertices::VList
    nedges::Int
    inclist::IncList
    attributes::GraphAttributes
    edge_resistances
end

# required interfaces
@graph_implements ExGenericIncidenceList vertex_list vertex_map edge_map adjacency_list incidence_list

Graphs.is_directed(g::ExGenericIncidenceList) = g.is_directed

Graphs.num_vertices(g::ExGenericIncidenceList) = length(g.vertices)
Graphs.vertices(g::ExGenericIncidenceList) = g.vertices

Graphs.num_edges(g::ExGenericIncidenceList) = g.nedges

Graphs.vertex_index(v, g::ExGenericIncidenceList) = vertex_index(v)
Graphs.edge_index(e, g::ExGenericIncidenceList) = edge_index(e)

Graphs.out_degree(v, g::ExGenericIncidenceList) = length(g.inclist[vertex_index(v)])
Graphs.out_edges(v, g::ExGenericIncidenceList) = g.inclist[vertex_index(v)]

Graphs.out_neighbors(v, g::ExGenericIncidenceList) = out_neighbors_proxy(g.inclist[vertex_index(v)])

typealias ExSimpleIncidenceList ExGenericIncidenceList{Int, Edge{Int}, Range1{Int}, Vector{Vector{Edge{Int}}}}
typealias MyIncList ExSimpleIncidenceList

function exsimple_inclist(nv::Int; is_directed::Bool = true)
    inclist = Array(Vector{Edge{Int}}, nv)    
    for i = 1 : nv
        inclist[i] = Array(Edge{Int}, 0)
    end
    ExSimpleIncidenceList(is_directed, 1:nv, 0, inclist, GraphAttributes(nv), Float64[])
end

weightedinclist = exsimple_inclist

import Graphs.edge_index, Graphs.target, Graphs.source
Base.isless(v1::Edge{Int}, v2::Edge{Int}) = isless(edge_index(v1), edge_index(v2))

resistance{V,E}(e::E, g::ExGenericIncidenceList{V,E}) = g.edge_resistances[edge_index(e, g)]
conductance{V,E}(e::E, g::ExGenericIncidenceList{V,E}) = 1/resistance(e, g)

Graphs.attributes{V}(v::V, g::ExGenericIncidenceList{V}) = g.vertex_attributes[vertex_index(v, g)]

function set_attributes!{V}(g::ExSimpleIncidenceList{V},v::V, d::AttributeDict)
    g.attributes.vertex_attributes[vertex_index(v, g)] = d
end

import Graphs.add_edge!

add_edge!{V,E}(g::ExGenericIncidenceList{V, E}, u::V, v::V, r::Float64) = add_edge!(g, u, v, r, AttributeDict())
function add_edge!{V,E}(g::ExGenericIncidenceList{V, E}, u::V, v::V, r::Float64, d::AttributeDict)
    nv::Int = num_vertices(g)
    ui::Int = vertex_index(u)
    vi::Int = vertex_index(v)
    
    if !(ui >= 1 && ui <= nv && vi >= 1 && vi <= nv)
        throw(ArgumentError("u or v is not a valid vertex."))
    end
    ei::Int = (g.nedges += 1)

    e = E(ei, u, v)
    push!(g.edge_resistances, r)

    push!(g.inclist[ui], e)

    if !g.is_directed
        push!(g.inclist[vi], revedge(e))
    end
end

function resistance_matrix(g::MyIncList)
    n::Int = num_vertices(g)
    a = zeros(n, n)
    a = 1./a
    for u in vertices(g)
        ui = vertex_index(u, g)
        for e in out_edges(u, g)
            vi = vertex_index(target(e, g), g)
            a[ui, vi] = e.resistance
        end
    end    
    return a
end

function Graphs.edges(graph::MyIncList)
    es = Array(edge_type(graph),num_edges(graph))
    for vertex in vertices(graph)
        for e in out_edges(vertex, graph)
            es[edge_index(e)] = e
        end
    end
    return es
end


edgedists(g) = g.edge_resistances

import Base.show
show(io::IO, e::WeightedEdge) = print(io, 
    "Edge($(edge_index(e)), $(vertex_index(source(e))) -> $(vertex_index(target(e))))")