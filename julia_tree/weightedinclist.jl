using Graphs
import Base.show
immutable WeightedEdge{V}
    index::Int
    resistance::Float64
    source::V
    target::V
end
import Graphs.edge_index, Graphs.target, Graphs.source
edge_index(e::WeightedEdge) = e.index
Base.isless(v1::WeightedEdge, v2::WeightedEdge) = isless(edge_index(v1), edge_index(v2))
target(e::WeightedEdge ) = e.target
source(e::WeightedEdge ) = e.source
target(e::WeightedEdge,g ) = e.target
source(e::WeightedEdge,g ) = e.source

typealias AttrDict Dict{UTF8String, Any}
immutable AttrNode
    index::Int
    attrs::AttrDict
end

Base.isless(v1::AttrNode, v2::AttrNode) = isless(vertex_index(v1), vertex_index(v2))
import Graphs.vertex_index
vertex_index(v::AttrNode) = v.index

attrs(v::AttrNode) = v.attrs

show(v::AttrNode) = "Node $(vertex_index(v))"
typealias WIncidenceList{V} GenericIncidenceList{V, WeightedEdge{V}, Vector{V}, Vector{Vector{WeightedEdge{V}}}}

typealias MyIncList WIncidenceList{AttrNode}

function weightedinclist()
    MyIncList(false, Array(AttrNode, 0), 0, Array(Vector{WeightedEdge{AttrNode}},0))
end

import Graphs.add_vertex!
function add_vertex!{V}(g::WIncidenceList{V}, v::V)
    nv::Int = num_vertices(g)
    iv::Int = vertex_index(v)
    if iv != nv + 1
        throw(ArgumentError("Invalid vertex index."))
    end

    push!(g.vertices, v)
    push!(g.inclist, Array(edge_type(g),0))
    v
end

function add_vertex!{V}(g::WIncidenceList{V}, d::AttrDict)
    nv::Int = num_vertices(g)
    v = AttrNode(nv + 1, d)
    add_vertex!(g, v)
end

import Graphs.add_edge!
function add_edge!(g::MyIncList, e::WeightedEdge{AttrNode})
    g.nedges += 1
    push!(g.inclist[vertex_index(e.source)], e)

    if !g.is_directed
        push!(g.inclist[vertex_index(e.target)], WeightedEdge{AttrNode}(e.index, e.resistance, e.target, e.source))
    end
end
function add_edge!(g::MyIncList, i::Integer, r::Float64, u::Integer, v::Integer)
    nv::Int = num_vertices(g)

    if !(u >= 1 && u <= nv && v >= 1 && v <= nv)
        throw(ArgumentError("u or v is not a valid vertex."))
    end
    edge = WeightedEdge(i, r, g.vertices[u], g.vertices[v])
    add_edge!(g, edge)
end

function add_edge!(g::MyIncList, u::Integer, v::Integer, r::Float64)
    nv::Int = num_vertices(g)

    if !(u >= 1 && u <= nv && v >= 1 && v <= nv)
        throw(ArgumentError("u or v is not a valid vertex."))
    end
    edge = WeightedEdge(num_edges(g) + 1, r, g.vertices[u], g.vertices[v])
    add_edge!(g, edge)
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


function edgedists(g)
    result = Array(Float64, 0)
    for e in edges(g)
        push!(result, resistance(e))
    end
    return result
end
import Base.show
show(io::IO, v::AttrNode) = print(io, "Vertex($(vertex_index(v)))")
show(io::IO, e::WeightedEdge) = print(io, 
    "Edge($(edge_index(e)), $(vertex_index(source(e))) -> $(vertex_index(target(e))))")