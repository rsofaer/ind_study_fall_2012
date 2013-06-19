using Graphs

immutable WeightedEdge{V}
    index::Int
    resistance::Float64
    source::V
    target::V
end
import Graphs.edge_index, Graphs.target, Graphs.source
edge_index(e::WeightedEdge) = e.index
target(e::WeightedEdge) = e.target
source(e::WeightedEdge) = e.source

immutable AttrNode
    index::Int
    attrs::Dict
end
import Graphs.vertex_index
vertex_index(v::AttrNode) = v.index

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
import Graphs.add_edge!
function add_edge!(g::MyIncList, e::WeightedEdge{AttrNode})
    g.nedges += 1
    push!(g.inclist[vertex_index(e.source)], e)

    if !g.is_directed
        push!(g.inclist[vertex_index(e.target)], e)
    end
end
function add_edge!(g::MyIncList, i::Int, r::Float64, u::Int, v::Int)
    nv::Int = num_vertices(g)

    if !(u >= 1 && u <= nv && v >= 1 && v <= nv)
        throw(ArgumentError("u or v is not a valid vertex."))
    end
    edge = WeightedEdge(i, r, g.vertices[u], g.vertices[v])
    add_edge!(g, edge)
end
