using Graphs

immutable WeightedEdge{V}
    index::Int
    resistance::Float64
    source::V
    target::V
end

typealias WIncidenceList{V} GenericIncidenceList{V, WeightedEdge{V}, Vector{V}, Vector{Vector{Edge{V}}}}

typealias MyIncList WIncidenceList{Int}
