# Primitives defined on page 5 of Spielman and Teng et. al. Lower Stretch Spanning Trees
#
using Graphs

import Base.length

function length(e::Edge)
	1/weight(e)
end

# A convenience function for applying an attribute to a 
# group of edges and/or vertices.
# Why can't I map a Set?
function apply_attribute(c, d::Dict)
	for target = c

		merge!(attributes(target),d)

	end
end

# Shortest path in LENGTH
function dist(a::Vertex, b::Vertex)
	Inf
end

# The subgraph induced by s.
# edges(subgraph(g,s)) is the set of edges with both endpoints in s.
function subgraph(g::AbstractGraph, s::Set{Vertex})
	return typeof(g)(s, filter(e -> length(intersect(ends(e),s)) == 2, edges(g)))
end

# The edges with exactly one end in s.
function boundary(g::AbstractGraph, s::Set{Vertex})
	return filter(e -> length(intersect(ends(e),s)) == 1,edges(g))
end

# The sum of the weights in s
cost(s::Set{Edge}) = reduce(+,map(x -> weight(x), s))

vol(s::Union(Set{Edge},Set{Vertex})) = length(s)

# The vertices of distance at most r from v (in LENGTH)
function ball(v::Vertex, r::Real)

end

# every vertex u not in B(v,r) with a neighbor w in B(v,r)
# such that dist(v,u) = dist(v,w) + the length of the edge from w to u
function ballshell(v::Vertex, r::Real)
end

