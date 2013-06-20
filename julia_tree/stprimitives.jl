# Primitives defined on page 5 of Spielman and Teng et. al. Lower Stretch Spanning Trees
#
using Graphs
using DataStructures

import Base.length

function resistance{E}(e::E)
  e.resistance
end

function conductance{E}(e::E)
  1/e.resistance
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
function dist{V}(a::V, b::V)
	Inf
end

# The subgraph induced by s.
# edges(subgraph(g,s)) is the set of edges with both endpoints in s.
# function subgraph{V, E}(g::AbstractGraph, s::Set{V})
#  	return typeof(g)(s, filter(e -> length(intersect(ends(e),s)) == 2, edges(g)))
# end

# The edges with exactly one end in s.
function boundary{V,E}(s::Set{V}, g::AbstractGraph{V,E})

	return filter(e -> length(intersect(ends(e),s)) == 1,edges(g))
end

# The sum of the weights in s
cost(s::Set{Edge}) = reduce(+,map(x -> conductance(x), s))

vol{E,V}(s::Union(Set{E},Set{V})) = length(s)

# The vertices of distance at most r from v (in LENGTH)
function ball{V,E}(v::V, r::Real, g::AbstractGraph{V,E}, weights)
	shortest = dijkstra_shortest_paths(g,weights, v)
end

# every vertex u not in B(v,r) with a neighbor w in B(v,r)
# such that dist(v,u) = dist(v,w) + the length of the edge from w to u
function ballshell{V,E}(v::V, r::Real, g::AbstractGraph{V,E})

end

# the smallest rho st dist(x,y) < rho for all y
function radius{V,E}(g::AbstractGraph{V,E}, x::V)
	ed = edgedists(g)
	max(dijkstra_shortest_paths(g, ed, x).dists)
end

# Contract any edge with a length less than l
function contract{V,E}(g::AbstractGraph{V,E}, l::Real)
	vertexsets = DisjointSets{V}(vertices(g))
	for e in edges(g)
		if resistance(e) < l
			union!(vertexsets, e.source, e.target)
		end
	end
	vertexsets

	c_g = weightedinclist()
	# groupnumbers is an array,
	# groupnumbers[i] is the index of the image of vertex i in contracted g
	groupnumbers = vertexsets.internal.parents


	preimages = Dict{V,Vector{V}}()
	for n in 1:num_vertices(g)
	
	end

	

end


function LowStretchTree{V,E}(g::AbstractGraph{V,E}, x::V, original_num_vertices)
	beta = 1/(2*log(4/3,original_num_vertices + 32))

	if(num_vertices(g) <= 2)
		return g
	end

	rho = radius(g,x) 

	contracted_g, vertex_preimages = contract(g,beta*rho/original_num_vertices)

	c_vertex_sets, c_edges = star_decomp(contracted_g, x, 1/3, beta)

	# For each i, let Vi be the preimage under the contraction of vertices in Vi,
	# (xi , yi ) ∈ V0 × Vi be the edge of shortest length for which xi is a preimage of xi and yi
	# is a preimage of yi
	full_vertex_sets = []
	full_edges = []
	trees = []
	for i in length(c_vertex_sets)
		add!(full_vertex_sets,[])
		for j in length(c_vertex_sets[i])
			add!(full_vertex_sets[i], vertex_preimages[c_vertex_sets[i][j]])
		end
		# TODO add to full_edges
		trees[i] = LowStretchTree(subgraph(g,full_vertex_sets[i]), full_edges[i], original_num_vertices)
	end
	return union(trees,full_x)
end

LowStretchTree{V,E}(g::AbstractGraph{V,E}, x::V) = LowStretchTree(g,x, num_vertices(g))