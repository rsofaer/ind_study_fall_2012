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

# The sum of the weights (conductances) in s
cost(s::Set{Edge}) = reduce(+,map(x -> conductance(x), s))

vol{T}(s::Set{T}) = length(s)

# The vertices of distance at most r from v (in LENGTH)
function ball{V,E}( g::AbstractGraph{V,E}, v::V, r::Real)
	shortest = dijkstra_shortest_paths(g,edgedists(g), v)
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

function induced_subgraph{V,E}(g::AbstractGraph{V,E}, vertices)
end

# Contract any edge with a resistance less than l
function contract{V,E}(g::AbstractGraph{V,E}, l::Real)
	vertexsets = DisjointSets{V}(vertices(g))
	es = edges(g)
	for e in es
		if resistance(e) < l
			union!(vertexsets, e.source, e.target)
		end
	end
	vertexsets

	c_g = weightedinclist()
	ng = num_groups(vertexsets)
	for n in 1:ng
		d = AttrDict()
		d["preimage"] = Array(V,0)
		add_vertex!(c_g, d)
	end

	
	contracted_v = vertices(c_g)
	#image_map is a dict from the root number in vertexsets to the image vertex
	# which corresponds to that root number
	image_map = Dict{Int,Int}()
	next_vertex = 1
	for v in vertices(g)
		
		root = find_root(vertexsets,v)
		if !haskey(image_map, root)
			image = contracted_v[next_vertex]
			next_vertex += 1
			image_map[root] = vertex_index(image)
		end
		# Push the current vertex into the preimage array of its image.
		image = contracted_v[image_map[root]]
		preimage_array = attrs(image)["preimage"]
		push!(preimage_array, v)
	end

	conductance_matrix = spzeros(ng,ng)
	for e in es
		# Now we want resistance from one node to another to be min(resistance)
		si = image_map[find_root(vertexsets,e.source)]
		ti = image_map[find_root(vertexsets,e.target)]
		conductance_matrix[si, ti] = max(conductance_matrix[si,ti], conductance(e)) 
	end
	
	nzv = findnz(conductance_matrix)
	for i in 1:length(nzv[1])
		add_edge!(c_g, nzv[1][i], nzv[2][i], 1/nzv[3][i])
	end

	return c_g

end


function LowStretchTree{V,E}(g::AbstractGraph{V,E}, x::V, original_num_vertices::Int, original_num_edges::Int)
	beta = 1/(2*log(4/3,original_num_vertices + 32))

	if(num_vertices(g) <= 2)
		return g
	end

	rho = radius(g,x) 

	contracted_g = contract(g,beta*rho/original_num_vertices)

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
		trees[i] = LowStretchTree(subgraph(g,full_vertex_sets[i]), full_edges[i], original_num_vertices, original_num_edges)
	end
	return union(trees,full_x)
end

LowStretchTree{V,E}(g::AbstractGraph{V,E}, x::V) = LowStretchTree(g,x, num_vertices(g), num_edges(g))

# ({V0, ..., Vk, x, y}), 
function StarDecomp{V,E}(g::AbstractGraph{V,E}, x::V, delta::Float64, epsilon::Float64, original_num_edges::Int)
	rho = radius(g, x)
	central_radius = BallCut(g, x, rho, delta)
	center_ball = ball(g, x, r)
	center_shell = ballshell(g, x, r)
	cored_g = induced_subgraph(g, vertices(g) - center_ball)
	# cone_side_links[i] is the vertex in cones[i]
	# which will link to center_ball in the spanning tree
	cones, cone_side_links = ConeDecomp(g, center_shell, epsilon*rho/2)

	dijkstra = dijkstra_shortest_paths(g, edgedists(g), x)

	# There will be a link from core_side_links[i] to cone_side_links[i] in the tree
	core_side_links = Array(V, 0)
	for i in 1:length(cones)
		path = extract_path(dijkstra, cone_side_links[i])
		found_link = false
		for v in path
			if !contains(center_ball, v) && contains(center_shell, v)
				push!(core_side_links, yk)
				break
			end
		end
		if !found_link
			error("Cound not find a link from center $center_ball to cone $cones[i]")
		end
	end

	return (cones, cone_side_links, core_side_links)
end

function ConeDecomp{V,E}(g::AbstractGraph{V,E}, shell, delta)
	prev_shell = shell
	k = 0
	prev_g = g
	cone_side_links = Array(V,0)
	cones = Array(Vector{V}, 0)
	while !isempty(prev_shell)
		k += 1
		push!(cones, Array(V, 0))
		x = prev_shell[1]
		push!(cone_side_links, x)
		r = ConeCut(prev_g, x, 0, delta, prev_shell)
		push!(cones, concentric_system(prev_shell, r, x))
		prev_g = induced_subgraph(prev_g, vertices(prev_g) - cones[end])
		prev_shell = prev_shell - cones[end]
	end
	return (cones, cone_side_links)
end

function BallCut{V,E}(g::AbstractGraph{V,E}, center::V, rho::Float64, delta::Float64, original_num_edges::Int)
	r = rho*delta
	ds = dijkstra_shortest_paths(g, edgedists(g), center)
	cur_ind = 1 # 1 is center.  We need to find the biggest distance smaller than r
	vertex_indices_ascending_by_distance = sortperm(ds.dists)
	v_next = vertex_indices_ascending_by_distance[cur_ind]
	while cost(boundary(ball(r, center))) > ((vol(ball(r,center)) + 1)/((1-2*delta)*rho))*log2(original_num_edges+1)
		#find v_next not in ball(r, center) that minimizes dist(center, v_next) and set r = dist(center, v_next)
		while ds.dists[v_next] <= r
			cur_ind += 1
			v_next = vertex_indices_ascending_by_distance[cur_ind]
		end
		r = ds.dists[v_next]
	end
	return r
end
# The dijkstraStates data contains all the paths from the source it was generated with.
# This function returns an array with the path from the source to v
function extract_path{V}(ds::DijkstraStates, v::V)
	path = Array(V, 0)
	last_vertex = v
	while last_vertex != ds.parents[vertex_index(last_vertex)]
		unshift!(path, last_vertex)
		last_vertex  = ds.parents[vertex_index(last_vertex)]
	end
	return path
end