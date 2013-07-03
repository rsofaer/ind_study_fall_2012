# Primitives defined on page 5 of Spielman and Teng et. al. Lower Stretch Spanning Trees
#
using Graphs
using DataStructures
using Debug

require("subgraph.jl")


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

# The edges with exactly one end in s.
function boundary{V,E}(g::AbstractGraph{V,E}, s)
	result = E[]
	for v in s
		for e in out_edges(v, g)
			neighbor = begin 
				if is_directed(g)
					target(e, g)
				else
					if target(e, g) == v
						source(e, g)
					else
						target(e, g)
					end
				end
			end

			if !contains(s, neighbor)
				push!(result, e)
			end
		end
	end
	return result
end

boundary{V, E}(g::AbstractGraph{V,E}, h::AbstractGraph{V,E}) = boundary(g, vertices(h))

# The sum of the weights (conductances) in s
cost(s) = reduce(+,map(x -> conductance(x), s))

vol(s) = length(s)

# The vertices of distance at most r from v (in LENGTH)
function ball{V,E}( g::AbstractGraph{V,E}, center::V, r::Real)
	ds = dijkstra_shortest_paths(g,edgedists(g), center)
	result = Array(V, 0)
	vlist = vertices(g)
	for i in 1:length(ds.dists)
		if ds.dists[i] < r
			push!(result, vlist[i])
		end
	end
	return result
end

# every vertex u not in B(v,r) with a neighbor w in B(v,r)
# such that dist(v,u) = dist(v,w) + the length of the edge from w to u
function ballshell{V,E}(g::AbstractGraph{V,E}, center::V, r::Real)
	# TODO directed graphs
	result = Array(V, 0)

	center_ball = ball(g, center, r)
	for v in center_ball
		for e in out_edges(v, g)
			neighbor = begin 
				if is_directed(g)
					target(e, g)
				else
					if target(e, g) == v
						source(e, g)
					else
						target(e, g)
					end
				end
			end

			if !contains(center_ball, neighbor) && !contains(result, neighbor)
				push!(result, neighbor)
			end
		end
	end
	return result
end

# the smallest rho st dist(x,y) < rho for all y
function radius{V,E}(g::AbstractGraph{V,E}, x::V)

	ed = edgedists(g)

	max(dijkstra_shortest_paths(g, ed, x).dists)
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
	#root_to_image_map is a dict from the root number in vertexsets to the image vertex
	# which corresponds to that root number
	root_to_image_map = Dict{Int,Int}()
	image_map = Dict{V,V}()
	next_vertex = 1
	for v in vertices(g)
		
		root = find_root(vertexsets,v)
		if !haskey(root_to_image_map, root)
			image = contracted_v[next_vertex]
			next_vertex += 1
			root_to_image_map[root] = vertex_index(image)
		end

		# Push the current vertex into the preimage array of its image.
		image = contracted_v[root_to_image_map[root]]

		image_map[v] = image
		preimage_array = attrs(image)["preimage"]
		push!(preimage_array, v)
	end

	conductance_matrix = spzeros(ng,ng)
	for e in es
		# Now we want resistance from one node to another to be min(resistance)
		si = root_to_image_map[find_root(vertexsets,e.source)]
		ti = root_to_image_map[find_root(vertexsets,e.target)]
		conductance_matrix[si, ti] = max(conductance_matrix[si,ti], conductance(e)) 
	end
	
	nzv = findnz(conductance_matrix)
	for i in 1:length(nzv[1])
		add_edge!(c_g, nzv[1][i], nzv[2][i], 1/nzv[3][i])
	end
	return c_g, image_map

end


function LowStretchTree{V,E}(g::AbstractGraph{V,E}, x::V, original_num_vertices::Int, original_num_edges::Int)
	beta = 1/(2*log(4/3,original_num_vertices + 32))



	if(num_vertices(g) <= 2)
		return g
	end

	rho = radius(g,x) 
	contracted_g, image_map = contract(g,beta*rho/original_num_vertices)

	c_center_ball, c_vertex_sets, c_cone_side_links, c_core_side_links = StarDecomp(contracted_g, image_map[x], 1/3, beta, original_num_edges)
	# For each i, let Vi be the preimage under the contraction of vertices in Vi,
	# (xi , yi ) ∈ V0 × Vi be the edge of shortest length for which xi is a preimage of xi and yi
	# is a preimage of yi
	# Here yi is a core side vertex and xi is a cone side vertex
	full_vertex_sets = Vector{V}[]
	full_cone_side_links = V[]
	full_core_side_links = V[]
	cone_core_links = E[]
	trees = AbstractGraph{V,E}[]

	for i in 1:length(c_vertex_sets)
		push!(full_vertex_sets,V[])
		# Get the preimages of the vertex sets
		for j in 1:length(c_vertex_sets[i])
			append!(full_vertex_sets[i], attrs(c_vertex_sets[i][j])["preimage"])			
		end

		# Get the minimal link from the preimage of cone to the preimage of the core
		cone_side_preimage = attrs(c_cone_side_links[i])["preimage"]
		core_side_preimage = attrs(c_core_side_links[i])["preimage"]
		minimal_link = nothing
		for v in cone_side_preimage
			for e in out_edges(v, g)
				if (contains(core_side_preimage, source(e)) || contains(core_side_preimage, target(e))) &&
				   ( minimal_link == nothing || resistance(e) < resistance(minimal_link))
				  minimal_link = e
				end
			end
		end

		push!(cone_core_links, minimal_link)

		if contains(cone_side_preimage, source(minimal_link))
			push!(full_cone_side_links, source(minimal_link))
			push!(full_core_side_links, target(minimal_link))
		else
			push!(full_cone_side_links, target(minimal_link))
			push!(full_core_side_links, source(minimal_link))
		end

		push!(trees, LowStretchTree(subgraph(g,full_vertex_sets[i]), full_cone_side_links[i], original_num_vertices, original_num_edges))
	end

	center_ball = V[]
	for v in c_center_ball
		append!(center_ball, attrs(v)["preimage"])
	end

	push!(trees, LowStretchTree(subgraph(g,center_ball), x, original_num_vertices, original_num_edges))

	tree = reduce(combine, trees)

	#debug
	if any(x -> vertex_index(x) == 17, vertices(g)) && !any(x -> vertex_index(x) == 17, vertices(tree))
		println("We lost vertex 17.
			$g
			$(vertices(g))
			$(vertices(tree))")
		plot(g)
		plot(add_edges(tree, cone_core_links))
		println("center ball $center_ball")
		global globalshit
		globalshit = (g, trees, tree, cone_core_links, c_vertex_sets, full_vertex_sets, image_map)
		error("lost vertex 17")
	end
	return add_edges(tree, cone_core_links)
end

LowStretchTree{V,E}(g::AbstractGraph{V,E}, x::V) = LowStretchTree(g,x, num_vertices(g), num_edges(g))

# ({V0, ..., Vk, x, y}), 
function StarDecomp{V,E}(g::AbstractGraph{V,E}, x::V, delta::Float64, epsilon::Float64, original_num_edges::Int)
	rho = radius(g, x)
	central_radius = BallCut(g, x, rho, delta, original_num_edges)
	center_ball = ball(g, x, central_radius)
	center_shell = ballshell(g, x, central_radius)

	cored_g = subgraph(g, filter(x -> !contains(center_ball, x), vertices(g)))
	# cone_side_terminals[i] is the vertex in cones[i]
	# which will link to center_ball in the spanning tree
	cones, cone_side_terminals = ConeDecomp(cored_g, center_shell, epsilon*rho/2, original_num_edges)

	dijkstra = dijkstra_shortest_paths(g, edgedists(g), x)

	# There will be a link from core_side_links[i] to cone_side_terminals[i] in the tree
	core_side_links = Array(V, 0)
	for i in 1:length(cones)
		path = extract_path(dijkstra, cone_side_terminals[i])
		found_link = false
		for v in path
			if !contains(center_ball, v) && contains(center_shell, v)
				push!(core_side_links, v)
				found_link = true
				break
			end
		end
		if !found_link
			error("Cound not find a link from center $center_ball to cone $(cones[i])")
		end
	end
	#debug
	if any(x -> vertex_index(x) == 6, vertices(g)) && !any(x -> vertex_index(x) == 6, center_ball) && !any(c -> contains(map(v -> vertex_index(v), c), 6),cones)
		println("We lost vertex 6.
			$g
			g: $(vertices(g))
			cones $(cones)
			cored_g: $(vertices(cored_g))
			center_shell: $center_shell
			")
		plot(g)
		println("center ball $center_ball")

		global globalshit
		globalshit = (g, x, delta, epsilon, original_num_edges, central_radius)
		error("lost vertex 6")
	end

	return (center_ball, cones, cone_side_terminals, core_side_links)
end

function ConeDecomp{V,E}(g::AbstractGraph{V,E}, shell::Vector{V}, delta, original_num_edges)
	prev_shell = shell
	k = 0
	prev_g = g
	cone_side_terminals = Array(V,0)
	cones = Array(Vector{V}, 0)
	while !isempty(prev_shell)
		k += 1
		x = prev_shell[1]
		push!(cone_side_terminals, x)
		r = ConeCut(prev_g, x, 0, delta, prev_shell, original_num_edges)
		push!(cones, vertices(build_cone(g, prev_shell, r, x)))
		prev_g = subgraph(prev_g, filter(x -> !contains(cones[end], x), vertices(prev_g)))
		prev_shell = filter(x -> !contains(cones[end], x), prev_shell)
		plot(prev_g)
	end

	#debug
	if any(x -> vertex_index(x) == 6, vertices(g)) && !any(c -> contains(map(v -> vertex_index(v), c), 6),cones)
		println("We lost vertex 6.
			$g
			g: $(vertices(g))
			cones $(cones)
			center_shell: $shell
			cts: $cone_side_terminals
			")
		plot(g)

		global globalshit
		globalshit = (g, shell, cones)
		error("lost vertex 6")
	end
	return (cones, cone_side_terminals)
end

function BallCut{V,E}(g::AbstractGraph{V,E}, center::V, rho::Float64, delta::Float64, original_num_edges::Int)
	r = rho*delta
	ds = dijkstra_shortest_paths(g, edgedists(g), center)
	cur_ind = 1 # 1 is center.  We need to find the biggest distance smaller than r
	vertex_indices_ascending_by_distance = sortperm(ds.dists)
	v_next = vertex_indices_ascending_by_distance[cur_ind]
	while cost(boundary(g, ball(g, center, r))) > ((vol(ball(g, center, r)) + 1)/((1-2*delta)*rho))*log2(original_num_edges+1)
		#find v_next not in ball(r, center) that minimizes dist(center, v_next) and set r = dist(center, v_next)
		while ds.dists[v_next] <= r
			cur_ind += 1
			v_next = vertex_indices_ascending_by_distance[cur_ind]
		end
		r = ds.dists[v_next]
	end
	return r
end

# Set of forward edges induced by the set s
# F (S) = {(u → v) : (u, v) ∈ E, dist(u, S) + d(u, v) = dist(v, S )}
function forward_edges{V,E}(g::AbstractGraph{V,E}, inducing_set::Vector{V})
	result = Array(E, 0)
	for v in inducing_set
		for e in out_edges(v, g)
			if !contains(inducing_set, target(e))
				push!(result, e)
			end
		end
	end
	return result
end

# the Set of vertices in V that can be reached from v by a path,
# the sum of the lengths of whose edges e that do not belong to F (S) is at most l
# is the cone of width l around center induced by S
# c = build_cone(g, v, S, l)
function build_cone{V,E}(g::AbstractGraph{V,E}, S::Vector{V},  l::Real, center::V)
	ed = edgedists(g)

	indices = map(x -> edge_index(x, g), forward_edges(g, S))
	for i in 1:length(ed)
		if contains(indices, i)
			ed[i] = 0
		end
	end

	ds = dijkstra_shortest_paths(g, ed, center)
	result = Array(V, 0)
	push!(result, center)

	vlist = vertices(g)
	for i in 1:length(ds.dists)
		if ds.dists[i] <= l
			push!(result, vlist[i])
		end
	end
	return subgraph(g, result)
end

#r = ConeCut(G, v, λ, λ′, S) 
function ConeCut{V,E}(g::AbstractGraph{V,E}, center::V, lambda::Real, lambda_prime::Real, inducing_set::Array{V}, original_num_edges::Int)
	r = lambda
	mu = NaN
	cur_cone = build_cone(g, inducing_set, r, center)
	if vol(edges(cur_cone)) == 0
		mu = (vol(vertices(cur_cone))+ 1)*log2(original_num_edges + 1)
	else
		mu = vol(vertices(cur_cone))*log2(original_num_edges/vol(edges(cur_cone)))
	end


	while cost(boundary(g, cur_cone)) > mu/(lambda_prime - lambda)
		# find w::V not in cur_cone closest to cur_cone and increase r so cur_cone encompasses w
		ds = dijkstra_shortest_paths(g, edgedists(g), vertices(cur_cone))
		w_ind = min_ind_with_filter(ds.dists, x -> x > 0)
		r += ds.dists[w_ind]
		cur_cone = build_cone(g, inducing_set, r, center)
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

function min_ind_with_filter(iterable, filter = x -> true)
	cur_min = nothing
	cur_min_ind = -1
	for i in 1:length(iterable)
		a = iterable[i]
		if filter(a) && (cur_min == nothing || a < cur_min)
			cur_min = a
			cur_min_ind = i
		end
	end
	return cur_min_ind
end
