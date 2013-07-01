using Base.Test

require("subgraph.jl")
require("data.jl")

let g=loadcgraph("../data/tiny")
  sg = subgraph(g, vertices(g)[2:3])
  @test vertices(sg) == vertices(g)[2:3]
  @test length(edges(sg)) == 1
  @test edges(sg)[1] == edges(g)[2]
  @test is_directed(sg) == is_directed(g)


  @test implements_vertex_list(sg)    == true
  @test implements_edge_list(sg)      == false
  @test implements_vertex_map(sg)     == true
  @test implements_edge_map(sg)       == true

  @test implements_adjacency_list(sg) == true
  @test implements_incidence_list(sg) == true
  @test implements_bidirectional_adjacency_list(sg) == false
  @test implements_bidirectional_incidence_list(sg) == false
  @test implements_adjacency_matrix(sg) == false

  @graph_requires sg incidence_list
  og = simple_inclist(5)
  @graph_requires og incidence_list
end