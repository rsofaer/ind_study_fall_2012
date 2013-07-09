using Base.Test
require("data.jl")
let
    lm = loadlesmis()
    @test num_vertices(lm) == 77
    @test num_edges(lm) == 508
end

let cal = loadcalmap()
  @test num_vertices(cal) == 21048
  @test num_edges(cal) == 21693
end