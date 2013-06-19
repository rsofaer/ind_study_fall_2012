using Graphs
require("weightedinclist.jl")
# Let's load the CA road map

basename = "../data/cal"

function loadcgraph(basename)
    nodesname = basename * ".cnode"
    edgesname = basename * ".cedge"
    graph = weightedinclist()

    # Regex for a line of the node file.
    noderegex = r"(\d+) (-?\d+.?\d+) (-?\d+.?\d+)"
    open(nodesname) do f
        for l in eachline(f)
            caps = match(noderegex, l).captures
            node = AttrNode(int(caps[1]) + 1, ["lat" => caps[2], "long" => caps[3]])
            add_vertex!(graph, node)
        end
    end

    edgeregex = r"(\d+) (\d+) (\d+) (-?\d+.?\d+)"
    open(edgesname) do f
        for l in eachline(f)
            caps = match(edgeregex, l).captures
            add_edge!(graph, int(caps[1]) + 1, float(caps[4]), int(caps[2]) + 1, int(caps[3]) + 1)
        end
    end
    graph
end
g = loadcgraph(basename)

