using MAT
require("weightedinclist.jl")

function loadcgraph(basename)
    nodesname = basename * ".cnode"
    edgesname = basename * ".cedge"
    graph = weightedinclist()

    # Regex for a line of the node file.
    noderegex = r"(\d+) (-?\d+.?\d+) (-?\d+.?\d+)"
    open(nodesname) do f
        for l in eachline(f)
            caps = match(noderegex, l).captures
            d = AttrDict()
            d["lat"] = caps[2]
            d["long"] = caps[3]
            node = AttrNode(int(caps[1]) + 1, d)
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

function loadcalmap()
    basename = "../data/cal"
    loadcgraph(basename)
end

function loadlesmis()
    struct = matread("../data/lesmis.mat")
    struct = struct["Problem"]
    # The struct has kind, date, id, notes, title, author, ed, aux, A, and name
    # aux has character names
    # A is the actual sparse matrix.

    mat = struct["A"]

    graph = weightedinclist()
    
    for name in struct["aux"]["nodename"]
        d = AttrDict()
        d["name"] = name
        
        add_vertex!(graph, d)
    end

    # Get a list of the non zero values, the edges, in:
    # (list of rows, list of cols, list of conductances) format
    nzs = findnz(mat)
    for i in 1:length(nzs[1])
        add_edge!(graph, i, 1/nzs[3][i], nzs[1][i], nzs[2][i])
    end
    graph
end
