using MAT
require("weightedinclist.jl")

function loadcgraph(basename; get_vertex_attrs=false)
    nodesname = basename * ".cnode"
    edgesname = basename * ".cedge"

    nv = countlines(nodesname)
    graph = weightedinclist(nv, is_directed = false)

    if get_vertex_attrs
        vi = 1
        # Regex for a line of the node file.
        noderegex = r"(\d+) (-?\d+.?\d+) (-?\d+.?\d+)"
        open(nodesname) do f
            for l in eachline(f)
                caps = match(noderegex, l).captures
                d = AttributeDict()
                d["lat"] = caps[2]
                d["long"] = caps[3]
                vertex = vertices(graph)[vi]
                set_attributes!(graph, vertex, d)
                vi += 1
            end
        end
    end

    edgeregex = r"(\d+) (\d+) (\d+) (-?\d+.?\d+)"
    open(edgesname) do f
        for l in eachline(f)
            caps = match(edgeregex, l).captures
            add_edge!(graph, int(caps[2]) + 1, int(caps[3]) + 1,  float(caps[4]))
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

    graph = weightedinclist(length(struct["aux"]["nodename"]), is_directed=false)
    
    for i in 1:length(struct["aux"]["nodename"])
        name = struct["aux"]["nodename"][i]
        d = AttributeDict()
        d["name"] = name
        d["label"] = "$name: $i"
        
        set_attributes!(graph, vertices(graph)[i], d)
    end

    # Get a list of the non zero values, the edges, in:
    # (list of rows, list of cols, list of conductances) format
    done = Dict()
    nzs = findnz(mat)
    n = 1
    for i in 1:length(nzs[1])
        if nzs[1][i] < nzs[2][i]
            add_edge!(graph, int(nzs[1][i]), int(nzs[2][i]), 1/nzs[3][i])
            n += 1
        end
    end
    graph
end
