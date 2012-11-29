function [sp, spCost] = dumb_edge_dijkstra(weightMatrix, src, dest)
% Dijkstra's algorithm for Single Source Shortest Path
global NIL = -1;

% Data initialization

n = size(weightMatrix, 1);
visited(1:n) = 0;
distance(1:n) = inf;
prev(1:n) = NIL; % Track the previous node to each node
distance(src) = 0;


edgeLengths = setdiff(unique(weightMatrix),[0]); % Get all distinct edge lengths, but discard 0.
k = size(edgeLengths,2);
edgeLists = cell(size(edgeLengths,2),1); % Empty list for each edge length
edgeLists{1} = [];
curEdges(1:k) = NIL;  % Active edge for each length, a pointer into the edgelist
f(1:k) = inf;  % distance plus an edge

% Put the edges from the source nodes in the list
visited(src) = true;
sourceRow = weightMatrix(src,:);
srInds = find(sourceRow); % Get the j such that there exists an edge (source, j)
for i = srInds
  % we have an edge from source to i
  weight = sourceRow(1,i);
  weightInd = lookup(edgeLengths, weight);
  edgeLists{weightInd} = [edgeLists{weightInd} [src; i]];
  if(curEdges(weightInd) == NIL)
    curEdges(weightInd) = 1; % point curEdges(weightInd) to the first element of the list
  end
end

for i = (1:k)
  [f, curEdges] = update(i,f, curEdges, edgeLists, visited, distance, weightMatrix);
end

while(sum(visited) < n)
  [fVal fInd] = min(f);
  curEdge = edgeLists{fInd}(:,curEdges(fInd));
  distance(curEdge(2)) = distance(curEdge(1)) + weightMatrix(curEdge(1), curEdge(2));
  prev(curEdge(2)) = curEdge(1);
  visited(curEdge(2)) = true;

  sourceRow = weightMatrix(curEdge(2),:);
  srInds = find(sourceRow); % Get the j such that there exists an edge (curEdge(2), j)
  for i = srInds
    % we have an edge from curEdge(2) to i
    weight = sourceRow(1,i);
    weightInd = lookup(edgeLengths, weight);
    edgeLists{weightInd} = [edgeLists{weightInd} [curEdge(2); i]];
    if(curEdges(weightInd) == NIL)
      curEdges(weightInd) = size(edgeLists{weightInd},2); % point curEdges(weightInd) to the first element of the list
    end

  end

  for i = (1:k)
   [f, curEdges] = update(i,f, curEdges, edgeLists, visited, distance, weightMatrix);
  end
end
  sp = [dest];
  while(sp(1) ~= src)
    sp = [prev(sp(1)) sp];
  end
  spCost = distance(dest);

end
function [f, curEdges] = update(weightInd,f, curEdges, edgeLists, visited, distance, weightMatrix)
  global NIL;
  if(curEdges(weightInd) == NIL)
    return;
  end
  curEdge = edgeLists{weightInd}(:,curEdges(weightInd));
  src = curEdge(1);
  dest = curEdge(2);

  if(visited(dest) == false) % The destination of the cur edge is unvisited
    f(weightInd) = distance(src) + weightMatrix(src,dest);
    return;
  end
  while(visited(dest) == true && curEdges(weightInd) < size(edgeLists{weightInd},2))
    curEdges(weightInd)+= 1;
    curEdge = edgeLists{weightInd}(:,curEdges(weightInd));
    src = curEdge(1);
    dest = curEdge(2);
  end

  if(visited(dest) == false)
    f(weightInd) = distance(src) + weightMatrix(src,dest);
  else
    curEdges(weightInd) = NIL;
    f(weightInd) = inf;
  end
end


