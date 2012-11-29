function [sp, spCost] = dumb_dijkstra(weightMatrix, source, dest)
% Dijkstra's algorithm for Single Source Shortest Path

n = size(weightMatrix, 1);
visited(1:n) = 0;
distance(1:n) = inf;
prev(1:n) = n + 1; % Track the previous node to each node

distance(source) = 0;

while(sum(visited) < n)
	nextIndex = -1;
	nextDist = inf;
	for(i = 1:n)
		if(visited(i) == 0 && distance(i) < nextDist)
      nextDist = distance(i);
      nextIndex = i;
    end
	end
  visited(nextIndex) = 1;
  for(i = 1:n)
    if(distance(i) > distance(nextIndex) + weightMatrix(nextIndex,i))
      distance(i) = distance(nextIndex) + weightMatrix(nextIndex,i);
      prev(i) = nextIndex;
    end
  end
end

sp = [dest];
while(sp(1) ~= source)
  sp = [prev(sp(1)) sp];
end
spCost = distance(dest);