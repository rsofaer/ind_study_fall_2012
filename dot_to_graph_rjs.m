function [Adj, x, y] = unweighted_dot_to_adj(filename)
% reads a dot, gets rid of the labels and sets the nonexistent edges to Inf.
  [Adj labels x y] = dot_to_graph(filename);
  Adj = arrayfun(@sign, Adj);
  Adj = 1./Adj;
end
