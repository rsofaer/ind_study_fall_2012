#include <boost/graph/graph_traits.hpp>
#include <boost/lexical_cast.hpp>
#include <utility>                   // for std::pair
#include <algorithm>                 // for std::for_each

#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/filtered_graph.hpp>
#include <boost/graph/johnson_all_pairs_shortest.hpp>
#include <boost/property_map/property_map.hpp>


using namespace std;
using namespace boost;
template<class Graph>
struct stretchCalculator{

typedef vector<vector<double> > Matrix;

static void printEdge(typename graph_traits<Graph>::edge_descriptor i){std::cout << ' ' << i;}
template<typename Param>
static bool trueF(Param p){return true}

static void printM(Matrix& m){
  
  for( Matrix::const_iterator i = m.begin(); i != m.end(); ++i){
    Matrix::value_type r = *i;
    for(Matrix::value_type::const_iterator j = r.begin(); j != r.end(); ++j){
      std::cout << *j << " ";
    }
    std::cout << std::endl;
  }
}
// Given a spanning tree T, and a graph G with edges E.
// avg_stretchT(G) = (1/|E|)*(the sum over all edges in E of
//                             the new distance/the old distance)
template<class Callable>
static double stretch(Graph& g, Callable& edgeIncludedPred){
  int nvg = num_vertices(g);

  // Get all pairs shortest path for the graph.
  Matrix gPaths(nvg, vector<double>(nvg,0));
  johnson_all_pairs_shortest_paths(g, gPaths);

  // Get all pairs shortest path for the Tree.
  //Matrix tPaths(nvt, vector<double>(nvt,0));
  //johnson_all_pairs_shortest_paths(t,tPaths);
  auto t = filtered_graph<Graph, Callable>(g, edgeIncludedPred);

  auto weights = get(edge_weight, g);
  auto weightsT = get(edge_weight, t);
  double totalWeight = 0.0;

  // For each edge in G's edges, 
  for(auto edge = edges(g); edge.first != edge.second; ++edge.first)
  {
    //Get the edge weight in G.
    auto weight = weights[*(edge.first)];
    totalWeight += weight;
	Edge e = *(edge.first);
	std::cout << "Edge: " << e << std::endl;
    std::cout << "Original weight: " << weight << std::endl;

    //std::cout << "Original shortest path: " <<  gPaths[source(*(edge.first),g)][target(*(edge.first),g)] << std::endl;
    //double newWeight = tPaths[source(*(edge.first),g)][target(*(edge.first),g)];
    //std::cout << "New Shortest Path: " << newWeight << std::endl;

    // new path length is the sum of lengths along the tree
    // stretch for this edge is new_path_length*weight
  }
  std::cout << "Total weight: " << totalWeight << std::endl;
  return 0.0;
}

};