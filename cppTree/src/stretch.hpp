#include <boost/graph/graph_traits.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/utility/result_of.hpp>
#include <utility>                   // for std::pair
#include <algorithm>                 // for std::for_each

#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/filtered_graph.hpp>
#include <boost/graph/johnson_all_pairs_shortest.hpp>
#include <boost/property_map/property_map.hpp>
#include <boost/property_map/function_property_map.hpp>

using namespace std;
#ifndef _IND_STUDY_CPPTREE_STRETCH_CALCULATOR_H
#define _IND_STUDY_CPPTREE_STRETCH_CALCULATOR_H

using namespace boost;
namespace cpptree{
template<class Graph, typename EdgePredicate>
struct stretchCalculator{

typedef vector<vector<double> > Matrix;
typedef typename graph_traits<Graph>::edge_descriptor Edge;
typedef typename property_map<Graph, edge_weight_t>::type WeightMap; // A weightmap is drawn from a specific graph with get.


struct edgeReciprocalFunctor{
	const WeightMap& w;
	typedef typename WeightMap::value_type value_type;
	typedef typename WeightMap::value_type result;
	typedef typename WeightMap::key_type key_type;

	explicit edgeReciprocalFunctor(const WeightMap& w): w(w) {}
	const typename value_type operator[] (const key_type& e) const{ 
		return 1/w[e];
	}
	const typename value_type operator() (const key_type& e) const{ 
		return 1/w[e];
	}
};

static void printEdge(Edge i){std::cout << ' ' << i;}

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

static double stretch(Graph& g, EdgePredicate& edgeIncludedPred){
  int nvg = num_vertices(g);

  // Get all pairs shortest path for the graph.
  Matrix gPaths(nvg, vector<double>(nvg,0));
  johnson_all_pairs_shortest_paths(g, gPaths);

  // Get all pairs shortest path for the Tree.
  //Matrix tPaths(nvt, vector<double>(nvt,0));
  //johnson_all_pairs_shortest_paths(t,tPaths);

   // Filter the graph by the edge set.
  auto t = filtered_graph<Graph, EdgePredicate>(g, edgeIncludedPred);

  auto weights = boost::get(edge_weight, g); // Get the weight map from the full graph.
  WeightMap weightsT = boost::get(edge_weight, t);  // Get the weight map from the tree.

  // Invert the weights to get the sum of the reciprocal along the shortest path.
  // Since there is a unique shortest path in a tree for all positive weight functions,
  // we can find the path using the reciprocal weights. 
  auto reciprocal_map = edgeReciprocalFunctor(weightsT);
  Matrix tReciprocalPaths(nvg,vector<double>(nvg, 0));
  auto pmapr = make_function_property_map<edgeReciprocalFunctor::key_type, edgeReciprocalFunctor::value_type,
	  edgeReciprocalFunctor>(reciprocal_map);
  johnson_all_pairs_shortest_paths(t, tReciprocalPaths,  weight_map(pmapr));
  
  
  double totalWeight = 0.0;
  double totalStretch = 0.0;

  // For each edge in G's edges, 
  for(auto edge = edges(g); edge.first != edge.second; ++edge.first)
  {
    //Get the edge weight in G.
	Edge e = *(edge.first);
    auto weight = weights[e];
    totalWeight += weight;
	std::cout << "Edge: " << e << std::endl;
    std::cout << "Original weight: " << weight << std::endl;
	std::cout << "Reciprocal weight: " << reciprocal_map[e] << std::endl;
    //std::cout << "Original shortest path: " <<  gPaths[source(*(edge.first),g)][target(*(edge.first),g)] << std::endl;
    //double newWeight = tPaths[source(*(edge.first),g)][target(*(edge.first),g)];
    //std::cout << "New Shortest Path: " << newWeight << std::endl;

    // new path length is the sum of lengths along the tree
    // stretch for this edge is new_path_length*weight
	totalStretch += (tReciprocalPaths[source(e, t)][target(e, t)])*weight;
  }
  std::cout << "Total weight: " << totalWeight << std::endl;
  return totalStretch/totalWeight;
}

};
}
#endif