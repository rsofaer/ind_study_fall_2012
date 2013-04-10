#include <utility>                   // for std::pair
#include <algorithm>                 // for std::for_each
#include <boost/unordered/unordered_set.hpp>
#include <boost/graph/graph_traits.hpp>
#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/random.hpp>

#include <iostream>
#include <ctime>
#include <string>
#include <random>
//#include <boost/random/linear_congruential.hpp>
#include <boost/graph/kruskal_min_spanning_tree.hpp>
#include "stretch.hpp"
#include "stprimitives.hpp"
#include "graph_typedefs.hpp"
using namespace boost;
using namespace cpptree;

  typedef adjacency_list<vecS, vecS, undirectedS, 
            no_property, property<edge_weight_t, double >, listS> Graph;

   typedef gt<Graph>::EdgeSet EdgeSet;

int main(int argc, char* argv[])
{
	enum edge_included_t { edge_included };
  int numV = 20;
  int numE = 100;
  Graph g;
  std::minstd_rand gen;
  gen.seed((unsigned int) time(0));
  generate_random_graph(g, numV, numE, gen, false);

  randomize_property<edge_weight_t>(g, gen);

  std::cout << num_vertices(g) << " verticies" << std::endl;
  std::cout << num_edges(g) << " edges" << std::endl;

  auto weights = get(edge_weight, g);

  for(auto edgePair = edges(g); edgePair.first != edgePair.second; ++edgePair.first)
  {
      weights[*edgePair.first] = fmod(weights[*edgePair.first],1000);
  }

  
  
  EdgeSet t;
  auto includedPred = boost::is_in_subset<EdgeSet>(t);
    kruskal_minimum_spanning_tree(g, std::insert_iterator<EdgeSet>(t,t.begin()));

  std::cout << "Edges in spanning tree: " << t.size() << std::endl;
  std::cout << "Stretch: " << stretchCalculator<Graph, boost::is_in_subset<EdgeSet> >::stretch(g,includedPred) << std::endl;

  return 0;
}
