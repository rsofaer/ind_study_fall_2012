#include <utility>                   // for std::pair
#include <algorithm>                 // for std::for_each
#include <boost/graph/graph_traits.hpp>
#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/random.hpp>

#include <iostream>
#include <ctime>
#include <string>
#include <boost/random/linear_congruential.hpp>
#include "stretch.hpp"
using namespace boost;


int main(int argc, char* argv[])
{
  typedef adjacency_list<vecS, vecS, undirectedS, 
            no_property, property<edge_weight_t, int>, listS> Graph;

  int numV = 20;
  int numE = 182;
  Graph g;
  random::minstd_rand gen;
  gen.seed(time(0));
  generate_random_graph(g, numV, numE, gen, false);
  randomize_property<edge_weight_t>(g, gen);

  std::cout << num_vertices(g) << " verticies" << std::endl;
  std::cout << num_edges(g) << " edges" << std::endl;

  auto weights = get(edge_weight, g);

  for(auto edgePair = edges(g); edgePair.first != edgePair.second; ++edgePair.first)
  {
      weights[*edgePair.first] = weights[*edgePair.first] % 1000;
  }
  
  std::cout << "Stretch: " << stretch(g,g) << std::endl;

  return 0;
}


