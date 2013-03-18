#include <utility>                   // for std::pair
#include <algorithm>                 // for std::for_each
#include <unordered_set>
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
using namespace boost;

  typedef adjacency_list<vecS, vecS, undirectedS, 
            no_property, property<edge_weight_t, double >, listS> Graph;
    typedef graph_traits<Graph>::edge_descriptor Edge;
	typedef graph_traits<Graph>::vertex_descriptor Vertex;

   struct EdgeHasher{
	   std::size_t operator ()(Edge const& e){
	    std::size_t hash = 0;
		boost::hash_combine(hash, e.m_source);
		boost::hash_combine(hash, e.m_target);
        return hash;
   }
  };
   struct EdgeEqual{
	bool operator()(Edge const& a, Edge const& b){
		return a.m_source == b.m_source && a.m_target == b.m_target;
	}
   };
 


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

  
  typedef std::unordered_set<Edge, EdgeHasher, EdgeEqual> EdgeSet;
  EdgeSet t;
  auto includedPred = [&t](graph_traits<Graph>::edge_descriptor e) -> bool { 
	  return (t.find(e) != t.end());
  };
    kruskal_minimum_spanning_tree(g, std::insert_iterator<EdgeSet>(t,t.begin()));

  std::cout << "Edges in spanning tree: " << t.size() << std::endl;
  std::cout << "Stretch: " << stretchCalculator<Graph>::stretch(g,includedPred) << std::endl;

  return 0;
}


