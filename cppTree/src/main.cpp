

  #include <utility>                   // for std::pair
  #include <algorithm>                 // for std::for_each
  #include <boost/graph/graph_traits.hpp>
  #include <boost/graph/adjacency_list.hpp>
#include <boost/graph/graphml.hpp>

#include <iostream>
#include <string>
using namespace boost;


int main(int argc, char* argv[])
{
  if(argc < 2 || strlen(argv[1]) == 0){
    std::cerr << "You must give the name of a gml file." << std::endl;
    return 1;
  }
  std::cerr << "Reading file: " << argv[1] << std::endl;
  std::ifstream inFile;
  inFile.open(argv[1], std::ifstream::in);
  std::cerr << "read file" << std::endl;
  
  
  typedef adjacency_list<vecS, vecS, undirectedS, listS> Graph;

  boost::dynamic_properties dp;
  Graph g;
  boost::read_graphml(inFile, g, dp);

  std::cout << num_vertices(g) << " verticies" << std::endl;

  return 0;
}


