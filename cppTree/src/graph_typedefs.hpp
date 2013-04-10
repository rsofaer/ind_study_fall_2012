#ifndef _IND_STUDY_CPPTREE_GRAPH_TYPEDEFS_H
#define _IND_STUDY_CPPTREE_GRAPH_TYPEDEFS_H
#include <boost\graph\graph_traits.hpp>
#include <boost\unordered_set.hpp>

namespace cpptree{
	template<class Graph>
	struct gt{
		typedef typename graph_traits<Graph>::edge_descriptor Edge;
		typedef typename graph_traits<Graph>::vertex_descriptor Vertex;

		struct EdgeHasher{
			std::size_t operator ()(Edge const& e) const{
				std::size_t hash = 0;
				boost::hash_combine(hash, e.m_source);
				boost::hash_combine(hash, e.m_target);
				return hash;
			}
		};
		struct EdgeEqual{
			bool operator()(Edge const& a, Edge const& b) const{
				return a.m_source == b.m_source && a.m_target == b.m_target;
			}
		};
		typedef boost::unordered_set<Edge, EdgeHasher, EdgeEqual> EdgeSet;

		typedef boost::unordered_set<Vertex> VertexSet;
		typedef typename property_map<Graph, edge_weight_t>::type WeightMap;
	};

}
#endif