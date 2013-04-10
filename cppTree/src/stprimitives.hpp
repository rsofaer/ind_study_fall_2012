// This is a file of the primitives on page 5 of http://arxiv.org/pdf/cs/0411064v5.pdf, Lower Stretch Spanning Trees.
#ifndef _IND_STUDY_CPPTREE_STPRIMITIVES_H
#define _IND_STUDY_CPPTREE_STPRIMITIVES_H

#include <boost/graph/graph_traits.hpp>
#include <set>
#include "graph_typedefs.hpp"
namespace cpptree{
template<class Graph>
struct st_primitives{
	typedef typename gt<Graph>::Edge Edge;
	typedef typename gt<Graph>::Vertex Vertex;
	typedef typename gt<Graph>::WeightMap WeightMap; // A weightmap is drawn from a specific graph with get.
	typedef typename gt<Graph>::VertexSet VertexSet;
	typedef typename gt<Graph>::EdgeSet EdgeSet;

	static void ball(const Vertex& v, const Graph& g, const edge_weight_t radius, set<Vertex> ballContents){

	}

	
	static void boundary(const VertexSet& S, EdgeSet& e){

	}

	/*
	r = BallCut(G,x0,ρ,δ)
		1. Set r = δρ.
		2. While cost (∂ (B(r,x0))) >((vol(B(r,x0))+1)/(1−2δ)ρ)log2(m + 1),
			a. Find the vertex v not in B(r,x0) that minimizes dist(x0,v) and set r = dist(x0,v).
	*/
	static edge_weight_t ballCut(const Graph& g, const Vertex& center, const edge_weight_t rho, const edge_weight_t delta){
		auto r = rho*delta;
		
		return r;
	}
};
}
#endif