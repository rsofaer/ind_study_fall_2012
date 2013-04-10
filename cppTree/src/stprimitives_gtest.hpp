#ifndef _CPPTREE_STPRIMITIVES_GTEST_H_
#define _CPPTREE_STPRIMITIVES_GTEST_H_
#include <boost\graph\adjacency_list.hpp>
#include "stprimitives.hpp"
#include "gtest/gtest.h"
namespace cpptree_test{
using namespace cpptree;
typedef adjacency_list<vecS, vecS, undirectedS, 
            no_property, property<edge_weight_t, double >, listS> Graph;
TEST(boundary, st_primitives)

	EXPECT_EQ(1,0);
}
#endif // !_CPPTREE_STPRIMITIVES_GTEST_H_
