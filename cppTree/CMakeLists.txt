cmake_minimum_required(VERSION 2.8)

SET(CMAKE_BUILD_TYPE Debug)
SET(Trilinos_DIR /usr/include/trilinos)
SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -std=c++0x") 
find_package(Boost COMPONENTS graph REQUIRED)

#FIND_PACKAGE(MPI REQUIRED)

INCLUDE_DIRECTORIES ( ./  ./src 
                      ${Boost_INCLUDE_DIR}
                     )
                   #FOREACH(lib ${Trilinos_TPL_LIST})
                   #MESSAGE(${lib})
#ENDFOREACH()
#MESSAGE(${MPI_LIBRARY})
LINK_DIRECTORIES ( ${Boost_LIBRARY_DIRS})

ADD_SUBDIRECTORY(src)

#ENABLE_TESTING()
#ADD_SUBDIRECTORY(test)
