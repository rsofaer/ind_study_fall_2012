cmake_minimum_required(VERSION 2.8)

SET(CMAKE_BUILD_TYPE Debug)
SET(Trilinos_DIR /usr/include/trilinos)
FIND_PACKAGE(Trilinos REQUIRED)
find_package(Boost COMPONENTS graph REQUIRED)

#FIND_PACKAGE(MPI REQUIRED)

INCLUDE_DIRECTORIES ( ./  ./src_subdirTBD /usr/lib/openmpi/include
                      ${Boost_INCLUDE_DIR}
                     ${Trilinos_INCLUDE_DIRS} ${Trilinos_TPL_INCLUDE_DIRS})
                   #FOREACH(lib ${Trilinos_TPL_LIST})
                   #MESSAGE(${lib})
#ENDFOREACH()
#MESSAGE(${MPI_LIBRARY})
LINK_DIRECTORIES (${Trilinos_LIBRARY_DIRS} ${Trilinos_TPL_LIBRARY_DIRS} /usr/local/Cellar/boost/1.49.0/lib)

ADD_SUBDIRECTORY(src)

#ENABLE_TESTING()
#ADD_SUBDIRECTORY(test)
