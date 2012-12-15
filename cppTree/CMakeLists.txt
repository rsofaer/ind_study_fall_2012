cmake_minimum_required(VERSION 2.8)

SET(Trilinos_DIR /usr/include/trilinos)
FIND_PACKAGE(Trilinos REQUIRED)

INCLUDE_DIRECTORIES ( ./  ./src_subdirTBD
                     ${Trilinos_INCLUDE_DIRS} ${Trilinos_TPL_INCLUDE_DIRS})

LINK_DIRECTORIES (${Trilinos_LIBRARY_DIRS} ${Trilinos_TPL_LIBRARY_DIRS})

ADD_SUBDIRECTORY(src)

#ENABLE_TESTING()
#ADD_SUBDIRECTORY(test)