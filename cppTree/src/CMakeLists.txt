SET(SOURCES
    main.cpp
    )

SET(HEADERS
  stretch.hpp
    )
SET(_GLIBCXX_FULLY_DYNAMIC_STRING 1)

ADD_EXECUTABLE(run main.cpp)
TARGET_LINK_LIBRARIES(run teuchos ${Boost_GRAPH_LIBRARY})

