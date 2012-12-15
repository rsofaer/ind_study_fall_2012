SET(SOURCES
    main.cpp
    )

SET(HEADERS
    )


ADD_LIBRARY(myappLib ${SOURCES} ${HEADERS})

ADD_EXECUTABLE(MyApp.exe main.cpp)

TARGET_LINK_LIBRARIES(MyApp.exe  myappLib  ${Trilinos_LIBRARIES} ${Trilinos_TPL_LIBRARIES} ${MPI_LIBRARIES}) 