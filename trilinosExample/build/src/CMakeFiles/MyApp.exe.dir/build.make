# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 2.8

#=============================================================================
# Special targets provided by cmake.

# Disable implicit rules so canonical targets will work.
.SUFFIXES:

# Remove some rules from gmake that .SUFFIXES does not remove.
SUFFIXES =

.SUFFIXES: .hpux_make_needs_suffix_list

# Produce verbose output by default.
VERBOSE = 1

# Suppress display of executed commands.
$(VERBOSE).SILENT:

# A target that is always out of date.
cmake_force:
.PHONY : cmake_force

#=============================================================================
# Set environment variables for the build.

# The shell in which to execute make rules.
SHELL = /bin/sh

# The CMake executable.
CMAKE_COMMAND = /usr/local/Cellar/cmake/2.8.7/bin/cmake

# The command to remove a file.
RM = /usr/local/Cellar/cmake/2.8.7/bin/cmake -E remove -f

# The program to use to edit the cache.
CMAKE_EDIT_COMMAND = /usr/local/Cellar/cmake/2.8.7/bin/ccmake

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = /Users/raphael/workspace/ind_study_fall_2012/trilinosExample

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /Users/raphael/workspace/ind_study_fall_2012/trilinosExample/build

# Include any dependencies generated for this target.
include src/CMakeFiles/MyApp.exe.dir/depend.make

# Include the progress variables for this target.
include src/CMakeFiles/MyApp.exe.dir/progress.make

# Include the compile flags for this target's objects.
include src/CMakeFiles/MyApp.exe.dir/flags.make

src/CMakeFiles/MyApp.exe.dir/main_file.cpp.o: src/CMakeFiles/MyApp.exe.dir/flags.make
src/CMakeFiles/MyApp.exe.dir/main_file.cpp.o: ../src/main_file.cpp
	$(CMAKE_COMMAND) -E cmake_progress_report /Users/raphael/workspace/ind_study_fall_2012/trilinosExample/build/CMakeFiles $(CMAKE_PROGRESS_1)
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Building CXX object src/CMakeFiles/MyApp.exe.dir/main_file.cpp.o"
	cd /Users/raphael/workspace/ind_study_fall_2012/trilinosExample/build/src && /usr/bin/c++   $(CXX_DEFINES) $(CXX_FLAGS) -o CMakeFiles/MyApp.exe.dir/main_file.cpp.o -c /Users/raphael/workspace/ind_study_fall_2012/trilinosExample/src/main_file.cpp

src/CMakeFiles/MyApp.exe.dir/main_file.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/MyApp.exe.dir/main_file.cpp.i"
	cd /Users/raphael/workspace/ind_study_fall_2012/trilinosExample/build/src && /usr/bin/c++  $(CXX_DEFINES) $(CXX_FLAGS) -E /Users/raphael/workspace/ind_study_fall_2012/trilinosExample/src/main_file.cpp > CMakeFiles/MyApp.exe.dir/main_file.cpp.i

src/CMakeFiles/MyApp.exe.dir/main_file.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/MyApp.exe.dir/main_file.cpp.s"
	cd /Users/raphael/workspace/ind_study_fall_2012/trilinosExample/build/src && /usr/bin/c++  $(CXX_DEFINES) $(CXX_FLAGS) -S /Users/raphael/workspace/ind_study_fall_2012/trilinosExample/src/main_file.cpp -o CMakeFiles/MyApp.exe.dir/main_file.cpp.s

src/CMakeFiles/MyApp.exe.dir/main_file.cpp.o.requires:
.PHONY : src/CMakeFiles/MyApp.exe.dir/main_file.cpp.o.requires

src/CMakeFiles/MyApp.exe.dir/main_file.cpp.o.provides: src/CMakeFiles/MyApp.exe.dir/main_file.cpp.o.requires
	$(MAKE) -f src/CMakeFiles/MyApp.exe.dir/build.make src/CMakeFiles/MyApp.exe.dir/main_file.cpp.o.provides.build
.PHONY : src/CMakeFiles/MyApp.exe.dir/main_file.cpp.o.provides

src/CMakeFiles/MyApp.exe.dir/main_file.cpp.o.provides.build: src/CMakeFiles/MyApp.exe.dir/main_file.cpp.o

# Object files for target MyApp.exe
MyApp_exe_OBJECTS = \
"CMakeFiles/MyApp.exe.dir/main_file.cpp.o"

# External object files for target MyApp.exe
MyApp_exe_EXTERNAL_OBJECTS =

src/MyApp.exe: src/CMakeFiles/MyApp.exe.dir/main_file.cpp.o
src/MyApp.exe: src/libmyappLib.a
src/MyApp.exe: /usr/local/lib/libmesquite.dylib
src/MyApp.exe: /usr/local/lib/libmoochothyra.dylib
src/MyApp.exe: /usr/local/lib/libmoocho.dylib
src/MyApp.exe: /usr/local/lib/librythmos.dylib
src/MyApp.exe: /usr/local/lib/libmoertel.dylib
src/MyApp.exe: /usr/local/lib/liblocathyra.dylib
src/MyApp.exe: /usr/local/lib/liblocaepetra.dylib
src/MyApp.exe: /usr/local/lib/liblocalapack.dylib
src/MyApp.exe: /usr/local/lib/libloca.dylib
src/MyApp.exe: /usr/local/lib/libnoxthyra.dylib
src/MyApp.exe: /usr/local/lib/libnoxepetra.dylib
src/MyApp.exe: /usr/local/lib/libnoxlapack.dylib
src/MyApp.exe: /usr/local/lib/libnox.dylib
src/MyApp.exe: /usr/local/lib/libintrepid.dylib
src/MyApp.exe: /usr/local/lib/libfei_trilinos.dylib
src/MyApp.exe: /usr/local/lib/libfei_base.dylib
src/MyApp.exe: /usr/local/lib/libstratimikos.dylib
src/MyApp.exe: /usr/local/lib/libstratimikosbelos.dylib
src/MyApp.exe: /usr/local/lib/libstratimikosaztecoo.dylib
src/MyApp.exe: /usr/local/lib/libstratimikosamesos.dylib
src/MyApp.exe: /usr/local/lib/libstratimikosml.dylib
src/MyApp.exe: /usr/local/lib/libstratimikosifpack.dylib
src/MyApp.exe: /usr/local/lib/libifpack2.dylib
src/MyApp.exe: /usr/local/lib/libanasazitpetra.dylib
src/MyApp.exe: /usr/local/lib/libModeLaplace.dylib
src/MyApp.exe: /usr/local/lib/libanasaziepetra.dylib
src/MyApp.exe: /usr/local/lib/libanasazi.dylib
src/MyApp.exe: /usr/local/lib/libbelostpetra.dylib
src/MyApp.exe: /usr/local/lib/libbelosepetra.dylib
src/MyApp.exe: /usr/local/lib/libbelos.dylib
src/MyApp.exe: /usr/local/lib/libml.dylib
src/MyApp.exe: /usr/local/lib/libkomplex.dylib
src/MyApp.exe: /usr/local/lib/libifpack.dylib
src/MyApp.exe: /usr/local/lib/libpamgen_extras.dylib
src/MyApp.exe: /usr/local/lib/libpamgen.dylib
src/MyApp.exe: /usr/local/lib/libamesos.dylib
src/MyApp.exe: /usr/local/lib/libgaleri-xpetra.dylib
src/MyApp.exe: /usr/local/lib/libgaleri.dylib
src/MyApp.exe: /usr/local/lib/libaztecoo.dylib
src/MyApp.exe: /usr/local/lib/libdpliris.dylib
src/MyApp.exe: /usr/local/lib/libisorropia.dylib
src/MyApp.exe: /usr/local/lib/liboptipack.dylib
src/MyApp.exe: /usr/local/lib/libthyratpetra.dylib
src/MyApp.exe: /usr/local/lib/libthyraepetraext.dylib
src/MyApp.exe: /usr/local/lib/libthyraepetra.dylib
src/MyApp.exe: /usr/local/lib/libthyracore.dylib
src/MyApp.exe: /usr/local/lib/libthyratpetra.dylib
src/MyApp.exe: /usr/local/lib/libthyraepetraext.dylib
src/MyApp.exe: /usr/local/lib/libthyraepetra.dylib
src/MyApp.exe: /usr/local/lib/libthyracore.dylib
src/MyApp.exe: /usr/local/lib/libepetraext.dylib
src/MyApp.exe: /usr/local/lib/libtpetraext.dylib
src/MyApp.exe: /usr/local/lib/libtpetrainout.dylib
src/MyApp.exe: /usr/local/lib/libtpetra.dylib
src/MyApp.exe: /usr/local/lib/libtriutils.dylib
src/MyApp.exe: /usr/local/lib/libglobipack.dylib
src/MyApp.exe: /usr/local/lib/libshards.dylib
src/MyApp.exe: /usr/local/lib/libzoltan.dylib
src/MyApp.exe: /usr/local/lib/libsimpi.dylib
src/MyApp.exe: /usr/local/lib/libepetra.dylib
src/MyApp.exe: /usr/local/lib/libkokkosdisttsqr.dylib
src/MyApp.exe: /usr/local/lib/libkokkosnodetsqr.dylib
src/MyApp.exe: /usr/local/lib/libkokkoslinalg.dylib
src/MyApp.exe: /usr/local/lib/libkokkosnodeapi.dylib
src/MyApp.exe: /usr/local/lib/libkokkos.dylib
src/MyApp.exe: /usr/local/lib/libkokkosdisttsqr.dylib
src/MyApp.exe: /usr/local/lib/libkokkosnodetsqr.dylib
src/MyApp.exe: /usr/local/lib/libkokkoslinalg.dylib
src/MyApp.exe: /usr/local/lib/libkokkosnodeapi.dylib
src/MyApp.exe: /usr/local/lib/libkokkos.dylib
src/MyApp.exe: /usr/local/lib/librtop.dylib
src/MyApp.exe: /usr/local/lib/libsacado.dylib
src/MyApp.exe: /usr/local/lib/libtpi.dylib
src/MyApp.exe: /usr/local/lib/libteuchos.dylib
src/MyApp.exe: /usr/lib/liblapack.dylib
src/MyApp.exe: /usr/lib/libblas.dylib
src/MyApp.exe: /usr/lib/libpthread.dylib
src/MyApp.exe: src/CMakeFiles/MyApp.exe.dir/build.make
src/MyApp.exe: src/CMakeFiles/MyApp.exe.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --red --bold "Linking CXX executable MyApp.exe"
	cd /Users/raphael/workspace/ind_study_fall_2012/trilinosExample/build/src && $(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/MyApp.exe.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
src/CMakeFiles/MyApp.exe.dir/build: src/MyApp.exe
.PHONY : src/CMakeFiles/MyApp.exe.dir/build

src/CMakeFiles/MyApp.exe.dir/requires: src/CMakeFiles/MyApp.exe.dir/main_file.cpp.o.requires
.PHONY : src/CMakeFiles/MyApp.exe.dir/requires

src/CMakeFiles/MyApp.exe.dir/clean:
	cd /Users/raphael/workspace/ind_study_fall_2012/trilinosExample/build/src && $(CMAKE_COMMAND) -P CMakeFiles/MyApp.exe.dir/cmake_clean.cmake
.PHONY : src/CMakeFiles/MyApp.exe.dir/clean

src/CMakeFiles/MyApp.exe.dir/depend:
	cd /Users/raphael/workspace/ind_study_fall_2012/trilinosExample/build && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /Users/raphael/workspace/ind_study_fall_2012/trilinosExample /Users/raphael/workspace/ind_study_fall_2012/trilinosExample/src /Users/raphael/workspace/ind_study_fall_2012/trilinosExample/build /Users/raphael/workspace/ind_study_fall_2012/trilinosExample/build/src /Users/raphael/workspace/ind_study_fall_2012/trilinosExample/build/src/CMakeFiles/MyApp.exe.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : src/CMakeFiles/MyApp.exe.dir/depend

