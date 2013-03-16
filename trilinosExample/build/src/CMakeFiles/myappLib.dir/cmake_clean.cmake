FILE(REMOVE_RECURSE
  "CMakeFiles/myappLib.dir/src_file.cpp.o"
  "libmyappLib.pdb"
  "libmyappLib.a"
)

# Per-language clean rules from dependency scanning.
FOREACH(lang CXX)
  INCLUDE(CMakeFiles/myappLib.dir/cmake_clean_${lang}.cmake OPTIONAL)
ENDFOREACH(lang)
