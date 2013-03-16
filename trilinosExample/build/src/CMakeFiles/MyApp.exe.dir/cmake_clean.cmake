FILE(REMOVE_RECURSE
  "CMakeFiles/MyApp.exe.dir/main_file.cpp.o"
  "MyApp.exe.pdb"
  "MyApp.exe"
)

# Per-language clean rules from dependency scanning.
FOREACH(lang CXX)
  INCLUDE(CMakeFiles/MyApp.exe.dir/cmake_clean_${lang}.cmake OPTIONAL)
ENDFOREACH(lang)
