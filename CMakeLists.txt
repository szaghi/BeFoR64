CMAKE_MINIMUM_REQUIRED(VERSION 3.0.2)
project(BeFoR64 Fortran)

if(TARGET PENF)
#    If lib PENF was added to the external project before BeFoR64
else()
add_subdirectory(src/third_party/PENF)
endif()
add_subdirectory(src/lib)
