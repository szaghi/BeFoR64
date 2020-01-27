include(GNUInstallDirs)

SET(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}/tests/${CMAKE_INSTALL_BINDIR}")

function(add_befor64_doctest test_src libraries)
    get_filename_component(test_name ${test_src} NAME_WE)
    get_filename_component(test_dir ${test_src} DIRECTORY)
    set(result_src "${test_dir}/${test_name}.result")

    add_executable(${test_name} ${test_src})
    foreach(lib IN LISTS libraries)
        target_link_libraries(${test_name} ${lib})
    endforeach()
    add_test(NAME ${test_name} COMMAND ${test_name})

    file(STRINGS ${result_src} result_raw)
    string(REGEX REPLACE "([+.])([^+.]*)" "\\\\\\1\\2" result ${result_raw})

    set_tests_properties(${test_name} PROPERTIES
        PASS_REGULAR_EXPRESSION "${result}"
    )
endfunction()

file(GLOB_RECURSE befor64_doctests *.f90)

foreach(befor64_doctest IN LISTS befor64_doctests)
    add_befor64_doctest(${befor64_doctest} BeFoR64 PENF)
endforeach()
