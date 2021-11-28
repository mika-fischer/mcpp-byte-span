cmake_minimum_required(VERSION 3.18)
project(mcpp-byte-span)

if (CMAKE_SOURCE_DIR STREQUAL PROJECT_SOURCE_DIR)
    set(is_toplevel TRUE)
else ()
    set(is_toplevel FALSE)
endif ()

option(mcpp-byte-span_WITH_TESTS "Build tests" ${is_toplevel})

add_library(mcpp-byte-span INTERFACE)
target_include_directories(mcpp-byte-span INTERFACE
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
    $<INSTALL_INTERFACE:include>)
target_compile_features(mcpp-byte-span INTERFACE cxx_std_17)
set_target_properties(mcpp-byte-span PROPERTIES EXPORT_NAME mcpp::byte-span)
add_library(MCPP::byte-span ALIAS mcpp-byte-span)

install(TARGETS mcpp-byte-span EXPORT ${PROJECT_NAME}-config)
install(EXPORT ${PROJECT_NAME}-config DESTINATION share/cmake/${PROJECT_NAME})
install(DIRECTORY include DESTINATION ".")

if (mcpp-byte-span_WITH_TESTS)
    add_subdirectory(tests)
endif ()