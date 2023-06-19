add_executable(test-byte-span byte_span.cpp)
target_link_libraries(test-byte-span mcpp::byte-span)
add_test(NAME test-byte-span COMMAND test-byte-span)