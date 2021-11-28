// Copyright Mika Fischer 2021.
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE.txt or copy at https://www.boost.org/LICENSE_1_0.txt)

#include "mcpp/byte_span/byte_span.hpp"
#include <functional>
#include <iterator>
#include <string_view>
#include <utility>

namespace mcpp::byte_span::detail {
static_assert(is_byte_like_v<char>);
static_assert(is_byte_like_v<unsigned char>);
static_assert(is_byte_like_v<uint8_t>);
static_assert(is_byte_like_v<std::byte>);
static_assert(is_byte_like_v<void>);

static_assert(!is_byte_like_v<char &>);
static_assert(!is_byte_like_v<const char &>);
static_assert(!is_byte_like_v<char *>);
static_assert(!is_byte_like_v<const char *>);

static_assert(!is_byte_like_v<short>);
static_assert(!is_byte_like_v<int8_t>);

static_assert(is_compatible_v<char, std::byte>);
static_assert(!is_compatible_v<const char, std::byte>);
static_assert(is_compatible_v<char, const std::byte>);
static_assert(is_compatible_v<const char, const std::byte>);

static_assert(is_compatible_v<void, std::byte>);
static_assert(!is_compatible_v<const void, std::byte>);
static_assert(is_compatible_v<void, const std::byte>);
static_assert(is_compatible_v<const void, const std::byte>);

enum class TestEnum { U8 };

static_assert(!is_container_v<const TestEnum &>);
static_assert(!is_container_compatible_v<const TestEnum &, const std::byte>);

static_assert(is_container_compatible_v<std::vector<char> &, std::byte>);
static_assert(!is_container_compatible_v<const std::vector<char> &, std::byte>);
static_assert(is_container_compatible_v<std::vector<char> &, const std::byte>);
static_assert(is_container_compatible_v<const std::vector<char> &, const std::byte>);

static_assert(!std::is_const_v<container_element_type<std::vector<char>>>);
static_assert(is_container_compatible_v<std::vector<char>, std::byte>);
static_assert(!is_container_compatible_v<const std::vector<char>, std::byte>);
static_assert(is_container_compatible_v<std::vector<char>, const std::byte>);
static_assert(is_container_compatible_v<const std::vector<char>, const std::byte>);

static_assert(detail::is_container_compatible_v<const char (&)[5], const std::byte>);

} // namespace mcpp::byte_span::detail

// runtime tests
auto main() -> int {
}