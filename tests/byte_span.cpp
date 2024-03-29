// Copyright Mika Fischer 2021.
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE.txt or copy at https://www.boost.org/LICENSE_1_0.txt)

#include "mcpp/byte_span/byte_span.hpp"

#include <functional>
#include <iterator>
#include <string_view>
#include <type_traits>
#include <utility>
#include <vector>

namespace mcpp::byte_span::detail {

// Size
static_assert(sizeof(mut_byte_view) == sizeof(void *) + sizeof(std::size_t));
static_assert(sizeof(byte_view) == sizeof(void *) + sizeof(std::size_t));
static_assert(sizeof(mut_byte_span<1>) == sizeof(void *));
static_assert(sizeof(byte_span<1>) == sizeof(void *));
static_assert(sizeof(mut_byte_span<0>) == 1);
static_assert(sizeof(byte_span<0>) == 1);

// Default constructor
static_assert(std::is_default_constructible_v<byte_view>);
static_assert(std::is_default_constructible_v<byte_span<0>>);
static_assert(!std::is_default_constructible_v<byte_span<4>>);
static_assert(std::is_default_constructible_v<mut_byte_view>);
static_assert(std::is_default_constructible_v<mut_byte_span<0>>);
static_assert(!std::is_default_constructible_v<mut_byte_span<4>>);

template <bool const_dynamic, bool const_static, bool mutable_dynamic, bool mutable_static, typename... Args>
constexpr auto test() -> bool {
    static_assert(std::is_constructible_v<byte_view, Args...> == const_dynamic);
    static_assert(std::is_constructible_v<byte_span<4>, Args...> == const_static);
    static_assert(std::is_constructible_v<mut_byte_view, Args...> == mutable_dynamic);
    static_assert(std::is_constructible_v<mut_byte_span<4>, Args...> == mutable_static);
    return true;
}

template <bool const_dynamic, bool const_static, bool mutable_dynamic, bool mutable_static, typename Arg>
constexpr auto test_implicit() -> bool {
    static_assert(std::is_convertible_v<Arg, byte_view> == const_dynamic);
    static_assert(std::is_convertible_v<Arg, byte_span<4>> == const_static);
    static_assert(std::is_convertible_v<Arg, mut_byte_view> == mutable_dynamic);
    static_assert(std::is_convertible_v<Arg, mut_byte_span<4>> == mutable_static);
    return true;
}

template <bool const_dynamic, bool const_static, bool mutable_dynamic, bool mutable_static, typename Arg>
constexpr auto test_explicit() -> bool {
    static_assert(!std::is_convertible_v<Arg, byte_view>);
    static_assert(!std::is_convertible_v<Arg, byte_span<4>>);
    static_assert(!std::is_convertible_v<Arg, mut_byte_view>);
    static_assert(!std::is_convertible_v<Arg, mut_byte_span<4>>);
    static_assert(std::is_constructible_v<byte_view, Arg> == const_dynamic);
    static_assert(std::is_constructible_v<byte_span<4>, Arg> == const_static);
    static_assert(std::is_constructible_v<mut_byte_view, Arg> == mutable_dynamic);
    static_assert(std::is_constructible_v<mut_byte_span<4>, Arg> == mutable_static);
    return true;
}

// basic_byte_span<T, N> -> basic_byte_span<U, M>
static_assert(test_implicit<true, false, true, false, mut_byte_span<> &>());
static_assert(test_implicit<true, false, true, false, const mut_byte_span<> &>());
static_assert(test_implicit<true, false, true, false, mut_byte_span<> &&>());
static_assert(test_implicit<true, true, true, true, mut_byte_span<4> &>());
static_assert(test_implicit<true, true, true, true, const mut_byte_span<4> &>());
static_assert(test_implicit<true, true, true, true, mut_byte_span<4> &&>());
static_assert(test_implicit<true, false, true, false, mut_byte_span<2> &>());
static_assert(test_implicit<true, false, true, false, const mut_byte_span<2> &>());
static_assert(test_implicit<true, false, true, false, mut_byte_span<2> &&>());
static_assert(test_implicit<true, false, false, false, byte_span<> &>());
static_assert(test_implicit<true, false, false, false, const byte_span<> &>());
static_assert(test_implicit<true, false, false, false, byte_span<> &&>());
static_assert(test_implicit<true, true, false, false, byte_span<4> &>());
static_assert(test_implicit<true, true, false, false, const byte_span<4> &>());
static_assert(test_implicit<true, true, false, false, byte_span<4> &&>());
static_assert(test_implicit<true, false, false, false, byte_span<2> &>());
static_assert(test_implicit<true, false, false, false, const byte_span<2> &>());
static_assert(test_implicit<true, false, false, false, byte_span<2> &&>());

// Pointer & size
static_assert(test<true, true, false, false, const std::byte *, std::size_t>());
static_assert(test<true, true, true, true, std::byte *, std::size_t>());

// C array
static_assert(test_implicit<true, true, true, true, std::byte (&)[4]>());
static_assert(test_implicit<true, false, true, false, std::byte (&)[2]>());
static_assert(test_implicit<true, true, true, true, char (&)[4]>());
static_assert(test_implicit<true, false, true, false, char (&)[2]>());

// const C array
static_assert(test_implicit<true, true, false, false, const std::byte (&)[4]>());
static_assert(test_implicit<true, false, false, false, const std::byte (&)[2]>());
static_assert(test_implicit<false, false, false, false, const char (&)[4]>());
static_assert(test_implicit<false, false, false, false, const char (&)[2]>());
static_assert(test_explicit<true, true, false, false, const char (&)[4]>());
static_assert(test_explicit<true, false, false, false, const char (&)[2]>());

// C array
static_assert(test_implicit<true, true, true, true, std::byte (&&)[4]>());
static_assert(test_implicit<true, false, true, false, std::byte (&&)[2]>());
static_assert(test_implicit<true, true, true, true, char (&&)[4]>());
static_assert(test_implicit<true, false, true, false, char (&&)[2]>());

// string literal
static_assert(test_implicit<false, false, false, false, decltype("foo")>());
static_assert(test_implicit<false, false, false, false, decltype("f")>());
static_assert(test_explicit<true, true, false, false, decltype("foo")>());
static_assert(test_explicit<true, false, false, false, decltype("f")>());

// std::array
static_assert(test_implicit<true, true, true, true, std::array<std::byte, 4> &>());
static_assert(test_implicit<true, false, true, false, std::array<std::byte, 2> &>());
static_assert(test_implicit<true, true, false, false, const std::array<std::byte, 4> &>());
static_assert(test_implicit<true, false, false, false, const std::array<std::byte, 2> &>());
static_assert(test_implicit<true, true, true, true, std::array<std::byte, 4> &&>());
static_assert(test_implicit<true, false, true, false, std::array<std::byte, 2> &&>());

// Container
static_assert(test_implicit<true, false, true, false, std::vector<std::byte> &>());
static_assert(test_implicit<true, false, false, false, const std::vector<std::byte> &>());
static_assert(test_implicit<true, false, true, false, std::vector<std::byte> &&>());

// nullptr
static_assert(test_implicit<true, false, true, false, std::nullptr_t>());

// std::string_view
static_assert(test_implicit<true, false, false, false, std::string_view &>());
static_assert(test_implicit<true, false, false, false, const std::string_view &>());
static_assert(test_implicit<true, false, false, false, std::string_view &&>());

template <typename T>
struct myspan {
    static constexpr std::size_t extent = dynamic_extent;
    auto size_bytes() noexcept -> std::size_t;
    template <std::size_t, std::size_t>
    auto subspan() -> myspan;
    auto data() -> T *;
};
static_assert(is_span_like_v<myspan<char>>);
static_assert(test_implicit<true, false, true, false, myspan<char> &>());
static_assert(test_implicit<true, false, true, false, const myspan<char> &>());
static_assert(test_implicit<true, false, true, false, myspan<char> &&>());
static_assert(test_implicit<true, false, false, false, myspan<const char> &>());
static_assert(test_implicit<true, false, false, false, const myspan<const char> &>());
static_assert(test_implicit<true, false, false, false, myspan<const char> &&>());
struct foo {};
static_assert(test<false, false, false, false, myspan<foo> &>());
static_assert(test<false, false, false, false, const myspan<foo> &>());
static_assert(test<false, false, false, false, myspan<foo> &&>());
static_assert(test<false, false, false, false, myspan<const foo> &>());
static_assert(test<false, false, false, false, const myspan<const foo> &>());
static_assert(test<false, false, false, false, myspan<const foo> &&>());

// is_byte_like
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

// is_compatible
static_assert(is_compatible_v<char, const std::byte>);
static_assert(is_compatible_v<void, const std::byte>);
static_assert(is_compatible_v<std::byte, const std::byte>);
static_assert(is_compatible_v<const char, const std::byte>);
static_assert(is_compatible_v<const void, const std::byte>);
static_assert(is_compatible_v<const std::byte, const std::byte>);
static_assert(is_compatible_v<char, std::byte>);
static_assert(is_compatible_v<void, std::byte>);
static_assert(is_compatible_v<std::byte, std::byte>);
static_assert(!is_compatible_v<const char, std::byte>);
static_assert(!is_compatible_v<const void, std::byte>);
static_assert(!is_compatible_v<const std::byte, std::byte>);

enum class TestEnum { U8 };

// is_container
static_assert(!is_container_v<const TestEnum &>);
static_assert(!is_container_v<std::byte (&)[4]>);
static_assert(!is_container_v<const std::byte (&)[4]>);

static_assert(!is_container_compatible_v<const TestEnum &, const std::byte>);

// is_container_compatible
static_assert(is_container_compatible_v<std::vector<char> &, std::byte>);
static_assert(!is_container_compatible_v<const std::vector<char> &, std::byte>);
static_assert(is_container_compatible_v<std::vector<char> &, const std::byte>);
static_assert(is_container_compatible_v<const std::vector<char> &, const std::byte>);

static_assert(!std::is_const_v<container_element_type<std::vector<char>>>);
static_assert(is_container_compatible_v<std::vector<char>, std::byte>);
static_assert(!is_container_compatible_v<const std::vector<char>, std::byte>);
static_assert(is_container_compatible_v<std::vector<char>, const std::byte>);
static_assert(is_container_compatible_v<const std::vector<char>, const std::byte>);

static_assert(is_container_compatible_v<const std::byte (&)[4], const std::byte>);
static_assert(!is_container_compatible_v<const std::byte (&)[4], std::byte>);
static_assert(is_container_compatible_v<std::byte (&)[4], std::byte>);
static_assert(is_container_compatible_v<std::byte (&)[4], const std::byte>);
static_assert(is_container_compatible_v<const char (&)[4], const std::byte>);
static_assert(!is_container_compatible_v<const char (&)[4], std::byte>);
static_assert(is_container_compatible_v<char (&)[4], std::byte>);
static_assert(is_container_compatible_v<char (&)[4], const std::byte>);

// byte_cast
static_assert(std::is_same_v<decltype(byte_cast<char>(std::declval<const std::byte *>())), const char *>);
static_assert(std::is_same_v<decltype(byte_cast<char>(std::declval<std::byte *>())), char *>);

// member functions
static_assert(std::is_same_v<decltype(std::declval<mut_byte_view>().u8data()), uint8_t *>);
static_assert(std::is_same_v<decltype(std::declval<mut_byte_view>().cdata()), char *>);
static_assert(std::is_same_v<decltype(std::declval<mut_byte_view>().ucdata()), unsigned char *>);
static_assert(std::is_same_v<decltype(std::declval<mut_byte_view>().begin<uint8_t>()), uint8_t *>);
static_assert(std::is_same_v<decltype(std::declval<mut_byte_view>().begin<char>()), char *>);
static_assert(std::is_same_v<decltype(std::declval<mut_byte_view>().begin<unsigned char>()), unsigned char *>);

static_assert(std::is_same_v<decltype(std::declval<byte_view>().u8data()), const uint8_t *>);
static_assert(std::is_same_v<decltype(std::declval<byte_view>().cdata()), const char *>);
static_assert(std::is_same_v<decltype(std::declval<byte_view>().ucdata()), const unsigned char *>);
static_assert(std::is_same_v<decltype(std::declval<byte_view>().begin<uint8_t>()), const uint8_t *>);
static_assert(std::is_same_v<decltype(std::declval<byte_view>().begin<char>()), const char *>);
static_assert(std::is_same_v<decltype(std::declval<byte_view>().begin<unsigned char>()), const unsigned char *>);

} // namespace mcpp::byte_span::detail

// runtime tests
auto main() -> int {
}