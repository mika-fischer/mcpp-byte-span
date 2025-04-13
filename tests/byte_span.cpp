// Copyright Mika Fischer 2021.
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE.txt or copy at https://www.boost.org/LICENSE_1_0.txt)

#include "mcpp/byte_span/byte_span.hpp"

#include <compare>
#include <span>
#include <string_view>
#include <type_traits>
#include <vector>

namespace mcpp::byte_span::detail {

namespace {
// dynamic_extent
static_assert(std::same_as<decltype(dynamic_extent), decltype(std::dynamic_extent)>);
static_assert(dynamic_extent == std::dynamic_extent);

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
concept test = requires {
    requires std::is_constructible_v<byte_view, Args...> == const_dynamic;
    requires std::is_constructible_v<byte_span<4>, Args...> == const_static;
    requires std::is_constructible_v<mut_byte_view, Args...> == mutable_dynamic;
    requires std::is_constructible_v<mut_byte_span<4>, Args...> == mutable_static;
};

template <bool const_dynamic, bool const_static, bool mutable_dynamic, bool mutable_static, typename Arg>
concept test_implicit = requires {
    requires std::convertible_to<Arg, byte_view> == const_dynamic;
    requires std::convertible_to<Arg, byte_span<4>> == const_static;
    requires std::convertible_to<Arg, mut_byte_view> == mutable_dynamic;
    requires std::convertible_to<Arg, mut_byte_span<4>> == mutable_static;
};

template <bool const_dynamic, bool const_static, bool mutable_dynamic, bool mutable_static, typename Arg>
concept test_explicit = requires {
    requires !std::convertible_to<Arg, byte_view>;
    requires !std::convertible_to<Arg, byte_span<4>>;
    requires !std::convertible_to<Arg, mut_byte_view>;
    requires !std::convertible_to<Arg, mut_byte_span<4>>;
    requires std::is_constructible_v<byte_view, Arg> == const_dynamic;
    requires std::is_constructible_v<byte_span<4>, Arg> == const_static;
    requires std::is_constructible_v<mut_byte_view, Arg> == mutable_dynamic;
    requires std::is_constructible_v<mut_byte_span<4>, Arg> == mutable_static;
};

// basic_byte_span<T, N> -> basic_byte_span<U, M>
static_assert(test_implicit<true, false, true, false, mut_byte_span<> &>);
static_assert(test_implicit<true, false, true, false, const mut_byte_span<> &>);
static_assert(test_implicit<true, false, true, false, mut_byte_span<> &&>);
static_assert(test_implicit<true, true, true, true, mut_byte_span<4> &>);
static_assert(test_implicit<true, true, true, true, const mut_byte_span<4> &>);
static_assert(test_implicit<true, true, true, true, mut_byte_span<4> &&>);
static_assert(test_implicit<true, false, true, false, mut_byte_span<2> &>);
static_assert(test_implicit<true, false, true, false, const mut_byte_span<2> &>);
static_assert(test_implicit<true, false, true, false, mut_byte_span<2> &&>);
static_assert(test_implicit<true, false, false, false, byte_span<> &>);
static_assert(test_implicit<true, false, false, false, const byte_span<> &>);
static_assert(test_implicit<true, false, false, false, byte_span<> &&>);
static_assert(test_implicit<true, true, false, false, byte_span<4> &>);
static_assert(test_implicit<true, true, false, false, const byte_span<4> &>);
static_assert(test_implicit<true, true, false, false, byte_span<4> &&>);
static_assert(test_implicit<true, false, false, false, byte_span<2> &>);
static_assert(test_implicit<true, false, false, false, const byte_span<2> &>);
static_assert(test_implicit<true, false, false, false, byte_span<2> &&>);

// Pointer & size
static_assert(test<true, true, false, false, const std::byte *, std::size_t>);
static_assert(test<true, true, true, true, std::byte *, std::size_t>);

// C array
static_assert(test_implicit<true, true, true, true, std::byte (&)[4]>);
static_assert(test_implicit<true, false, true, false, std::byte (&)[2]>);
static_assert(test_implicit<true, true, true, true, char (&)[4]>);
static_assert(test_implicit<true, false, true, false, char (&)[2]>);

// const C array
static_assert(test_implicit<true, true, false, false, const std::byte (&)[4]>);
static_assert(test_implicit<true, false, false, false, const std::byte (&)[2]>);
static_assert(test_implicit<false, false, false, false, const char (&)[4]>);
static_assert(test_implicit<false, false, false, false, const char (&)[2]>);
static_assert(test_explicit<true, true, false, false, const char (&)[4]>);
static_assert(test_explicit<true, false, false, false, const char (&)[2]>);

// C array
static_assert(test_implicit<true, true, true, true, std::byte (&&)[4]>);
static_assert(test_implicit<true, false, true, false, std::byte (&&)[2]>);
static_assert(test_implicit<true, true, true, true, char (&&)[4]>);
static_assert(test_implicit<true, false, true, false, char (&&)[2]>);

// string literal
static_assert(test_implicit<false, false, false, false, decltype("foo")>);
static_assert(test_implicit<false, false, false, false, decltype("f")>);
static_assert(test_explicit<true, true, false, false, decltype("foo")>);
static_assert(test_explicit<true, false, false, false, decltype("f")>);

// std::array
static_assert(test_implicit<true, true, true, true, std::array<std::byte, 4> &>);
static_assert(test_implicit<true, false, true, false, std::array<std::byte, 2> &>);
static_assert(test_implicit<true, true, false, false, const std::array<std::byte, 4> &>);
static_assert(test_implicit<true, false, false, false, const std::array<std::byte, 2> &>);
static_assert(test_implicit<true, true, true, true, std::array<std::byte, 4> &&>);
static_assert(test_implicit<true, false, true, false, std::array<std::byte, 2> &&>);

// Container
static_assert(test_implicit<true, false, true, false, std::vector<std::byte> &>);
static_assert(test_implicit<true, false, false, false, const std::vector<std::byte> &>);
static_assert(test_implicit<true, false, true, false, std::vector<std::byte> &&>);

// nullptr
static_assert(test_implicit<true, false, true, false, std::nullptr_t>);

// std::string_view
static_assert(test_implicit<true, false, false, false, std::string_view &>);
static_assert(test_implicit<true, false, false, false, const std::string_view &>);
static_assert(test_implicit<true, false, false, false, std::string_view &&>);

template <typename T>
struct myspan {
    static constexpr std::size_t extent = dynamic_extent;
    auto size_bytes() noexcept -> std::size_t;
    template <std::size_t, std::size_t>
    auto subspan() -> myspan;
    auto data() -> T * { return nullptr; };
    [[nodiscard]] auto size() const -> std::size_t;
    auto begin() -> T * { return nullptr; }
    auto end() -> T * { return nullptr; };
};
static_assert(span_like<myspan<char>>);
static_assert(span_like<std::span<char>>);
static_assert(test_implicit<true, false, true, false, std::span<char> &>);
static_assert(test_implicit<true, false, true, false, const std::span<char> &>);
static_assert(test_implicit<true, false, true, false, std::span<char> &&>);
static_assert(test_implicit<true, false, false, false, std::span<const char> &>);
static_assert(test_implicit<true, false, false, false, const std::span<const char> &>);
static_assert(test_implicit<true, false, false, false, std::span<const char> &&>);
static_assert(test<false, false, false, false, std::span<int> &>);
static_assert(test<false, false, false, false, const std::span<int> &>);
static_assert(test<false, false, false, false, std::span<int> &&>);
static_assert(test<false, false, false, false, std::span<const int> &>);
static_assert(test<false, false, false, false, const std::span<const int> &>);
static_assert(test<false, false, false, false, std::span<const int> &&>);

// byte_like
static_assert(byte_like<char>);
static_assert(byte_like<unsigned char>);
static_assert(byte_like<uint8_t>);
static_assert(byte_like<std::byte>);
static_assert(!byte_like<void>);
static_assert(!byte_like<signed char>);
static_assert(!byte_like<int8_t>);
static_assert(!byte_like<short>);
static_assert(!byte_like<char &>);
static_assert(!byte_like<const char &>);
static_assert(!byte_like<char *>);
static_assert(!byte_like<const char *>);

// byte_aliasable
static_assert(byte_aliasable<char, const std::byte>);
static_assert(byte_aliasable<std::byte, const std::byte>);
static_assert(byte_aliasable<const char, const std::byte>);
static_assert(byte_aliasable<const std::byte, const std::byte>);
static_assert(byte_aliasable<char, std::byte>);
static_assert(byte_aliasable<std::byte, std::byte>);
static_assert(!byte_aliasable<const char, std::byte>);
static_assert(!byte_aliasable<const void, std::byte>);
static_assert(!byte_aliasable<const std::byte, std::byte>);
static_assert(!byte_aliasable<void, const std::byte>);
static_assert(!byte_aliasable<const void, const std::byte>);
static_assert(!byte_aliasable<void, std::byte>);

using R = std::vector<char>;

// container_element_t
static_assert(std::same_as<container_element_t<R>, char>);
static_assert(std::same_as<container_element_t<const R>, const char>);
static_assert(std::same_as<container_element_t<R &>, char>);
static_assert(std::same_as<container_element_t<const R &>, const char>);
static_assert(std::same_as<container_element_t<R &&>, char>);
static_assert(std::same_as<container_element_t<const R &&>, const char>);

// extent
static_assert(extent<char[4]>() == 4);
static_assert(extent<char (&)[4]>() == 4);
static_assert(extent<char (&&)[4]>() == 4);

static_assert(extent<std::span<char, 4>>() == 4);
static_assert(extent<const std::span<char, 4> &>() == 4);
static_assert(extent<std::span<char>>() == dynamic_extent);
static_assert(extent<const std::span<char> &>() == dynamic_extent);

static_assert(extent<std::array<char, 4>>() == 4);
static_assert(extent<const std::array<char, 4> &>() == 4);

static_assert(extent<std::vector<char>>() == dynamic_extent);

// byte_aliasable_range
static_assert(byte_aliasable_range<R &, std::byte>);
static_assert(!byte_aliasable_range<const R &, std::byte>);
static_assert(byte_aliasable_range<R &, const std::byte>);
static_assert(byte_aliasable_range<const R &, const std::byte>);

static_assert(std::same_as<detail::container_element_t<R>, char>);
static_assert(byte_aliasable_range<R, std::byte>);
static_assert(!byte_aliasable_range<const R, std::byte>);
static_assert(byte_aliasable_range<R, const std::byte>);
static_assert(byte_aliasable_range<const R, const std::byte>);

static_assert(byte_aliasable_range<const std::byte (&)[4], const std::byte>);
static_assert(!byte_aliasable_range<const std::byte (&)[4], std::byte>);
static_assert(byte_aliasable_range<std::byte (&)[4], std::byte>);
static_assert(byte_aliasable_range<std::byte (&)[4], const std::byte>);
static_assert(byte_aliasable_range<const char (&)[4], const std::byte>);
static_assert(!byte_aliasable_range<const char (&)[4], std::byte>);
static_assert(byte_aliasable_range<char (&)[4], std::byte>);
static_assert(byte_aliasable_range<char (&)[4], const std::byte>);

// byte_cast
static_assert(requires(const std::byte *cptr, std::byte *ptr) {
    { byte_cast<const char>(cptr) } -> std::same_as<const char *>;
    { byte_cast<const char>(ptr) } -> std::same_as<const char *>;
    { byte_cast<char>(cptr) } -> std::same_as<const char *>;
    { byte_cast<char>(ptr) } -> std::same_as<char *>;
});

// member functions
static_assert(requires(const mut_byte_view view) {
    { view.u8data() } -> std::same_as<uint8_t *>;
    { view.cdata() } -> std::same_as<char *>;
    { view.ucdata() } -> std::same_as<unsigned char *>;
    { view.begin<uint8_t>() } -> std::same_as<uint8_t *>;
    { view.begin<char>() } -> std::same_as<char *>;
    { view.begin<unsigned char>() } -> std::same_as<unsigned char *>;
    { view.begin<const uint8_t>() } -> std::same_as<const uint8_t *>;
    { view.begin<const char>() } -> std::same_as<const char *>;
    { view.begin<const unsigned char>() } -> std::same_as<const unsigned char *>;
});
static_assert(requires(const byte_view view) {
    { view.u8data() } -> std::same_as<const uint8_t *>;
    { view.cdata() } -> std::same_as<const char *>;
    { view.ucdata() } -> std::same_as<const unsigned char *>;
    { view.begin<uint8_t>() } -> std::same_as<const uint8_t *>;
    { view.begin<char>() } -> std::same_as<const char *>;
    { view.begin<unsigned char>() } -> std::same_as<const unsigned char *>;
    { view.begin<const uint8_t>() } -> std::same_as<const uint8_t *>;
    { view.begin<const char>() } -> std::same_as<const char *>;
    { view.begin<const unsigned char>() } -> std::same_as<const unsigned char *>;
});

static_assert(requires(byte_view a, mut_byte_view b, byte_span<4> c, mut_byte_span<4> d) {
    { a == a } -> std::same_as<bool>;
    { a == b } -> std::same_as<bool>;
    { a == c } -> std::same_as<bool>;
    { a == d } -> std::same_as<bool>;
    { b == c } -> std::same_as<bool>;
    { b == d } -> std::same_as<bool>;
    { c == d } -> std::same_as<bool>;
});

static_assert(requires(byte_view a, mut_byte_view b, byte_span<4> c, mut_byte_span<4> d) {
    { a <=> a } -> std::same_as<std::strong_ordering>;
    { a <=> b } -> std::same_as<std::strong_ordering>;
    { a <=> c } -> std::same_as<std::strong_ordering>;
    { a <=> d } -> std::same_as<std::strong_ordering>;
    { b <=> c } -> std::same_as<std::strong_ordering>;
    { b <=> d } -> std::same_as<std::strong_ordering>;
    { c <=> d } -> std::same_as<std::strong_ordering>;
});

} // namespace

} // namespace mcpp::byte_span::detail

// runtime tests
auto main() -> int {
}