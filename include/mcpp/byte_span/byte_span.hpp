// Copyright Mika Fischer 2021.
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE.txt or copy at https://www.boost.org/LICENSE_1_0.txt)

#pragma once

#include <algorithm>
#include <cstdint>
#include <string_view>
#include <type_traits>
#include <vector>

namespace mcpp::byte_span {

constexpr inline size_t dynamic_extent = SIZE_MAX;

template <typename T, size_t Extent = dynamic_extent>
class basic_byte_span;

namespace detail {

template <typename>
struct is_byte_span : std::false_type {};
template <typename T, size_t S>
struct is_byte_span<basic_byte_span<T, S>> : std::true_type {};
template <typename T>
constexpr inline bool is_byte_span_v = is_byte_span<T>::value;

template <typename>
struct is_std_array : std::false_type {};
template <typename T, size_t N>
struct is_std_array<std::array<T, N>> : std::true_type {};
template <typename T>
constexpr inline bool is_std_array_v = is_std_array<T>::value;

template <typename>
struct is_std_string_view : std::false_type {};
template <typename CharT, typename Traits>
struct is_std_string_view<std::basic_string_view<CharT, Traits>> : std::true_type {};
template <typename T>
constexpr inline bool is_std_string_view_v = is_std_string_view<T>::value;

template <typename>
struct is_std_span : std::false_type {};
// TODO
// template <typename T, size_t Extent>
// struct is_std_span<vstd::span<T, Extent>> : std::true_type {};
template <typename T>
constexpr inline bool is_std_span_v = is_std_span<T>::value;

template <typename, typename = void>
struct has_size_and_data : std::false_type {};
template <typename T>
struct has_size_and_data<T, std::void_t<decltype(std::size(std::declval<T>())), decltype(std::data(std::declval<T>()))>>
    : std::true_type {};
template <typename T>
constexpr inline bool has_size_and_data_v = has_size_and_data<T>::value;

template <typename Container>
using container_element_type =
    std::remove_pointer_t<decltype(std::data(std::declval<std::add_lvalue_reference_t<Container>>()))>;

template <typename T, typename U = std::remove_cv_t<T>>
constexpr inline bool is_byte_like_v = std::is_same_v<U, std::byte> || std::is_same_v<U, char> ||
                                       std::is_same_v<U, unsigned char> || std::is_same_v<U, void>;

template <typename Container, typename U = std::remove_cv_t<std::remove_reference_t<Container>>>
constexpr inline bool is_container_v = !is_byte_span_v<U> && !std::is_array_v<U> && !is_std_array_v<U> &&
                                       !is_std_string_view_v<U> && !is_std_span_v<U> && has_size_and_data_v<Container>;

template <typename T, typename U>
constexpr bool is_compatible_v = is_byte_like_v<T> &&is_byte_like_v<U> && (std::is_const_v<U> || !std::is_const_v<T>);

template <typename Container, typename U, typename = void>
struct is_container_compatible : std::false_type {};
template <typename Container, typename U>
struct is_container_compatible<Container, U, std::enable_if_t<is_compatible_v<container_element_type<Container>, U>>>
    : std::true_type {};
template <typename Container, typename U>
constexpr bool is_container_compatible_v = is_container_compatible<Container, U>::value;

template <typename T, size_t Extent>
class basic_byte_span_storage {
  private:
    T *ptr_;

  public:
    constexpr basic_byte_span_storage(T *ptr, size_t sz = Extent) noexcept : ptr_(ptr) { static_assert(sz == Extent); }
    constexpr auto data() const noexcept -> T * { return ptr_; }
    constexpr auto size() const noexcept -> size_t { return Extent; }
};
template <typename T>
class basic_byte_span_storage<T, 0> {
  public:
    constexpr basic_byte_span_storage(T *ptr = nullptr, size_t sz = 0) noexcept {
        // TODO: Configurable terminate?
        // static_assert(sz == 0ull);
    }
    constexpr auto data() const noexcept -> T * { return nullptr; }
    constexpr auto size() const noexcept -> size_t { return 0; }
};
template <typename T>
class basic_byte_span_storage<T, dynamic_extent> {
  private:
    T *ptr_;
    size_t size_;

  public:
    constexpr basic_byte_span_storage(T *ptr = nullptr, size_t sz = 0) noexcept : ptr_(ptr), size_(sz) {}
    constexpr auto data() const noexcept -> T * { return ptr_; }
    constexpr auto size() const noexcept -> size_t { return size_; }
};

} // namespace detail

template <typename T, typename U, std::enable_if_t<detail::is_byte_like_v<T> && detail::is_byte_like_v<U>, int> = 0>
constexpr auto byte_cast(const U *p) noexcept -> const T * {
    return static_cast<const T *>(static_cast<const void *>(p));
}

template <typename T, typename U, std::enable_if_t<detail::is_byte_like_v<T> && detail::is_byte_like_v<U>, int> = 0>
constexpr auto byte_cast(U *p) noexcept -> T * {
    return static_cast<T *>(static_cast<void *>(p));
}

template <typename T, size_t Extent>
class basic_byte_span : public detail::basic_byte_span_storage<T, Extent> {
  private:
    static_assert(std::is_same_v<std::remove_cv_t<T>, std::byte>);
    using storage_type = detail::basic_byte_span_storage<T, Extent>;

  public:
    // Default
    template <size_t E = Extent, std::enable_if_t<(E == dynamic_extent || E <= 0), int> = 0>
    constexpr basic_byte_span() noexcept {}

    // Nullptr
    template <size_t E = Extent, std::enable_if_t<(E == dynamic_extent || E <= 0), int> = 0>
    constexpr basic_byte_span(std::nullptr_t) noexcept {}

    template <size_t E = Extent, std::enable_if_t<(E == dynamic_extent || E <= 0), int> = 0>
    constexpr basic_byte_span(std::nullptr_t, size_t /*size*/) noexcept {
        // TODO: What to do if size != 0?
    }

    // Pointer & size
    template <typename U, std::enable_if_t<detail::is_compatible_v<std::remove_pointer_t<U>, T>, int> = 0>
    constexpr basic_byte_span(U *ptr, size_t count) noexcept : storage_type(byte_cast<std::byte>(ptr), count) {}

    // Two pointers
    template <typename U, std::enable_if_t<detail::is_compatible_v<std::remove_pointer_t<U>, T>, int> = 0>
    constexpr basic_byte_span(U *first, U *last) noexcept : storage_type(first, last - first) {}

    // C bounded array (except of char, because this is error-prone)
    template <typename U, size_t N, size_t E = Extent,
              std::enable_if_t<!std::is_same_v<std::remove_cv_t<U>, char> && (E == dynamic_extent || N == E) &&
                                   detail::is_container_compatible_v<U (&)[N], T>,
                               int> = 0>
    constexpr basic_byte_span(U (&arr)[N]) noexcept : storage_type(byte_cast<std::byte>(arr), N) {}

    // std::string_view
    template <typename U = T, size_t E = Extent, std::enable_if_t<std::is_const_v<U> && E == dynamic_extent, int> = 0>
    constexpr basic_byte_span(std::string_view sv) noexcept
        : storage_type(byte_cast<std::byte>(sv.data()), sv.size()) {}

    // char pointer
    template <size_t E = Extent, std::enable_if_t<E == dynamic_extent, int> = 0>
    constexpr basic_byte_span(const char *str) noexcept : storage_type(byte_cast<std::byte>(str), std::strlen(str)) {}

    // std::array
    template <typename U, size_t N, size_t E = Extent,
              std::enable_if_t<
                  (E == dynamic_extent || N == E) && detail::is_container_compatible_v<std::array<U, N> &, T>, int> = 0>
    constexpr basic_byte_span(std::array<U, N> &arr) noexcept : storage_type(byte_cast<std::byte>(arr.data()), N) {}

    // const std::array
    template <
        typename U, size_t N, size_t E = Extent,
        std::enable_if_t<
            (E == dynamic_extent || N == E) && detail::is_container_compatible_v<const std::array<U, N> &, T>, int> = 0>
    constexpr basic_byte_span(const std::array<U, N> &arr) noexcept
        : storage_type(byte_cast<std::byte>(arr.data()), N) {}

    // generic container
    template <typename Container, size_t E = Extent,
              std::enable_if_t<E == dynamic_extent && detail::is_container_v<Container> &&
                                   detail::is_container_compatible_v<Container &, T>,
                               int> = 0>
    constexpr basic_byte_span(Container &cont) : storage_type(byte_cast<std::byte>(std::data(cont)), std::size(cont)) {}

    // const generic container
    template <typename Container, size_t E = Extent,
              std::enable_if_t<E == dynamic_extent && detail::is_container_v<Container> &&
                                   detail::is_container_compatible_v<const Container &, T>,
                               int> = 0>
    constexpr basic_byte_span(const Container &cont)
        : storage_type(byte_cast<std::byte>(std::data(cont)), std::size(cont)) {}

    // span
    // TODO
    // template <
    //     typename U, size_t E2, size_t E1 = Extent,
    //     std::enable_if_t<
    //         (E1 == dynamic_extent || E1 == E2) && detail::is_container_compatible_v<vstd::span<U, E2> &, T>, int> =
    //         0>
    // constexpr basic_byte_span(vstd::span<U, E2> s) noexcept : storage_type(byte_cast<std::byte>(s.data()), s.size())
    // {}

    using storage_type::data;
    using storage_type::size;

    constexpr auto size_bytes() const noexcept -> size_t { return size(); }
    constexpr auto empty() const noexcept -> bool { return size() == 0; }

    template <typename U, std::enable_if_t<detail::is_compatible_v<U, T>, int> = 0>
    constexpr auto data() const noexcept {
        return byte_cast<U>(data());
    }

    constexpr auto u8data() const noexcept { return byte_cast<uint8_t>(data()); }
    constexpr auto cdata() const noexcept { return byte_cast<char>(data()); }
    constexpr auto ucdata() const noexcept { return byte_cast<unsigned char>(data()); }

    constexpr auto begin() const noexcept -> T * { return data(); }
    template <typename U, std::enable_if_t<detail::is_compatible_v<U, T>, int> = 0>
    constexpr auto begin() const noexcept {
        return byte_cast<U>(begin());
    }

    constexpr auto end() const noexcept -> T * { return data() + size(); }
    template <typename U, std::enable_if_t<detail::is_compatible_v<U, T>, int> = 0>
    constexpr auto end() const noexcept {
        return byte_cast<U>(end());
    }

    constexpr auto operator[](size_t offset) const noexcept -> T & { return data()[offset]; }

    template <size_t Count>
    constexpr auto first() const noexcept -> basic_byte_span<T, Count> {
        return {data(), Count};
    }
    constexpr auto first(size_t count) const noexcept -> basic_byte_span<T, dynamic_extent> { return {data(), count}; }

    template <size_t Count>
    constexpr auto last() const noexcept -> basic_byte_span<T, Count> {
        return {data() + (size() - Count), Count};
    }
    constexpr auto last(size_t count) const noexcept -> basic_byte_span<T, dynamic_extent> {
        return {data() + (size() - count), count};
    }

    template <size_t Offset, size_t Count = dynamic_extent>
    constexpr auto subspan() const noexcept {
        return basic_byte_span{data() + Offset, Count != dynamic_extent ? Count : size() - Offset};
    }
    constexpr auto subspan(size_t offset, size_t count = dynamic_extent) const noexcept
        -> basic_byte_span<T, dynamic_extent> {
        return {data() + offset, count == dynamic_extent ? size() - offset : count};
    }

    friend auto operator==(basic_byte_span a, basic_byte_span b) -> bool {
        return a.size() == b.size() && std::memcmp(a.data(), b.data(), a.size()) == 0;
    }

    template <size_t E2>
    friend auto operator==(basic_byte_span a, basic_byte_span<const std::byte, E2> b) -> bool {
        return a.size() == b.size() && std::memcmp(a.data(), b.data(), a.size()) == 0;
    }

    friend auto operator!=(basic_byte_span a, basic_byte_span b) -> bool { return !(a == b); }

    template <size_t E2>
    friend auto operator!=(basic_byte_span a, basic_byte_span<const std::byte, E2> b) -> bool {
        return !(a == b);
    }

    friend auto operator<(basic_byte_span a, basic_byte_span b) -> bool {
        return std::lexicographical_compare(a.template begin<uint8_t>(), a.template end<uint8_t>(),
                                            b.template begin<uint8_t>(), b.template end<uint8_t>());
    }

    template <size_t E2>
    friend auto operator<(basic_byte_span a, basic_byte_span<const std::byte, E2> b) -> bool {
        return std::lexicographical_compare(a.template begin<uint8_t>(), a.template end<uint8_t>(),
                                            b.template begin<uint8_t>(), b.template end<uint8_t>());
    }

    friend auto operator>(basic_byte_span a, basic_byte_span b) -> bool { return b < a; }

    template <size_t E2 = dynamic_extent>
    friend auto operator>(basic_byte_span a, basic_byte_span<const std::byte, E2> b) -> bool {
        return b < a;
    }

    friend auto operator<=(basic_byte_span a, basic_byte_span b) -> bool { return !(a > b); }

    template <size_t E2 = dynamic_extent>
    friend auto operator<=(basic_byte_span a, basic_byte_span<const std::byte, E2> b) -> bool {
        return !(a > b);
    }

    friend auto operator>=(basic_byte_span a, basic_byte_span b) -> bool { return !(a < b); }

    template <size_t E2 = dynamic_extent>
    friend auto operator>=(basic_byte_span a, basic_byte_span<const std::byte, E2> b) -> bool {
        return !(a < b);
    }
};

// TODO: Names for:
// - basic_byte_span<const std::byte, dynamic_extent>
// - basic_byte_span<cstd::byte, dynamic_extent>
// - template <size_t N> basic_byte_span<const std::byte, N>
// - template <size_t N> basic_byte_span<cstd::byte, N>
using byte_span = basic_byte_span<const std::byte>;
using mut_byte_span = basic_byte_span<std::byte>;

constexpr auto operator"" _bv(const char *str, std::size_t len) -> byte_span {
    return byte_span{str, len};
}

// TODO: Should this be here?
// template <typename T, size_t Extent>
// auto as_byte_span(vstd::span<T, Extent> s) noexcept
//     -> basic_byte_span<const std::byte, (Extent == dynamic_extent) ? dynamic_extent : sizeof(T) * Extent> {
//     return {vstd::as_bytes(s)};
// }

// template <class T, size_t Extent, typename std::enable_if<!std::is_const<T>::value, int>::type = 0>
// auto as_mut_byte_span(vstd::span<T, Extent> s) noexcept
//     -> basic_byte_span<std::byte, (Extent == dynamic_extent) ? dynamic_extent : sizeof(T) * Extent> {
//     return {vstd::as_writable_bytes(s)};
// }

// inline auto to_bytes_vec(byte_span bytes) noexcept -> bytes_vec {
//     return {bytes.begin(), bytes.end()};
// }

constexpr auto as_string_view(byte_span bytes) noexcept -> std::string_view {
    return {bytes.cdata(), bytes.size()};
}

inline auto to_string(byte_span bytes) -> std::string {
    return {bytes.cdata(), bytes.size()};
}

auto to_hexstring(byte_span bytes) noexcept -> std::string;

auto to_asciistring(byte_span bytes) noexcept -> std::string;

} // namespace mcpp::byte_span