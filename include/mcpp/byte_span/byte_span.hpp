// Copyright Mika Fischer 2021.
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE.txt or copy at https://www.boost.org/LICENSE_1_0.txt)

#pragma once

#include <algorithm>
#include <cstdint>
#include <iterator>
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

template <typename, typename = void>
struct is_span_like : std::false_type {};
template <typename T>
struct is_span_like<T, std::void_t<decltype(T::extent), decltype(std::declval<T>().size_bytes()),
                                   decltype(std::declval<T>().template subspan<0, 1>())>> : std::true_type {};
template <typename T>
constexpr inline bool is_span_like_v = is_span_like<T>::value;

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
                                       !is_std_string_view_v<U> && !is_span_like_v<U> && has_size_and_data_v<Container>;

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
    explicit constexpr basic_byte_span_storage(T *ptr, size_t /*sz*/ = Extent) noexcept : ptr_(ptr) {
        // TODO: Configurable terminate?
        // static_assert(sz == Extent);
    }
    [[nodiscard]] constexpr auto data() const noexcept -> T * { return ptr_; }
    [[nodiscard]] constexpr auto size() const noexcept -> size_t { return Extent; }
};
template <typename T>
class basic_byte_span_storage<T, 0> {
  public:
    explicit constexpr basic_byte_span_storage(T * /*ptr*/ = nullptr, size_t /*sz*/ = 0) noexcept {
        // TODO: Configurable terminate?
        // static_assert(sz == 0ull);
    }
    [[nodiscard]] constexpr auto data() const noexcept -> T * { return nullptr; }
    [[nodiscard]] constexpr auto size() const noexcept -> size_t { return 0; }
};
template <typename T>
class basic_byte_span_storage<T, dynamic_extent> {
  private:
    T *ptr_;
    size_t size_;

  public:
    explicit constexpr basic_byte_span_storage(T *ptr = nullptr, size_t sz = 0) noexcept : ptr_(ptr), size_(sz) {}
    [[nodiscard]] constexpr auto data() const noexcept -> T * { return ptr_; }
    [[nodiscard]] constexpr auto size() const noexcept -> size_t { return size_; }
};

constexpr auto subspan_extent(size_t offset, size_t count, size_t extent) -> size_t {
    if (count != dynamic_extent) {
        return count;
    }
    if (extent != dynamic_extent) {
        return extent - offset;
    }
    return dynamic_extent;
}

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
    // Member types
    using element_type = T;
    using value_type = std::remove_cv_t<T>;
    using size_type = std::size_t;
    using difference_type = std::ptrdiff_t;
    using pointer = T *;
    using const_pointer = const T *;
    using reference = T &;
    using const_reference = const T &;
    using iterator = pointer;
    using reverse_iterator = std::reverse_iterator<iterator>;

    // Member constant
    static constexpr std::size_t extent = Extent;

    // Member functions
    // Constructor (https://en.cppreference.com/w/cpp/container/span/span)
    // Default (1)
    template <size_t E = Extent, std::enable_if_t<E == 0 || E == dynamic_extent, int> = 0>
    constexpr basic_byte_span() noexcept {}

    // Pointer & size (2)
    template <typename U, std::enable_if_t<detail::is_compatible_v<std::remove_pointer_t<U>, T>, int> = 0>
    constexpr basic_byte_span(U *ptr, size_t count) noexcept : storage_type(byte_cast<std::byte>(ptr), count) {}

    // Two pointers (3)
    template <typename U, std::enable_if_t<detail::is_compatible_v<std::remove_pointer_t<U>, T>, int> = 0>
    constexpr basic_byte_span(U *first, U *last) noexcept : storage_type(first, last - first) {}

    // C bounded array (4)
    // (except of char, because this is error-prone)
    template <typename U, size_t N, size_t E = Extent,
              std::enable_if_t<!std::is_same_v<std::remove_cv_t<U>, char> && (N == E || E == dynamic_extent) &&
                                   detail::is_container_compatible_v<U (&)[N], T>,
                               int> = 0>
    constexpr basic_byte_span(U (&arr)[N]) noexcept : storage_type(byte_cast<std::byte>(arr), N) {}

    // std::array (5)
    template <typename U, size_t N, size_t E = Extent,
              std::enable_if_t<
                  (E == dynamic_extent || N == E) && detail::is_container_compatible_v<std::array<U, N> &, T>, int> = 0>
    constexpr basic_byte_span(std::array<U, N> &arr) noexcept : storage_type(byte_cast<std::byte>(arr.data()), N) {}

    // const std::array (6)
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

    ///////////////////////////////////////////////////////////////////////////
    // extra
    // nullptr
    template <size_t E = Extent, std::enable_if_t<(E == dynamic_extent || E <= 0), int> = 0>
    constexpr basic_byte_span(std::nullptr_t) noexcept {}

    template <size_t E = Extent, std::enable_if_t<(E == dynamic_extent || E <= 0), int> = 0>
    constexpr basic_byte_span(std::nullptr_t, size_t /*size*/) noexcept {
        // TODO: What to do if size != 0?
    }

    // std::string_view
    template <typename U = T, size_t E = Extent, std::enable_if_t<std::is_const_v<U> && E == dynamic_extent, int> = 0>
    constexpr basic_byte_span(std::string_view sv) noexcept
        : storage_type(byte_cast<std::byte>(sv.data()), sv.size()) {}

    // char pointer
    template <size_t E = Extent, std::enable_if_t<E == dynamic_extent, int> = 0>
    constexpr basic_byte_span(const char *str) noexcept : storage_type(byte_cast<std::byte>(str), std::strlen(str)) {}

    // span
    template <
        typename Span, size_t E1 = Extent,
        std::enable_if_t<!detail::is_byte_span_v<Span> && detail::is_span_like_v<Span> &&
                             (E1 == dynamic_extent || E1 == Span::extent) && detail::is_container_compatible_v<Span, T>,
                         int> = 0>
    constexpr basic_byte_span(Span s) noexcept : storage_type(byte_cast<std::byte>(s.data()), s.size()) {}

    // Iterators
    // begin (https://en.cppreference.com/w/cpp/container/span/begin)
    [[nodiscard]] constexpr auto begin() const noexcept -> iterator { return data(); }

    // end (https://en.cppreference.com/w/cpp/container/span/end)
    [[nodiscard]] constexpr auto end() const noexcept -> iterator { return data() + size(); }

    // rbegin (https://en.cppreference.com/w/cpp/container/span/rbegin)
    [[nodiscard]] constexpr auto rbegin() const noexcept -> reverse_iterator { return reverse_iterator(end()); }

    // rend (https://en.cppreference.com/w/cpp/container/span/rend)
    [[nodiscard]] constexpr auto rend() const noexcept -> reverse_iterator { return reverse_iterator(begin()); }

    // Element access
    // front (https://en.cppreference.com/w/cpp/container/span/front)
    [[nodiscard]] constexpr auto front() const noexcept -> reference { return *data(); }

    // back (https://en.cppreference.com/w/cpp/container/span/back)
    [[nodiscard]] constexpr auto back() const noexcept -> reference { return *(data() + (size() - 1)); }

    // operator[] (https://en.cppreference.com/w/cpp/container/span/operator_at)
    constexpr auto operator[](size_type idx) const noexcept -> reference { return data()[idx]; }

    // data (https://en.cppreference.com/w/cpp/container/span/data)
    using storage_type::data;

    // Observers
    // size (https://en.cppreference.com/w/cpp/container/span/size)
    using storage_type::size;

    // size_bytes (https://en.cppreference.com/w/cpp/container/span/size_bytes)
    [[nodiscard]] constexpr auto size_bytes() const noexcept -> size_type { return size() * sizeof(element_type); }

    // empty (https://en.cppreference.com/w/cpp/container/span/empty)
    [[nodiscard]] constexpr auto empty() const noexcept -> bool { return size() == 0; }

    // Subviews
    // first (https://en.cppreference.com/w/cpp/container/span/first)
    template <size_t Count>
    [[nodiscard]] constexpr auto first() const noexcept -> basic_byte_span<element_type, Count> {
        return {data(), Count};
    }
    [[nodiscard]] constexpr auto first(size_t Count) const noexcept -> basic_byte_span<element_type, dynamic_extent> {
        return {data(), Count};
    }

    // last (https://en.cppreference.com/w/cpp/container/span/last)
    template <size_t Count>
    [[nodiscard]] constexpr auto last() const noexcept -> basic_byte_span<element_type, Count> {
        return {data() + (size() - Count), Count};
    }
    [[nodiscard]] constexpr auto last(size_t Count) const noexcept -> basic_byte_span<element_type, dynamic_extent> {
        return {data() + (size() - Count), Count};
    }

    // subspan (https://en.cppreference.com/w/cpp/container/span/subspan)
    template <size_t Offset, size_t Count = dynamic_extent>
    [[nodiscard]] constexpr auto subspan() const noexcept
        -> basic_byte_span<element_type, detail::subspan_extent(Offset, Count, extent)> {
        return {data() + Offset, Count != dynamic_extent ? Count : size() - Offset};
    }
    [[nodiscard]] constexpr auto subspan(size_t Offset, size_t Count = dynamic_extent) const noexcept
        -> basic_byte_span<T, dynamic_extent> {
        return {data() + Offset, Count != dynamic_extent ? Count : size() - Offset};
    }

    ///////////////////////////////////////////////////////////////////////////
    // extra functionality

    template <typename U, std::enable_if_t<detail::is_compatible_v<U, T>, int> = 0>
    [[nodiscard]] constexpr auto begin() const noexcept {
        return byte_cast<U>(begin());
    }

    template <typename U, std::enable_if_t<detail::is_compatible_v<U, T>, int> = 0>
    [[nodiscard]] constexpr auto end() const noexcept {
        return byte_cast<U>(end());
    }

    template <typename U, std::enable_if_t<detail::is_compatible_v<U, T>, int> = 0>
    [[nodiscard]] constexpr auto rbegin() const noexcept {
        return std::reverse_iterator(end<U>());
    }

    template <typename U, std::enable_if_t<detail::is_compatible_v<U, T>, int> = 0>
    [[nodiscard]] constexpr auto rend() const noexcept {
        return std::reverse_iterator(begin<U>());
    }

    template <typename U, std::enable_if_t<detail::is_compatible_v<U, T>, int> = 0>
    constexpr auto data() const noexcept {
        return byte_cast<U>(data());
    }

    constexpr auto u8data() const noexcept { return byte_cast<uint8_t>(data()); }
    constexpr auto cdata() const noexcept { return byte_cast<char>(data()); }
    constexpr auto ucdata() const noexcept { return byte_cast<unsigned char>(data()); }

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
// using byte_span = basic_byte_span<const std::byte>;
// using mut_byte_span = basic_byte_span<std::byte>;

} // namespace mcpp::byte_span