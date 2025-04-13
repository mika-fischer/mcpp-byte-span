// Copyright Mika Fischer 2021.
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE.txt or copy at https://www.boost.org/LICENSE_1_0.txt)

#pragma once

#include <algorithm>   // equal, lexicographical_compare_three_way
#include <concepts>    // various
#include <cstddef>     // size_t, byte
#include <cstdint>     // uint8_t, SIZE_MAX
#include <iterator>    // data, size, reverse_iterator
#include <ranges>      // contiguous_range
#include <type_traits> // various
#include <utility>     // declval, forward

namespace mcpp::byte_span {

constexpr inline std::size_t dynamic_extent = SIZE_MAX;

template <typename T>
concept byte_like = std::same_as<T, std::byte> || std::same_as<T, char> || std::same_as<T, unsigned char>;

template <typename From, typename To>
concept byte_aliasable = byte_like<std::remove_const_t<From>> && byte_like<std::remove_const_t<To>> &&
                         (std::is_const_v<To> || !std::is_const_v<From>);

namespace detail {

template <std::ranges::contiguous_range T>
using container_element_t = std::remove_pointer_t<decltype(std::data(std::declval<T &>()))>;

template <std::ranges::contiguous_range T>
constexpr auto extent() noexcept -> std::size_t {
    using U = std::remove_cvref_t<T>;
    if constexpr (std::is_array_v<U>) {
        return std::extent_v<U>;
    } else if constexpr (requires { std::tuple_size<U>::value; }) {
        return std::tuple_size_v<U>;
    } else if constexpr (requires { U::extent; }) {
        return U::extent;
    } else {
        return dynamic_extent;
    }
}

template <typename T>
concept span_like = std::ranges::contiguous_range<T> && requires(T &span) {
    { T::extent } -> std::same_as<const std::size_t &>;
    { span.size_bytes() } -> std::same_as<std::size_t>;
    { span.template subspan<0, 0>() };
    { span.subspan(0, dynamic_extent) };
};

} // namespace detail

template <typename R, typename To>
concept byte_aliasable_range = std::ranges::contiguous_range<R> && byte_aliasable<detail::container_element_t<R>, To>;

namespace detail {

template <typename T, std::size_t Extent>
class basic_byte_span_storage {
  private:
    T *ptr_;

  public:
    explicit constexpr basic_byte_span_storage(T *ptr, std::size_t /*sz*/ = Extent) noexcept : ptr_(ptr) {
        // TODO: Configurable terminate?
        // static_assert(sz == Extent);
    }
    [[nodiscard]] constexpr auto data() const noexcept -> T * { return ptr_; }
    [[nodiscard]] constexpr auto size() const noexcept -> std::size_t { return Extent; }
};
template <typename T>
class basic_byte_span_storage<T, 0> {
  public:
    explicit constexpr basic_byte_span_storage(T * /*ptr*/ = nullptr, std::size_t /*sz*/ = 0) noexcept {
        // TODO: Configurable terminate?
        // static_assert(sz == 0ull);
    }
    [[nodiscard]] constexpr auto data() const noexcept -> T * { return nullptr; }
    [[nodiscard]] constexpr auto size() const noexcept -> std::size_t { return 0; }
};
template <typename T>
class basic_byte_span_storage<T, dynamic_extent> {
  private:
    T *ptr_;
    std::size_t size_;

  public:
    explicit constexpr basic_byte_span_storage(T *ptr = nullptr, std::size_t size = 0) noexcept
        : ptr_(ptr), size_(size) {}
    [[nodiscard]] constexpr auto data() const noexcept -> T * { return ptr_; }
    [[nodiscard]] constexpr auto size() const noexcept -> std::size_t { return size_; }
};

// NOLINTNEXTLINE(bugprone-easily-swappable-parameters)
constexpr auto subspan_extent(std::size_t offset, std::size_t count, std::size_t extent) noexcept -> std::size_t {
    if (count != dynamic_extent) {
        return count;
    }
    if (extent != dynamic_extent) {
        return extent - offset;
    }
    return dynamic_extent;
}

template <typename T, typename U>
using const_like_t = std::conditional_t<std::is_const_v<T>, const U, U>;

} // namespace detail

template <typename To, typename From>
    requires byte_aliasable<From, detail::const_like_t<From, To>>
[[nodiscard]] constexpr auto byte_cast(From *ptr) noexcept -> detail::const_like_t<From, To> * {
    return static_cast<detail::const_like_t<From, To> *>(static_cast<detail::const_like_t<From, void> *>(ptr));
}

template <typename T, std::size_t Extent>
    requires std::same_as<std::remove_const_t<T>, std::byte>
class basic_byte_span : public detail::basic_byte_span_storage<T, Extent> {
  private:
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
    using iterator = T *;
    using const_iterator = const T *;
    using reverse_iterator = std::reverse_iterator<T *>;
    using const_reverse_iterator = std::reverse_iterator<const T *>;

    // Member constant
    static constexpr std::size_t extent = Extent;

    // Member functions
    // Constructor (https://en.cppreference.com/w/cpp/container/span/span)
    // Default (1)
    constexpr basic_byte_span() noexcept
        requires(Extent == 0 || Extent == dynamic_extent)
    = default;

    // Pointer & size (2)
    template <byte_aliasable<T> U>
    constexpr basic_byte_span(U *ptr, std::size_t count) noexcept : storage_type(byte_cast<T>(ptr), count) {}

    // Two pointers (3)
    template <byte_aliasable<T> U>
    constexpr basic_byte_span(U *first, U *last) noexcept : storage_type(byte_cast<T>(first), last - first) {}

    // C bounded array (4)
    // (except for string literals, because this is error-prone due to the included zero-byte at the end)
    template <byte_aliasable<T> U, std::size_t N>
        requires(N == Extent || Extent == dynamic_extent)
    explicit(std::same_as<U, const char>) constexpr basic_byte_span(U (&arr)[N]) noexcept
        : storage_type(byte_cast<T>(arr), N) {
        static_assert(!std::same_as<std::remove_cvref_t<U>, char>);
    }

    template <byte_aliasable<T> U, std::size_t N>
        requires(N == Extent || Extent == dynamic_extent)
    // NOLINTNEXTLINE(hicpp-explicit-conversions)
    explicit(std::same_as<U, const char>) constexpr basic_byte_span(U (&&arr)[N]) noexcept
        : storage_type(byte_cast<std::byte>(std::move(arr)), N) {
        static_assert(!std::same_as<std::remove_cvref_t<U>, char>);
    }

    // generic contiguous range
    template <std::ranges::contiguous_range R>
        requires(Extent == dynamic_extent || detail::extent<R>() == Extent) &&
                (!std::is_array_v<std::remove_cvref_t<R>>) && byte_aliasable_range<R &, T>
    // NOLINTNEXTLINE(hicpp-explicit-conversions,cppcoreguidelines-missing-std-forward)
    constexpr basic_byte_span(R &&cont) : storage_type(byte_cast<T>(std::data(cont)), std::size(cont)) {}

    // Conversions
    template <byte_aliasable<T> U, std::size_t N>
        requires(!std::same_as<U, T> || N != Extent) && (Extent == dynamic_extent || N == Extent)
    // NOLINTNEXTLINE(hicpp-explicit-conversions)
    constexpr basic_byte_span(const basic_byte_span<U, N> &source) noexcept
        : storage_type(byte_cast<T>(source.data()), source.size()) {}

    ///////////////////////////////////////////////////////////////////////////
    // extra
    // nullptr
    // NOLINTNEXTLINE(hicpp-explicit-conversions)
    constexpr basic_byte_span(std::nullptr_t) noexcept
        requires(Extent == 0 || Extent == dynamic_extent)
    {};

    // Iterators
    // begin (https://en.cppreference.com/w/cpp/container/span/begin)
    [[nodiscard]] constexpr auto begin() const noexcept -> T * { return data(); }
    [[nodiscard]] constexpr auto cbegin() const noexcept -> const T * { return data(); }

    // end (https://en.cppreference.com/w/cpp/container/span/end)
    [[nodiscard]] constexpr auto end() const noexcept -> T * { return data() + size(); }
    [[nodiscard]] constexpr auto cend() const noexcept -> const T * { return data() + size(); }

    // rbegin (https://en.cppreference.com/w/cpp/container/span/rbegin)
    [[nodiscard]] constexpr auto rbegin() const noexcept -> reverse_iterator { return reverse_iterator(end()); }
    [[nodiscard]] constexpr auto crbegin() const noexcept -> const_reverse_iterator {
        return const_reverse_iterator(cend());
    }

    // rend (https://en.cppreference.com/w/cpp/container/span/rend)
    [[nodiscard]] constexpr auto rend() const noexcept -> reverse_iterator { return reverse_iterator(begin()); }
    [[nodiscard]] constexpr auto crend() const noexcept -> const_reverse_iterator {
        return const_reverse_iterator(cbegin());
    }

    // Element access
    // front (https://en.cppreference.com/w/cpp/container/span/front)
    [[nodiscard]] constexpr auto front() const noexcept -> T & { return *data(); }

    // back (https://en.cppreference.com/w/cpp/container/span/back)
    [[nodiscard]] constexpr auto back() const noexcept -> T & { return *(data() + (size() - 1)); }

    // operator[] (https://en.cppreference.com/w/cpp/container/span/operator_at)
    constexpr auto operator[](std::size_t idx) const noexcept -> T & { return data()[idx]; }

    // data (https://en.cppreference.com/w/cpp/container/span/data)
    using storage_type::data;

    // Observers
    // size (https://en.cppreference.com/w/cpp/container/span/size)
    using storage_type::size;

    // size_bytes (https://en.cppreference.com/w/cpp/container/span/size_bytes)
    [[nodiscard]] constexpr auto size_bytes() const noexcept -> std::size_t { return size() * sizeof(T); }

    // empty (https://en.cppreference.com/w/cpp/container/span/empty)
    [[nodiscard]] constexpr auto empty() const noexcept -> bool { return size() == 0; }

    // Subviews
    // first (https://en.cppreference.com/w/cpp/container/span/first)
    template <std::size_t Count>
        requires(Count <= Extent || Extent == dynamic_extent)
    [[nodiscard]] constexpr auto first() const noexcept -> basic_byte_span<T, Count> {
        return {data(), Count};
    }
    [[nodiscard]] constexpr auto first(std::size_t Count) const noexcept -> basic_byte_span<T, dynamic_extent> {
        return {data(), Count};
    }

    // last (https://en.cppreference.com/w/cpp/container/span/last)
    template <std::size_t Count>
        requires(Count <= Extent || Extent == dynamic_extent)
    [[nodiscard]] constexpr auto last() const noexcept -> basic_byte_span<T, Count> {
        return {data() + (size() - Count), Count};
    }
    [[nodiscard]] constexpr auto last(std::size_t Count) const noexcept -> basic_byte_span<T, dynamic_extent> {
        return {data() + (size() - Count), Count};
    }

    // subspan (https://en.cppreference.com/w/cpp/container/span/subspan)
    template <std::size_t Offset, std::size_t Count = dynamic_extent>
        requires(Offset <= Extent || Extent == dynamic_extent) &&
                (Count == dynamic_extent || Offset + Count <= Extent || Extent == dynamic_extent)
    [[nodiscard]] constexpr auto subspan() const noexcept
        -> basic_byte_span<T, detail::subspan_extent(Offset, Count, extent)> {
        return {data() + Offset, Count != dynamic_extent ? Count : size() - Offset};
    }
    [[nodiscard]] constexpr auto subspan(std::size_t Offset, std::size_t Count = dynamic_extent) const noexcept
        -> basic_byte_span<T, dynamic_extent> {
        return {data() + Offset, Count != dynamic_extent ? Count : size() - Offset};
    }

    ///////////////////////////////////////////////////////////////////////////
    // extra functionality

    template <typename U>
        requires byte_aliasable<T, detail::const_like_t<T, U>>
    [[nodiscard]] constexpr auto begin() const noexcept {
        return byte_cast<U>(begin());
    }

    template <typename U>
        requires byte_aliasable<T, detail::const_like_t<T, U>>
    [[nodiscard]] constexpr auto end() const noexcept {
        return byte_cast<U>(end());
    }

    template <typename U>
        requires byte_aliasable<T, detail::const_like_t<T, U>>
    [[nodiscard]] constexpr auto rbegin() const noexcept {
        return std::reverse_iterator(end<U>());
    }

    template <typename U>
        requires byte_aliasable<T, detail::const_like_t<T, U>>
    [[nodiscard]] constexpr auto rend() const noexcept {
        return std::reverse_iterator(begin<U>());
    }

    template <typename U>
        requires byte_aliasable<T, detail::const_like_t<T, U>>
    [[nodiscard]] constexpr auto data() const noexcept {
        return byte_cast<U>(data());
    }

    [[nodiscard]] constexpr auto u8data() const noexcept { return data<std::uint8_t>(); }
    [[nodiscard]] constexpr auto cdata() const noexcept { return data<char>(); }
    [[nodiscard]] constexpr auto ucdata() const noexcept { return data<unsigned char>(); }

    template <typename T2, std::size_t E2>
    [[nodiscard]] constexpr friend auto operator==(basic_byte_span lhs, basic_byte_span<T2, E2> rhs) noexcept -> bool {
        return std::equal(lhs.begin(), lhs.end(), rhs.begin(), rhs.end());
    }

    template <typename T2, std::size_t E2>
    [[nodiscard]] constexpr friend auto operator<=>(basic_byte_span lhs, basic_byte_span<T2, E2> rhs) noexcept
        -> std::strong_ordering {
        return std::lexicographical_compare_three_way(lhs.begin(), lhs.end(), rhs.begin(), rhs.end());
    }
};

template <std::size_t N = dynamic_extent>
using byte_span = basic_byte_span<const std::byte, N>;

template <std::size_t N = dynamic_extent>
using mut_byte_span = basic_byte_span<std::byte, N>;

using byte_view = byte_span<>;
using mut_byte_view = mut_byte_span<>;

} // namespace mcpp::byte_span