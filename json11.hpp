/* json11
 *
 * json11 is a tiny JSON library for C++11, providing JSON parsing and serialization.
 *
 * The core object provided by the library is json11::Json. A Json object represents any JSON
 * value: null, bool, number (int or double), string (std::string), array (std::vector), or
 * object (std::map).
 *
 * Json objects act like values: they can be assigned, copied, moved, compared for equality or
 * order, etc. There are also helper methods Json::dump, to serialize a Json to a string, and
 * Json::parse (static) to parse a std::string as a Json object.
 *
 * Internally, the various types of Json object are represented by the JsonValue class
 * hierarchy.
 *
 * A note on numbers - JSON specifies the syntax of number formatting but not its semantics,
 * so some JSON implementations distinguish between integers and floating-point numbers, while
 * some don't. In json11, we choose the latter. Because some JSON implementations (namely
 * Javascript itself) treat all numbers as the same type, distinguishing the two leads
 * to JSON that will be *silently* changed by a round-trip through those implementations.
 * Dangerous! To avoid that risk, json11 stores all numbers as double internally, but also
 * provides integer helpers.
 *
 * Fortunately, double-precision IEEE754 ('double') can precisely store any integer in the
 * range +/-2^53, which includes every 'int' on most systems. (Timestamps often use int64
 * or long long to avoid the Y2038K problem; a double storing microseconds since some epoch
 * will be exact for +/- 275 years.)
 */

/* Copyright (c) 2013 Dropbox, Inc.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

#ifndef JSON11_HPP_
    #define JSON11_HPP_
#endif

#include <string>
#include <vector>
#include <map>
#include <memory>
#include <initializer_list>
#include <string_view>
#include <functional>
#include <iterator>

#ifdef _MSC_VER
    #if _MSC_VER <= 1800 // VS 2013
        #ifndef noexcept
            #define noexcept throw()
        #endif

        #ifndef snprintf
            #define snprintf _snprintf_s
        #endif
    #endif
#endif

namespace json11 {

namespace mpcs {
enum JsonParse {
    STANDARD, COMMENTS
};

class JsonValue;

//the implementation is in inheritance and template methods
// I think they would benefit greatly from duck typing and std::visit
// since the number of classes are known.

// It would not be a good idea to use type eraser since it will be
// pretty brittle to cram all the behaviours of different constructors
// into a template.
class Json final {
public:
    // Types
    enum Type {
        NUL, NUMBER, BOOL, STRING, ARRAY, OBJECT
    };

    // Array and object typedefs
    /* Use std::vector because
     1. As implemented in json11.cpp, we do not know the total size of
     json array at the first place when decoding. Data is read and
     appended sequentially. So we dont use std::array or Json[]
     2. As json array must be ordered, we dont use any of the unordered
     containers
     3. Theres no FIFO or LIFO or in and out operations, so no stack, queue
     or deque.
     4. No need of slicing so we dont use list here. */
    
    using array = std::vector<Json>;
    using object = std::map<std::string, Json, std::less<> >;

    // Constructors for the various types of JSON value.
    Json() noexcept;                // NUL
    Json(std::nullptr_t) noexcept;  // NUL
    Json(double value);             // NUMBER
    Json(int value);                // NUMBER
    Json(bool value);               // BOOL
    /* left some raw string and c string instead of replacing them all with
    string_view to show familirity */
    Json(std::string const& value); // STRING
    Json(std::string &&value);      // STRING
    Json(char const * value);       // STRING
    Json(array const& values);      // ARRAY
    Json(array &&values);           // ARRAY
    Json(object const& values);     // OBJECT
    Json(object &&values);          // OBJECT

    // Implicit constructor: anything with a to_json() function.
    template <class T, class = decltype(&T::to_json)>
    Json(T const& t) : Json(t.to_json()) {}

    // Implicit constructor: map-like objects (std::map, std::unordered_map, etc)
    // add SFINAE to check whether M has a valid interator type
    // bidirectional for std::map, forward for std::unordered map
    
    template <class M, typename std::enable_if<
        std::is_constructible<std::string,
                              decltype(std::declval<M>().begin()->first)
                             >::value &&
        std::is_constructible<Json,
                              decltype(std::declval<M>().begin()->second)
                             >::value &&
        (
         std::is_same<typename std::iterator_traits<
                        decltype(std::declval<M>().begin())
                                                   >::iterator_category,
                     std::bidirectional_iterator_tag // for map
                    >::value ||
         std::is_same<typename std::iterator_traits<
                        decltype(std::declval<M>().begin())
                                                   >::iterator_category,
                     std::forward_iterator_tag // for unordered map
                    >::value
         ),                                int>::type = 0
            >
    Json(M const& m) : Json(object(m.begin(), m.end())) {}

    // Implicit constructor: vector-like objects (std::list, std::vector, std::set, etc)
    // add SFINAE to check whether V has a valid iterator type
    // random access for vector, array, deque,
    // bidirectional for forward_list, list,
    // dont feel like implementing queue and stacks here as they dont have iterators...
    template <class V, typename std::enable_if<
        std::is_constructible<Json, decltype(*std::declval<V>().begin())>::value &&
        (
         std::is_same<typename std::iterator_traits<
                        decltype(std::declval<V>().begin())
                                                   >::iterator_category,
                     std::bidirectional_iterator_tag // for map
                    >::value ||
         std::is_same<typename std::iterator_traits<
                        decltype(std::declval<V>().begin())
                                                   >::iterator_category,
                     std::random_access_iterator_tag // for unordered map
                    >::value
        ),
                                          int>::type = 0
             >
    Json(V const& v) : Json(array(v.begin(), v.end())) {}

    // This prevents Json(some_pointer) from accidentally producing a bool. Use
    // Json(bool(some_pointer)) if that behavior is desired.
    Json(void *) = delete;

    // Accessors
    Type type() const;

    bool is_null()   const { return type() == NUL; }
    bool is_number() const { return type() == NUMBER; }
    bool is_bool()   const { return type() == BOOL; }
    bool is_string() const { return type() == STRING; }
    bool is_array()  const { return type() == ARRAY; }
    bool is_object() const { return type() == OBJECT; }

    // Return the enclosed value if this is a number, 0 otherwise. Note that json11 does not
    // distinguish between integer and non-integer numbers - number_value() and int_value()
    // can both be applied to a NUMBER-typed object.
    double number_value() const;
    int int_value() const;

    // Return the enclosed value if this is a boolean, false otherwise.
    bool bool_value() const;
    // Return the enclosed string if this is a string, "" otherwise.
    std::string_view string_value() const;
    // Return the enclosed std::vector if this is an array, or an empty vector otherwise.
    array const& array_items() const;
    // Return the enclosed std::map if this is an object, or an empty map otherwise.
    object const& object_items() const;

    // Return a reference to arr[i] if this is an array, Json() otherwise.
    Json const& operator[](size_t i) const;
    // Return a reference to obj[key] if this is an object, Json() otherwise.
    Json const& operator[](std::string_view key) const;

    // Serialize.
    void dump(std::string &out) const;
    std::string dump() const {
        std::string out;
        dump(out);
        return out;
    }

    // Parse. If parse fails, return Json() and assign an error message to err.
    /* since this program does not throw, but return an error message instead,
    there really is no need to use 'expected' */
    static Json parse(std::string_view in,
                      std::string & err,
                      JsonParse strategy = JsonParse::STANDARD);
    
    // intentionally left the c string here instead of replacing it with string_view
    //just to show my familiarity
    static Json parse(char const* in,
                      std::string & err,
                      JsonParse strategy = JsonParse::STANDARD) {
        if (in) {
            return parse(std::string_view(in), err, strategy);
        } else {
            err = "null input";
            return nullptr;
        }
    }
    // Parse multiple objects, concatenated or separated by whitespace
    static std::vector<Json> parse_multi(
        std::string_view in,
        std::string::size_type & parser_stop_pos,
        std::string & err,
        JsonParse strategy = JsonParse::STANDARD);

    static inline std::vector<Json> parse_multi(
        std::string_view in,
        std::string & err,
        JsonParse strategy = JsonParse::STANDARD) {
        std::string::size_type parser_stop_pos;
        return parse_multi(in, parser_stop_pos, err, strategy);
    }

    /*here is the reason why we use std::map instead of std::unordered_map.
     To make sure "Json objects act like values: they can be assigned, copied, moved, compared for equality or order, etc." */
     
    bool operator== (Json const& rhs) const;
    bool operator<  (Json const& rhs) const;
    bool constexpr operator!= (Json const& rhs) const { return !(*this == rhs); }
    bool constexpr operator<= (Json const& rhs) const { return !(rhs < *this); }
    bool constexpr operator>  (Json const& rhs) const { return  (rhs < *this); }
    bool constexpr operator>= (Json const& rhs) const { return !(*this < rhs); }

    /* has_shape(types, err)
     *
     * Return true if this is a JSON object and, for each item in types, has a field of
     * the given type. If not, return false and set err to a descriptive message.
     */
    using shape = std::initializer_list<std::pair<std::string, Type>>;
    bool has_shape(shape const& types, std::string & err) const;

private:
    std::shared_ptr<JsonValue> m_ptr;
};

// Internal class hierarchy - JsonValue objects are not exposed to users of this API.
class JsonValue {
protected:
    friend class Json;
    friend class JsonInt;
    friend class JsonDouble;
    virtual Json::Type type() const = 0;
    virtual bool equals(JsonValue const* other) const = 0;
    virtual bool less(JsonValue const* other) const = 0;
    virtual void dump(std::string &out) const = 0;
    virtual double number_value() const;
    virtual int int_value() const;
    virtual bool bool_value() const;
    virtual std::string_view string_value() const;
    virtual const Json::array &array_items() const;
    virtual const Json &operator[](size_t i) const;
    virtual const Json::object &object_items() const;
    virtual const Json &operator[](std::string_view key) const;
    virtual ~JsonValue() {}
};

}// namespace mpcs

namespace original {
enum JsonParse {
    STANDARD, COMMENTS
};

class JsonValue;

class Json final {
public:
    // Types
    enum Type {
        NUL, NUMBER, BOOL, STRING, ARRAY, OBJECT
    };

    // Array and object typedefs
    typedef std::vector<Json> array;
    typedef std::map<std::string, Json> object;

    // Constructors for the various types of JSON value.
    Json() noexcept;                // NUL
    Json(std::nullptr_t) noexcept;  // NUL
    Json(double value);             // NUMBER
    Json(int value);                // NUMBER
    Json(bool value);               // BOOL
    Json(const std::string &value); // STRING
    Json(std::string &&value);      // STRING
    Json(const char * value);       // STRING
    Json(const array &values);      // ARRAY
    Json(array &&values);           // ARRAY
    Json(const object &values);     // OBJECT
    Json(object &&values);          // OBJECT

    // Implicit constructor: anything with a to_json() function.
    template <class T, class = decltype(&T::to_json)>
    Json(const T & t) : Json(t.to_json()) {}

    // Implicit constructor: map-like objects (std::map, std::unordered_map, etc)
    template <class M, typename std::enable_if<
        std::is_constructible<std::string, decltype(std::declval<M>().begin()->first)>::value
        && std::is_constructible<Json, decltype(std::declval<M>().begin()->second)>::value,
            int>::type = 0>
    Json(const M & m) : Json(object(m.begin(), m.end())) {}

    // Implicit constructor: vector-like objects (std::list, std::vector, std::set, etc)
    template <class V, typename std::enable_if<
        std::is_constructible<Json, decltype(*std::declval<V>().begin())>::value,
            int>::type = 0>
    Json(const V & v) : Json(array(v.begin(), v.end())) {}

    // This prevents Json(some_pointer) from accidentally producing a bool. Use
    // Json(bool(some_pointer)) if that behavior is desired.
    Json(void *) = delete;

    // Accessors
    Type type() const;

    bool is_null()   const { return type() == NUL; }
    bool is_number() const { return type() == NUMBER; }
    bool is_bool()   const { return type() == BOOL; }
    bool is_string() const { return type() == STRING; }
    bool is_array()  const { return type() == ARRAY; }
    bool is_object() const { return type() == OBJECT; }

    // Return the enclosed value if this is a number, 0 otherwise. Note that json11 does not
    // distinguish between integer and non-integer numbers - number_value() and int_value()
    // can both be applied to a NUMBER-typed object.
    double number_value() const;
    int int_value() const;

    // Return the enclosed value if this is a boolean, false otherwise.
    bool bool_value() const;
    // Return the enclosed string if this is a string, "" otherwise.
    const std::string &string_value() const;
    // Return the enclosed std::vector if this is an array, or an empty vector otherwise.
    const array &array_items() const;
    // Return the enclosed std::map if this is an object, or an empty map otherwise.
    const object &object_items() const;

    // Return a reference to arr[i] if this is an array, Json() otherwise.
    const Json & operator[](size_t i) const;
    // Return a reference to obj[key] if this is an object, Json() otherwise.
    const Json & operator[](const std::string &key) const;

    // Serialize.
    void dump(std::string &out) const;
    std::string dump() const {
        std::string out;
        dump(out);
        return out;
    }

    // Parse. If parse fails, return Json() and assign an error message to err.
    static Json parse(const std::string & in,
                      std::string & err,
                      JsonParse strategy = JsonParse::STANDARD);
    static Json parse(const char * in,
                      std::string & err,
                      JsonParse strategy = JsonParse::STANDARD) {
        if (in) {
            return parse(std::string(in), err, strategy);
        } else {
            err = "null input";
            return nullptr;
        }
    }
    // Parse multiple objects, concatenated or separated by whitespace
    static std::vector<Json> parse_multi(
        const std::string & in,
        std::string::size_type & parser_stop_pos,
        std::string & err,
        JsonParse strategy = JsonParse::STANDARD);

    static inline std::vector<Json> parse_multi(
        const std::string & in,
        std::string & err,
        JsonParse strategy = JsonParse::STANDARD) {
        std::string::size_type parser_stop_pos;
        return parse_multi(in, parser_stop_pos, err, strategy);
    }

    bool operator== (const Json &rhs) const;
    bool operator<  (const Json &rhs) const;
    bool operator!= (const Json &rhs) const { return !(*this == rhs); }
    bool operator<= (const Json &rhs) const { return !(rhs < *this); }
    bool operator>  (const Json &rhs) const { return  (rhs < *this); }
    bool operator>= (const Json &rhs) const { return !(*this < rhs); }

    /* has_shape(types, err)
     *
     * Return true if this is a JSON object and, for each item in types, has a field of
     * the given type. If not, return false and set err to a descriptive message.
     */
    typedef std::initializer_list<std::pair<std::string, Type>> shape;
    bool has_shape(const shape & types, std::string & err) const;

private:
    std::shared_ptr<JsonValue> m_ptr;
};

// Internal class hierarchy - JsonValue objects are not exposed to users of this API.
class JsonValue {
protected:
    friend class Json;
    friend class JsonInt;
    friend class JsonDouble;
    virtual Json::Type type() const = 0;
    virtual bool equals(const JsonValue * other) const = 0;
    virtual bool less(const JsonValue * other) const = 0;
    virtual void dump(std::string &out) const = 0;
    virtual double number_value() const;
    virtual int int_value() const;
    virtual bool bool_value() const;
    virtual const std::string &string_value() const;
    virtual const Json::array &array_items() const;
    virtual const Json &operator[](size_t i) const;
    virtual const Json::object &object_items() const;
    virtual const Json &operator[](const std::string &key) const;
    virtual ~JsonValue() {}
};
} // namespace original

using namespace mpcs;
} // namespace json11
