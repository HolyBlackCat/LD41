#ifndef TEMPLATE_H_INCLUDED
#define TEMPLATE_H_INCLUDED

#include <tuple>
#include <type_traits>
#include <utility>

namespace TemplateUtils
{
    namespace impl
    {
        template <typename T> struct func_types {};
        template <typename R, typename ...P> struct func_types<R(P...)>
        {
            using return_type = R;
            using param_types = std::tuple<P...>;
        };
        template <typename R, typename ...P> struct func_types<R(P...) noexcept>
        {
            using return_type = R;
            using param_types = std::tuple<P...>;
        };
    }
    template <typename T> using return_type = typename impl::func_types<T>::return_type;
    template <typename T> using param_types = typename impl::func_types<T>::param_types;


    template <typename T> struct MoveFunc
    {
        MoveFunc() noexcept {}

        MoveFunc(const MoveFunc &) noexcept {}

        MoveFunc(MoveFunc &&o) noexcept(noexcept(T::AtMove(std::declval<MoveFunc &>(), o)))
        {
            static_assert(std::is_base_of_v<MoveFunc<T>, T>, "This is a CRTP base.");
            T::AtMove(*this, o);
        }

        MoveFunc &operator=(const MoveFunc &) noexcept {}

        MoveFunc &operator=(MoveFunc &&o) noexcept(noexcept(T::AtMove(std::declval<MoveFunc &>(), o)))
        {
            static_assert(std::is_base_of_v<MoveFunc<T>, T>, "This is a CRTP base.");
            if (&o == this)
                return *this;
            T::AtMove(*this, o);
            return *this;
        }
    };
}

#endif
