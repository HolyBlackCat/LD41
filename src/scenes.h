#ifndef SCENES_H_INCLUDED
#define SCENES_H_INCLUDED

#include <algorithm>
#include <any>
#include <cstddef>
#include <deque>
#include <functional>
#include <tuple>
#include <type_traits>
#include <typeindex>
#include <typeinfo>
#include <unordered_map>
#include <utility>

#include "program.h"
#include "strings.h"

namespace Scenes
{
    template <typename ...T> struct type_list {};
    template <auto...> struct value_list {};

    namespace impl
    {
        template <typename L> struct tuple_t_impl {};
        template <typename ...L> struct tuple_t_impl<type_list<L...>> {using type = std::tuple<L...>;};
        template <typename L> using tuple_t = typename tuple_t_impl<L>::type;

        template <typename L> struct type_list_size_v_impl {};
        template <typename ...L> struct type_list_size_v_impl<type_list<L...>> : std::integral_constant<std::size_t, sizeof...(L)> {};
        template <typename L> inline constexpr std::size_t type_list_size_v = type_list_size_v_impl<L>::value;

        template <template <typename> typename C> struct trait_apply_to_value_list_type
        {
            template <typename V> struct type {};
            template <auto ...V> struct type<value_list<V...>> : std::bool_constant<(C<decltype(V)>::value && ...)> {};
        };
        template <template <typename> typename C> struct trait_negate
        {
            template <typename T> using type = std::negation<C<T>>;
        };

        // If T is member function pointer, `type` is an enclosing class for that function.
        // If T is a plain pointer, `type` is a pointed type without cv-qualifiers.
        template <typename T> struct ptr_base_t_impl {};
        template <typename T> struct ptr_base_t_impl<               T *> {using type = T;};
        template <typename T> struct ptr_base_t_impl<const          T *> {using type = T;};
        template <typename T> struct ptr_base_t_impl<      volatile T *> {using type = T;};
        template <typename T> struct ptr_base_t_impl<const volatile T *> {using type = T;};
        template <typename C, typename R, typename ...P> struct ptr_base_t_impl<R (C::*)(P...)      > {using type = C;};
        template <typename C, typename R, typename ...P> struct ptr_base_t_impl<R (C::*)(P...) const> {using type = C;};
        template <typename T> using ptr_base_t = typename ptr_base_t_impl<T>::type;

        template <typename T> struct mem_func_ptr_return_type_t_impl {};
        template <typename C, typename R, typename ...P> struct mem_func_ptr_return_type_t_impl<R (C::*)(P...)      > {using type = R;};
        template <typename C, typename R, typename ...P> struct mem_func_ptr_return_type_t_impl<R (C::*)(P...) const> {using type = R;};
        template <typename T> using mem_func_ptr_return_type_t = typename mem_func_ptr_return_type_t_impl<T>::type;

        template <typename T, typename> struct is_type_in_list_v_impl {};
        template <typename T> struct is_type_in_list_v_impl<T, type_list<>> : std::false_type {};
        template <typename T, typename F, typename ...L> struct is_type_in_list_v_impl<T,type_list<F,L...>> : std::bool_constant<std::is_same_v<T,F> || is_type_in_list_v_impl<T,type_list<L...>>::value> {};
        template <typename T, typename ...L> inline constexpr bool is_type_in_list_v = is_type_in_list_v_impl<T,L...>::value;

        template <typename L> struct trait_is_type_in_list
        {
            template <typename T> using type = std::bool_constant<is_type_in_list_v<T,L>>;
        };

        template <typename> struct has_repeating_types_v_impl {};
        template <> struct has_repeating_types_v_impl<type_list<>> : std::false_type {};
        template <typename T, typename ...L> struct has_repeating_types_v_impl<type_list<T,L...>> : std::bool_constant<is_type_in_list_v<T,type_list<L...>> || has_repeating_types_v_impl<type_list<L...>>::value> {};
        template <typename T> inline constexpr bool has_repeating_types_v = has_repeating_types_v_impl<T>::value;

        template <typename ...T> struct cat_t_impl {};
        template <> struct cat_t_impl<> {using type = type_list<>;};
        template <typename ...A, typename ...B> struct cat_t_impl<type_list<A...>,type_list<B...>> {using type = type_list<A...,B...>;};
        template <typename ...T, typename ...L> struct cat_t_impl<type_list<T...>,L...> {using type = typename cat_t_impl<type_list<T...>, typename cat_t_impl<L...>::type>::type;};
        template <typename ...T> using cat_t = typename cat_t_impl<T...>::type;

        template <typename T, typename L> using append_t = cat_t<type_list<T>, L>;

        template <typename T, typename L> using append_if_not_in_list_t = std::conditional_t<is_type_in_list_v<T,L>, L, append_t<T,L>>;

        template <template <typename> typename C, typename T, typename L> using conditional_append_t = std::conditional_t<C<T>::value, append_t<T,L>, L>;

        template <typename L> struct remove_duplicates_t_impl {};
        template <> struct remove_duplicates_t_impl<type_list<>> {using type = type_list<>;};
        template <typename T, typename ...L> struct remove_duplicates_t_impl<type_list<T,L...>> {using type = append_if_not_in_list_t<T, typename remove_duplicates_t_impl<type_list<L...>>::type>;};
        template <typename L> using remove_duplicates_t = typename remove_duplicates_t_impl<L>::type;

        template <template <typename> typename C, typename L> struct remove_if_false_t_impl {};
        template <template <typename> typename C> struct remove_if_false_t_impl<C,type_list<>> {using type = type_list<>;};
        template <template <typename> typename C, typename T, typename ...L> struct remove_if_false_t_impl<C,type_list<T,L...>> {using type = conditional_append_t<C, T, typename remove_if_false_t_impl<C,type_list<L...>>::type>;};
        template <template <typename> typename C, typename L> using remove_if_false_t = typename remove_if_false_t_impl<C,L>::type;

        template <typename A, typename B> struct contains_all_types_from_v_impl {};
        template <typename ...A, typename ...B> struct contains_all_types_from_v_impl<type_list<A...>,type_list<B...>> : std::bool_constant<(is_type_in_list_v<B,type_list<A...>> && ...)> {};
        template <typename A, typename B> inline constexpr bool contains_all_types_from_v = contains_all_types_from_v_impl<A,B>::value;

        template <template <typename> typename T, typename L> struct apply_t_impl {};
        template <template <typename> typename T, typename ...L> struct apply_t_impl<T,type_list<L...>> {using type = type_list<T<L>...>;};
        template <template <typename> typename T, typename L> using apply_t = typename apply_t_impl<T,L>::type;

        template <typename A, typename B> inline constexpr bool have_same_types_v = std::is_same_v<B, remove_duplicates_t<cat_t<A,B>>> && std::is_same_v<A, remove_duplicates_t<cat_t<B,A>>>;

        template <typename F, std::size_t ...I> void cexpr_for_each(std::index_sequence<I...>, F &&func)
        {
            (func(std::integral_constant<std::size_t,I>{}) , ...);
        }

        template <typename F, typename T> void for_each_tuple_element(T &&tuple, F &&func)
        {
            cexpr_for_each(std::make_index_sequence<std::tuple_size_v<std::remove_cv_t<std::remove_reference_t<T>>>>{}, [&](auto index){func(std::get<index.value>(std::forward<T>(tuple)));});
        }

        template <auto F, typename T, typename ...P> void call_mem_func_on_every_container_element(T &&container, P &&... params) // If the return type of `F` is bool, then the object will be deleted if it returns `false`.
        {
            if constexpr (std::is_member_function_pointer_v<decltype(F)>)
            {
                auto it = container.begin();
                while (it != container.end())
                {
                    if constexpr (std::is_same_v<bool, mem_func_ptr_return_type_t<decltype(F)>>)
                    {
                        bool keep_alive = ((*it).*F)(std::forward<P>(params)...);
                        if (!keep_alive)
                        {
                            it = container.erase(it);
                            continue;
                        }
                    }
                    else
                        ((*it).*F)(std::forward<P>(params)...);
                    it++;
                }
            }
        }
    }

    class Scene
    {
        virtual void *GetContainer_Generic(std::type_index index) const = 0;
        template <typename T> using container_t = std::deque<T>;
      public:
        virtual void Clear() = 0;
        virtual void Tick() = 0;
        virtual void Render() = 0;

        // *Opt functions do NOT generate a error if they can't find an appropriate container.
        template <typename T>       container_t<T> *GetContainerOpt()       {return (container_t<T> *)GetContainer_Generic(typeid(T)); static_assert(std::is_same_v<T,std::remove_cv_t<T>>, "The type must have no cv-qualifiers.");}
        template <typename T> const container_t<T> *GetContainerOpt() const {return (container_t<T> *)GetContainer_Generic(typeid(T)); static_assert(std::is_same_v<T,std::remove_cv_t<T>>, "The type must have no cv-qualifiers.");}
        template <typename T>       container_t<T> &GetContainer()       {auto *ptr = GetContainerOpt<T>(); if (!ptr) Program::Error(Str("No appropriate container for type ", typeid(T).name(), ".")); return *ptr;}
        template <typename T> const container_t<T> &GetContainer() const {auto *ptr = GetContainerOpt<T>(); if (!ptr) Program::Error(Str("No appropriate container for type ", typeid(T).name(), ".")); return *ptr;}
    };

    template <typename Tick, typename Render> class SceneTemplate;
    template <auto ...TickFunctions, auto ...RenderFunctions> class SceneTemplate<value_list<TickFunctions...>, value_list<RenderFunctions...>> final : public Scene
    {
        using objects_ignored_tick   = impl::apply_t<std::remove_cv_t, impl::apply_t<std::remove_pointer_t, impl::remove_if_false_t<std::is_pointer, type_list<decltype(TickFunctions  )...>>>>;
        using objects_ignored_render = impl::apply_t<std::remove_cv_t, impl::apply_t<std::remove_pointer_t, impl::remove_if_false_t<std::is_pointer, type_list<decltype(RenderFunctions)...>>>>;
        static_assert(!impl::has_repeating_types_v<objects_ignored_tick  >, "Duplicate no-tick tags.");
        static_assert(!impl::has_repeating_types_v<objects_ignored_render>, "Duplicate no-render tags.");

        using func_value_lists_tick   = impl::remove_if_false_t<impl::trait_apply_to_value_list_type<std::is_member_function_pointer>::type, type_list<value_list<TickFunctions  >...>>;
        using func_value_lists_render = impl::remove_if_false_t<impl::trait_apply_to_value_list_type<std::is_member_function_pointer>::type, type_list<value_list<RenderFunctions>...>>;
        static_assert(!impl::has_repeating_types_v<func_value_lists_tick  >, "Duplicate tick functions.");
        static_assert(!impl::has_repeating_types_v<func_value_lists_render>, "Duplicate render functions.");

        using func_value_lists_combined = impl::cat_t<impl::remove_duplicates_t<type_list<value_list<TickFunctions  >...>>, impl::remove_duplicates_t<type_list<value_list<RenderFunctions>...>>>;
        static_assert(!impl::has_repeating_types_v<func_value_lists_combined>, "Some functions are listed as both tick and render.");

        using real_func_ret_types_tick   = impl::apply_t<impl::mem_func_ptr_return_type_t, impl::remove_if_false_t<std::is_member_function_pointer, type_list<decltype(TickFunctions  )...>>>;
        using real_func_ret_types_render = impl::apply_t<impl::mem_func_ptr_return_type_t, impl::remove_if_false_t<std::is_member_function_pointer, type_list<decltype(RenderFunctions)...>>>;
        static_assert(impl::type_list_size_v<impl::remove_if_false_t<impl::trait_negate<impl::trait_is_type_in_list<type_list<void,bool>>::type>::type, real_func_ret_types_render>> == 0, "Tick functions must return bool or void.");
        static_assert(impl::type_list_size_v<impl::remove_if_false_t<impl::trait_negate<std::is_void                                           >::type, real_func_ret_types_render>> == 0, "Render functions must return void.");

        using object_types_t     = impl::remove_duplicates_t<impl::apply_t<impl::ptr_base_t, type_list<decltype(TickFunctions  )...>>>;
        using object_types_t_alt = impl::remove_duplicates_t<impl::apply_t<impl::ptr_base_t, type_list<decltype(RenderFunctions)...>>>;
        static_assert(impl::contains_all_types_from_v<object_types_t, object_types_t_alt>, "Some tick functions are missing.");
        static_assert(impl::contains_all_types_from_v<object_types_t_alt, object_types_t>, "Some render functions are missing.");


        using container_tuple_t = impl::tuple_t<impl::apply_t<Scene::container_t, object_types_t>>;
        container_tuple_t containers;

        virtual void *GetContainer_Generic(std::type_index index) const override
        {
            void *ret = 0;
            impl::for_each_tuple_element(containers, [&](const auto &ref)
            {
                using container_t = std::remove_reference_t<decltype(ref)>;
                using value_t = typename container_t::value_type;
                if (std::type_index(typeid(value_t)) == index) ret = (void *)&ref;
            });
            return ret;
        }

      public:

        virtual void Clear() override
        {
            impl::for_each_tuple_element(containers, [](auto &container){container = {};});
        }
        virtual void Tick() override
        {
            (impl::call_mem_func_on_every_container_element<TickFunctions>(std::get<Scene::container_t<impl::ptr_base_t<decltype(TickFunctions)>>>(containers), *this) , ...);
        }
        virtual void Render() override
        {
            (impl::call_mem_func_on_every_container_element<RenderFunctions>(std::get<Scene::container_t<impl::ptr_base_t<decltype(RenderFunctions)>>>(containers), *this) , ...);
        }
    };
}

#endif
