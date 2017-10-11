#ifndef WRAPPERS_H_INCLUDED
#define WRAPPERS_H_INCLUDED

#include <new>
#include <memory>
#include <tuple>
#include <type_traits>
#include <utility>

#include "template_utils.h"

namespace Wrappers
{
    namespace impl
    {
        template <typename T, typename = void> struct has_custom_null : std::false_type {};
        template <typename T> struct has_custom_null<T, std::void_t<decltype(T::Null())>> : std::true_type {};
    }

    /* T should contain:
     * static Handle Create(Params...)
     * static void Destroy(Handle) // The parameter should be compatible with return type of `Create()`.
     * static void Error(Params...) // Parameters should be compatible with those of `Create()`.
     * Optional:
     * static Handle Null()
     * static void Move(const Handle *old, const Handle *new) // This will be executed after a normal move.
     */

    template <typename T> class Handle
    {
      public:
        using handle_t = TemplateUtils::return_type<decltype(T::Create)>;
        using params_t = TemplateUtils::param_types<decltype(T::Create)>;
      private:
        handle_t handle;
      public:
        [[nodiscard]] bool is_null() const noexcept
        {
            return handle == null();
        }

        [[nodiscard]] explicit operator bool() const noexcept
        {
            return !is_null();
        }

        [[nodiscard]] const handle_t &value() const noexcept
        {
            return handle;
        }

        [[nodiscard]] const handle_t &operator *() const noexcept
        {
            return handle;
        }

        [[nodiscard]] Handle &&move() noexcept
        {
            return (Handle &&)*this;
        }

        void create(const params_t &params)
        {
            destroy();
            handle = std::apply(T::Create, params);
            if (is_null())
                std::apply(T::Error, params);
        }

        void destroy() noexcept
        {
            if (!is_null())
            {
                T::Destroy(handle);
                handle = null();
            }
        }

        [[nodiscard]] static handle_t null() noexcept
        {
            // This `if` is contrived way to say `if constexpr (CustomNull)` that dodges `CustomNull can't be null` warning.
            if constexpr (impl::has_custom_null<T>::value)
                return T::Null();
            else
                return handle_t(0);
        }

        Handle() noexcept : handle(null()) {}

        Handle(const params_t &params)
        {
            handle = std::apply(T::Create, params);
            if (is_null())
                std::apply(T::Error, params);
        }

        Handle(const Handle &) = delete;

        Handle(Handle &&o) noexcept(noexcept(handle_t(o.handle), o.handle = null())): handle(o.handle)
        {
            o.handle = null();
        }

        Handle &operator=(const Handle &) = delete;

        Handle &operator=(Handle &&o) noexcept(noexcept(handle = o.handle, o.handle = null()))
        {
            if (&o == this)
                return *this;
            destroy();
            handle = o.handle;
            o.handle = null();
            return *this;
        }

        Handle &operator=(const params_t &params)
        {
            create(params);
            return *this;
        }

        ~Handle() noexcept
        {
            destroy();
        }
    };

    template <typename T> class Ptr
    {
        T *ptr;
        template <typename TT, typename ...P> void alloc_without_free(P &&... p)
        {
            ptr = new TT((P &&)p...);
            if (!ptr) // Just to be sure.
                throw std::bad_alloc{};
        }
      public:
        template <typename TT, typename ...P> void alloc_t(P &&... p)
        {
            free();
            alloc_without_free<TT>((P &&)p...);
        }
        template <typename ...P> void alloc(P &&... p)
        {
            alloc_t<T>((P &&)p...);
        }
        void free()
        {
            if (ptr)
            {
                std::default_delete<T>{}(ptr);
                ptr = 0;
            }
        }

        [[nodiscard]] bool is_null() const noexcept
        {
            return !ptr;
        }
        [[nodiscard]] operator T *() const noexcept
        {
            return ptr;
        }
        [[nodiscard]] T &operator*() const noexcept
        {
            return *ptr;
        }
        [[nodiscard]] T *operator->() const noexcept
        {
            return ptr;
        }

        Ptr() noexcept : ptr(0) {}

        template <typename ...P> Ptr(P &&... p) : ptr(0)
        {
            alloc_without_free<T>((P &&)p...);
        }

        Ptr(const Ptr &) = delete;

        Ptr(Ptr &&o) noexcept : ptr(o.ptr)
        {
            o.ptr = 0;
        }

        Ptr &operator=(const Ptr &) = delete;

        Ptr &operator=(Ptr &&o)
        {
            if (&o == this)
                return *this;

            std::default_delete<T>{}(ptr);
            ptr = o.ptr;
            o.ptr = 0;
            return *this;
        }

        ~Ptr()
        {
            free();
        }
    };
}


#endif
