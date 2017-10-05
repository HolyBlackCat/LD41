#ifndef MOVABLE_HANDLE_H_INCLUDED
#define MOVABLE_HANDLE_H_INCLUDED

#include <type_traits>
#include <utility>

template <typename interface_Handle,
          typename interface_Param,
          interface_Handle (*interface_Create     )(const interface_Param &) noexcept,
          void             (*interface_Free       )(interface_Handle       ) noexcept,
          void             (*interface_CreateError)(const interface_Param &) noexcept,
          // Optional:
          interface_Handle (*interface_Null       )() noexcept = nullptr /*Defaults to 'return interface_Handle(0);'*/>
class MovableHandle
{
    static_assert(interface_Create && interface_Free && interface_CreateError, "A nullptr as function parameter.");
  public:
    using handle_t = interface_Handle;
    using param_t  = interface_Param;
  private:
    handle_t handle;

    void free_without_resetting_handle() noexcept
    {
        if (!is_null())
            interface_Free(handle);
    }
  public:
    [[nodiscard]] bool is_null() const noexcept
    {
        return handle == null();
    }

    void create(param_t param) noexcept
    {
        free_without_resetting_handle();
        handle = interface_Create(param);
        if (is_null())
            interface_CreateError(param);
    }

    void free() noexcept
    {
        if (!is_null())
        {
            interface_Free(handle);
            handle = null();
        }
    }

    [[nodiscard]] static handle_t null() noexcept
    {
        // This `if` is contrived way to say `if constexpr (interface_Null)` that dodges `interface_Null can't be null` warning.
        if constexpr (std::is_same_v<std::integral_constant<decltype(interface_Null), interface_Null>, std::integral_constant<decltype(interface_Null), nullptr>>)
            return interface_Null();
        else
            return handle_t(0);
    }

    MovableHandle() noexcept : handle(null()) {}

    MovableHandle(const param_t &param) noexcept
    {
        handle = interface_Create(param);
        if (is_null())
            interface_CreateError(param);
    }

    MovableHandle(const MovableHandle &) = delete;

    MovableHandle(MovableHandle &&o) noexcept : handle(o.handle)
    {
        o.handle = null();
    }

    MovableHandle &operator=(const MovableHandle &) = delete;

    MovableHandle &operator=(MovableHandle &&o) noexcept
    {
        if (&o == this)
            return *this;
        free_without_resetting_handle();
        handle = o.handle;
        o.handle = null();
        return *this;
    }

    MovableHandle &operator=(const param_t &param) noexcept
    {
        create(param);
        return *this;
    }

    ~MovableHandle() noexcept
    {
        free_without_resetting_handle();
    }

    static_assert(noexcept(handle_t(null())) &&
                  noexcept(std::declval<handle_t &>() = null()) &&
                  noexcept(std::declval<handle_t &>() = std::declval<const handle_t &>()), "The handle doesn't satisfy nothrow requirements.");

};


#endif
