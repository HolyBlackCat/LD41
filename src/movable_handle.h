#ifndef MOVABLE_HANDLE_H_INCLUDED
#define MOVABLE_HANDLE_H_INCLUDED

#include <type_traits>
#include <utility>

template <typename interface_Handle,
          typename interface_Param,
          interface_Handle (*interface_Create     )(const interface_Param &), // May throw
          void             (*interface_Destroy    )(interface_Handle       ),
          void             (*interface_CreateError)(const interface_Param &), // May throw
          // Optional:
          interface_Handle (*interface_Null       )() = nullptr /*Defaults to 'return interface_Handle(0);'*/>
class MovableHandle
{
    static_assert(interface_Create && interface_Destroy && interface_CreateError, "A nullptr as function parameter.");
  public:
    using handle_t = interface_Handle;
    using param_t  = interface_Param;
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

    [[nodiscard]] MovableHandle &&move() noexcept
    {
        return (MovableHandle &&)*this;
    }

    void create(param_t param)
    {
        destroy();
        handle = interface_Create(param);
        if (is_null())
            interface_CreateError(param);
    }

    void destroy() noexcept
    {
        if (!is_null())
        {
            interface_Destroy(handle);
            handle = null();
        }
    }

    [[nodiscard]] static handle_t null() noexcept
    {
        // This `if` is contrived way to say `if constexpr (interface_Null)` that dodges `interface_Null can't be null` warning.
        if constexpr (!std::is_same_v<std::integral_constant<decltype(interface_Null), interface_Null>, std::integral_constant<decltype(interface_Null), nullptr>>)
            return interface_Null();
        else
            return handle_t(0);
    }

    MovableHandle() noexcept : handle(null()) {}

    MovableHandle(const param_t &param)
    {
        handle = interface_Create(param);
        if (is_null())
            interface_CreateError(param);
    }

    MovableHandle(const MovableHandle &) = delete;

    MovableHandle(MovableHandle &&o) noexcept(noexcept(handle_t(o.handle), o.handle = null())): handle(o.handle)
    {
        o.handle = null();
    }

    MovableHandle &operator=(const MovableHandle &) = delete;

    MovableHandle &operator=(MovableHandle &&o) noexcept(noexcept(handle = o.handle, o.handle = null()))
    {
        if (&o == this)
            return *this;
        destroy();
        handle = o.handle;
        o.handle = null();
        return *this;
    }

    MovableHandle &operator=(const param_t &param)
    {
        create(param);
        return *this;
    }

    ~MovableHandle() noexcept
    {
        destroy();
    }
};


#endif
