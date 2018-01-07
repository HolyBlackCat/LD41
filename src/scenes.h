#ifndef SCENES_H_INCLUDED
#define SCENES_H_INCLUDED

#include <algorithm>
#include <any>
#include <functional>
#include <typeindex>
#include <typeinfo>
#include <unordered_map>
#include <utility>

#include "program.h"
#include "strings.h"

class Scene
{
    using func_t = std::function<void(const Scene &)>;

    mutable std::unordered_map<std::type_index, std::any> map;
    func_t func_tick, func_render;

  public:
    Scene() {}

    template <typename ...T, typename ...P> void Add(P &&... params)
    {
        if (!(map.insert({typeid(T), std::make_any<T>(std::forward<P>(params)...)}).second && ...))
            Program::Error("Duplicate member objects specified for a scene.");
    }
    void SetTick(func_t func)
    {
        func_tick = std::move(func);
    }
    void SetRender(func_t func)
    {
        func_render = std::move(func);
    }

    template <typename T> T *GetOpt() const
    {
        auto it = map.find(typeid(T));
        if (it != map.end())
            return std::any_cast<T>(&it->second);
        else
            return 0;
    }
    template <typename T> T &Get() const
    {
        auto ptr = GetOpt<T>();
        if (!ptr)
            Program::Error(Str("Scene has no object with type `", typeid(T).name(), "`."));
        return *ptr;
    }

    void Tick()
    {
        func_tick(*this);
    }
    void Render()
    {
        func_render(*this);
    }
};


#endif
