#include "input.h"

#include <algorithm>
#include <limits>


namespace Input
{
    Key Key::operator|(const Key &o) const
    {
        Key ret;
        ret.objects.reserve(objects.size() + o.objects.size());
        ret.objects.insert(ret.objects.end(),   objects.begin(),   objects.end());
        ret.objects.insert(ret.objects.end(), o.objects.begin(), o.objects.end());
        auto object_eq = [](const Object &a, const Object &b)
        {
            return a.device_type == b.device_type &&
                   a.device_id   == b.device_id   &&
                   a.index       == b.index;
        };
        ret.objects.erase(std::unique(ret.objects.begin(), ret.objects.end(), object_eq), ret.objects.end());
        ret.objects.shrink_to_fit();
        return ret;
    }

    void Key::update() const
    {
        if (last_updated == Events::Time())
            return;
        last_updated = Events::Time();

        state_pressed = state_released = state_repeated = 0;
        // state_down is changed after the loop.

        bool down_not_pressed = 0;

        for (const auto &it : objects)
        {
            if (KeyTime(it.device_type, it.device_id, Action::repeated, it.index) == Events::Time())
                state_repeated = 1;

            Events::Time_t t_pressed  = KeyTime(it.device_type, it.device_id, Action::pressed , it.index),
                           t_released = KeyTime(it.device_type, it.device_id, Action::released, it.index);

            bool is_down = t_pressed > t_released;

            if (t_pressed == Events::Time())
                state_pressed = 1;
            else if (is_down)
                down_not_pressed = 1;

            if (t_released == Events::Time())
                state_released = 1;
        }

        state_down = state_pressed || down_not_pressed;
        if (down_not_pressed)
            state_pressed = state_released = 0;
    }


    Key Mouse::button(int index) const
    {
        if (index <= 0)
            return {};
        return {device_type, device_id, index};
    }

    ivec2 Mouse::pos() const
    {
        return iround((Events::Input::Vector(device_type, device_id, Events::Input::vec_mouse_pos) - offset) * scale);
    }
    ivec2 Mouse::rel_pos() const
    {
        if (Events::Input::VectorTime(device_type, device_id, Events::Input::vec_mouse_pos_rel) != Events::Time())
            return {};
        return Events::Input::Vector(device_type, device_id, Events::Input::vec_mouse_pos_rel);
    }
    ivec2 Mouse::wheel() const
    {
        if (Events::Input::VectorTime(device_type, device_id, Events::Input::vec_mouse_wheel) != Events::Time())
            return {};
        return Events::Input::Vector(device_type, device_id, Events::Input::vec_mouse_wheel);
    }
}
