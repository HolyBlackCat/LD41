#include "input.h"

#include <algorithm>
#include <limits>

#include "mat.h"
#include "strings.h"


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

    void Mouse::Transform(ivec2 new_offset, float new_scale)
    {
        offset = new_offset;
        scale  = new_scale;
    }

    ivec2 Mouse::pos() const
    {
        return iround((raw_pos() - offset) * scale);
    }
    ivec2 Mouse::shift() const
    {
        return pos() - iround((raw_pos() - rel_pos() - offset) * scale);
    }
    ivec2 Mouse::wheel() const
    {
        if (Events::Input::VectorTime(device_type, device_id, Events::Input::vec_mouse_wheel) != Events::Time())
            return {};
        return Events::Input::Vector(device_type, device_id, Events::Input::vec_mouse_wheel);
    }
    ivec2 Mouse::raw_pos() const
    {
        return Events::Input::Vector(device_type, device_id, Events::Input::vec_mouse_pos);
    }
    ivec2 Mouse::rel_pos() const
    {
        if (Events::Input::VectorTime(device_type, device_id, Events::Input::vec_mouse_pos_rel) != Events::Time())
            return {};
        return Events::Input::Vector(device_type, device_id, Events::Input::vec_mouse_pos_rel);
    }

    void Mouse::Show(bool s)
    {
        SDL_ShowCursor(s);
    }
    void Mouse::Relative(bool r)
    {
        SDL_SetRelativeMouseMode((SDL_bool)r);
    }


    static int text_cursor_pos = 0, text_cursor_byte_pos = 0;

    std::string RawText()
    {
        if (u8valid16(Events::Input::Text()))
            return Events::Input::Text();
        else
            return "";
    }
    void Text(std::string *str, int len_cap)
    {
        static std::string *last_str = 0;
        if (str != last_str)
        {
            last_str = str;
            if (str)
            {
                text_cursor_pos = u8strlen(*str);
                text_cursor_byte_pos = str->size();
            }
            else
            {
                text_cursor_pos = 0;
                text_cursor_byte_pos = 0;
            }
        }
        if (str)
        {
            std::string &ref = *str;
            std::string text = RawText();

            if (!u8valid16(ref))
                ref = {};

            int old_len = u8strlen(ref);
            clamp_assign(text_cursor_pos += Keys::right.repeated() - Keys::left.repeated(), 0, old_len);

            text_cursor_byte_pos = 0;
            int index = 0;
            while (1)
            {
                if (u8isfirstbyte(ref[text_cursor_byte_pos]))
                {
                    if (index == text_cursor_pos)
                        break;
                    index++;
                }
                text_cursor_byte_pos++;
            }

            if (Keys::del.repeated() && text_cursor_pos < old_len)
            {
                ref = ref.substr(0, text_cursor_byte_pos) + ref.substr(text_cursor_byte_pos + u8charlen(ref[text_cursor_byte_pos]));
            }
            if (Keys::backspace.repeated() && text_cursor_pos > 0)
            {
                while (!u8isfirstbyte(ref[--text_cursor_byte_pos])) {}
                ref = ref.substr(0, text_cursor_byte_pos) + ref.substr(text_cursor_byte_pos + u8charlen(ref[text_cursor_byte_pos]));
                text_cursor_pos--;
            }

            if (text.size() && std::find(text.begin(), text.end(), '\n') == text.end() && (len_cap < 0 || u8strlen(ref) + u8strlen(text) <= len_cap))
            {
                ref = ref.substr(0, text_cursor_byte_pos) + text + ref.substr(text_cursor_byte_pos);
                text_cursor_byte_pos += text.size();
                text_cursor_pos += u8strlen(text);
            }
        }
    }
    int TextCursorPos()
    {
        return text_cursor_pos;
    }
    int TextCursorBytePos()
    {
        return text_cursor_byte_pos;
    }
}
