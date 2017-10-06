#ifndef WINDOW_H_INCLUDED
#define WINDOW_H_INCLUDED

#include <string>
#include <utility>

#include <SDL2/SDL.h>

#include "mat.h"
#include "movable_handle.h"
#include "program.h"

class Window
{
    using WinHandleType = SDL_Window *;
    struct WinHandleParam
    {
        std::string name;
        ivec2 pos;
        ivec2 size;
        uint32_t flags;
    };
    static WinHandleType WinHandleCreate(const WinHandleParam &param)
    {
        return SDL_CreateWindow(param.name.c_str(), param.pos.x, param.pos.y, param.size.x, param.size.y, param.flags);
    }
    static void WinHandleDestroy(WinHandleType handle)
    {
        SDL_DestroyWindow(handle);
    }
    static void WinHandleError(const WinHandleParam &)
    {
        Program::Error("Can't create a window.");
    }
    using WinHandle_t = MovableHandle<WinHandleType, WinHandleParam, WinHandleCreate, WinHandleDestroy, WinHandleError>;

    using ConHandleType = SDL_GLContext;
    using ConHandleParam = WinHandleType;
    static ConHandleType ConHandleCreate(const ConHandleParam &window)
    {
        return SDL_GL_CreateContext(window);
    }
    static void ConHandleDestroy(ConHandleType handle)
    {
        SDL_GL_DeleteContext(handle);
    }
    static void ConHandleError(const ConHandleParam &)
    {
        Program::Error("Can't create a GL context.");
    }
    using ConHandle_t = MovableHandle<ConHandleType, ConHandleParam, ConHandleCreate, ConHandleDestroy, ConHandleError>;

  public:
    struct Settings
    {
        bool resizable = 0;
        ivec2 pos = ivec2(SDL_WINDOWPOS_UNDEFINED);
        bool fullscreen = 0;
        bool keep_desktop_resolution_when_fullscreen = 0;

        Settings() {}
    };

  private:
    WinHandle_t window;
    ConHandle_t context;

    std::string name;
    ivec2 size;
    Settings settings;

  public:
    Window() {}
    Window(std::string name, ivec2 size, Settings settings = {})
    {
        Create(name, size, settings);
    }

    void Create(std::string new_name, ivec2 new_size, Settings new_settings = {})
    {
        name = new_name;
        size = new_size;
        settings = new_settings;

        uint32_t flags = SDL_WINDOW_OPENGL;
        if (settings.resizable)
            flags |= SDL_WINDOW_RESIZABLE;
        if (settings.fullscreen)
        {
            if (settings.keep_desktop_resolution_when_fullscreen)
                flags |= SDL_WINDOW_FULLSCREEN_DESKTOP;
            else
                flags |= SDL_WINDOW_FULLSCREEN;
        }

        WinHandle_t new_window  = {{name, settings.pos, size, flags}};
        ConHandle_t new_context = {{*new_window}};
        window  = new_window.move();
        context = new_context.move();
    }

    void Destroy()
    {
        context.destroy();
        window.destroy();
    }

    bool Exists() const
    {
        return bool(window);
    }
};

#endif
