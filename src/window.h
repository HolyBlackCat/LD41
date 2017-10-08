#ifndef WINDOW_H_INCLUDED
#define WINDOW_H_INCLUDED

#include <string>
#include <utility>

#include <SDL2/SDL.h>
#include <GLFL/glfl.h>

#include "exceptions.h"
#include "mat.h"
#include "program.h"
#include "reflection.h"
#include "wrappers.h"

class Window
{
  public:
    DefineExceptionBase(exception)
    DefineExceptionStatic(cant_create_window , :exception, "Can't create a window."         , (std::string,name,"Name")(std::string,settings,"Settings"))
    DefineExceptionStatic(cant_create_context, :exception, "Can't create an OpenGL context.", (std::string,name,"Name")(std::string,settings,"Settings"))

    ReflectMemberEnum(SwapModes, (no_vsync)(vsync)(late_swap_tearing)(any_mode))
    ReflectMemberEnum(Profiles, (core)(compatibility)(es)(any_profile))
    ReflectMemberEnum(YesOrNo, (yes)(no)(dont_care))

    struct Settings
    {
        ivec2 min_size = {};
        bool resizable = 0;

        Reflect(Settings)
        ( (bool)(fullscreen)(=0),
          (bool)(keep_desktop_resolution_when_fullscreen)(=0),
          (int)(display)(=0),
          (bool)(make_centered)(=0),
          (int)(gl_major)(=3),
          (int)(gl_minor)(=3),
          (Profiles)(gl_profile)(=compatibility),
          (bool)(gl_debug)(=0),
          (SwapModes)(swap_mode)(=late_swap_tearing),
          (YesOrNo)(hardware_acceleration)(=dont_care),
          (bool)(forward_compatibility)(=0),
          (int)(msaa)(=0),
          (ivec4)(color_bits)(={}),
          (int)(depth_bits,stencil_bits)(=0), )

        Settings() {}
        using ref = Settings &;
        ref MinSize(ivec2 sz)
        {
            min_size = sz;
            return *this;
        }
        ref Resizable(bool r)
        {
            resizable = r;
            return *this;
        }
        ref FullScreen(bool f)
        {
            fullscreen = f;
            return *this;
        }
        ref KeepDesktopResolutionWhenFullscreen(bool k)
        {
            keep_desktop_resolution_when_fullscreen = k;
            return *this;
        }
        ref Display(int n)
        {
            display = n;
            return *this;
        }
        ref MakeCentered(bool c)
        {
            make_centered = c;
            return *this;
        }
        ref GlVersion(int maj, int min)
        {
            gl_major = maj;
            gl_minor = min;
            return *this;
        }
        ref GlProfile(Profiles p)
        {
            gl_profile = p;
            return *this;
        }
        ref GlDebug(bool d)
        {
            gl_debug = d;
            return *this;
        }
        ref SwapMode(SwapModes s)
        {
            swap_mode = s;
            return *this;
        }
        ref HardwareAcceleration(YesOrNo s)
        {
            hardware_acceleration = s;
            return *this;
        }
        ref ForwardCompatibility(YesOrNo f)
        {
            forward_compatibility = f;
            return *this;
        }
        ref MSAA(int m)
        {
            msaa = m;
            return *this;
        }
        ref ColorBits(ivec4 b)
        {
            color_bits = b;
            return *this;
        }
        ref DepthBits(int b)
        {
            depth_bits = b;
            return *this;
        }
        ref StencilBits(int b)
        {
            stencil_bits = b;
            return *this;
        }
    };

  private:
    struct WindowHandleFuncs
    {
        static SDL_Window *Create(std::string name, ivec2 size, Settings &settings)
        {
            uint32_t flags = SDL_WINDOW_OPENGL;
            uint32_t context_flags = 0;

            if (settings.resizable)
                flags |= SDL_WINDOW_RESIZABLE;
            if (settings.fullscreen)
            {
                if (settings.keep_desktop_resolution_when_fullscreen)
                    flags |= SDL_WINDOW_FULLSCREEN_DESKTOP;
                else
                    flags |= SDL_WINDOW_FULLSCREEN;
            }
            ivec2 pos;
            if (settings.make_centered)
                pos = ivec2(SDL_WINDOWPOS_CENTERED_DISPLAY(settings.display));
            else
                pos = ivec2(SDL_WINDOWPOS_UNDEFINED_DISPLAY(settings.display));
            if (settings.gl_major || settings.gl_minor)
            {
                SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, settings.gl_major);
                SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, settings.gl_minor);
            }
            switch (settings.gl_profile)
            {
              case core:
                SDL_GL_SetAttribute(SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_CORE);
                break;
              case compatibility:
                SDL_GL_SetAttribute(SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_COMPATIBILITY);
                break;
              case es:
                SDL_GL_SetAttribute(SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_ES);
                break;
              default:
                break;
            }
            if (settings.gl_debug)
                context_flags |= SDL_GL_CONTEXT_DEBUG_FLAG;
            switch (settings.hardware_acceleration)
            {
              case yes:
                SDL_GL_SetAttribute(SDL_GL_ACCELERATED_VISUAL, 1);
                break;
              case no:
                SDL_GL_SetAttribute(SDL_GL_ACCELERATED_VISUAL, 0);
                break;
              default:
                break;
            }
            if (settings.forward_compatibility)
                context_flags |= SDL_GL_CONTEXT_FORWARD_COMPATIBLE_FLAG;
            if (settings.msaa == 1) settings.msaa = 0;
            if (settings.msaa)
            {
                SDL_GL_SetAttribute(SDL_GL_MULTISAMPLEBUFFERS, 1);
                SDL_GL_SetAttribute(SDL_GL_MULTISAMPLESAMPLES, settings.msaa);
            }
            if (settings.color_bits.r) SDL_GL_SetAttribute(SDL_GL_RED_SIZE    , settings.color_bits.r);
            if (settings.color_bits.g) SDL_GL_SetAttribute(SDL_GL_GREEN_SIZE  , settings.color_bits.g);
            if (settings.color_bits.b) SDL_GL_SetAttribute(SDL_GL_BLUE_SIZE   , settings.color_bits.b);
            if (settings.color_bits.a) SDL_GL_SetAttribute(SDL_GL_ALPHA_SIZE  , settings.color_bits.a);
            if (settings.depth_bits  ) SDL_GL_SetAttribute(SDL_GL_DEPTH_SIZE  , settings.depth_bits  );
            if (settings.stencil_bits) SDL_GL_SetAttribute(SDL_GL_STENCIL_SIZE, settings.stencil_bits);

            SDL_GL_SetAttribute(SDL_GL_CONTEXT_FLAGS, context_flags);

            return SDL_CreateWindow(name.c_str(), pos.x, pos.y, size.x, size.y, flags);
        }
        static void Destroy(SDL_Window *window)
        {
            SDL_DestroyWindow(window);
        }
        static void Error(std::string name, ivec2 /*size*/, const Settings &settings)
        {
            throw cant_create_window(name, Reflection::to_string_tree(settings));
        }
    };

    using WindowHandle = Wrappers::Handle<WindowHandleFuncs>;

    struct ContextHandleFuncs
    {
        static SDL_GLContext Create(SDL_Window *window, std::string /*name*/, const Settings &/*settings*/)
        {
            return SDL_GL_CreateContext(window);
        }
        static void Destroy(SDL_GLContext context)
        {
            SDL_GL_DeleteContext(context);
        }
        static void Error(SDL_Window */*window*/, std::string name, const Settings &settings)
        {
            throw cant_create_context(name, Reflection::to_string_tree(settings));
        }
    };

    using ContextHandle = Wrappers::Handle<ContextHandleFuncs>;


    WindowHandle window;
    ContextHandle context;
    Wrappers::Ptr<glfl::context> func_context;

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

        WindowHandle new_window = {{name, size, settings}};
        ContextHandle new_context = {{*new_window, name, settings}};
        window  = new_window.move();
        context = new_context.move();

        if (settings.min_size)
            SDL_SetWindowMinimumSize(*window, settings.min_size.x, settings.min_size.y);

        switch (settings.swap_mode)
        {
          case no_vsync:
            if (SDL_GL_SetSwapInterval(0))
            {
                settings.swap_mode = vsync;
                SDL_GL_SetSwapInterval(1);
            }
            break;
          case vsync:
            if (SDL_GL_SetSwapInterval(1))
            {
                settings.swap_mode = no_vsync;
                SDL_GL_SetSwapInterval(0);
            }
            break;
          case late_swap_tearing:
            if (SDL_GL_SetSwapInterval(-1))
            {
                settings.swap_mode = vsync;
                if (SDL_GL_SetSwapInterval(1))
                {
                    settings.swap_mode = no_vsync;
                    SDL_GL_SetSwapInterval(0);
                }
            }
            break;
          default:
            break;
        }

        func_context.alloc();
        glfl::set_active_context(func_context);
        glfl::set_function_loader(SDL_GL_GetProcAddress);
        if (settings.gl_profile != es)
            glfl::load_gl(settings.gl_major, settings.gl_minor);
        else
            glfl::load_gles(settings.gl_major, settings.gl_minor);
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

    const Settings &GetSettings() const
    {
        return settings;
    }
};

#endif
