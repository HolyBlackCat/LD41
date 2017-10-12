#include "events.h"

#include <SDL2/SDL.h>

#include "program.h"
#include "window.h"

namespace Events
{
    static TimePoint_t tick_counter = 1; // Sic! This starts from 1, because 0 means 'never'.

    TimePoint_t TimePoint()
    {
        return tick_counter;
    }


    static bool exit_requested = 0;

    bool ExitRequested()
    {
        return exit_requested;
    }
    void IgnoreExitRequest()
    {
        exit_requested = 0;
    }


    static constexpr int key_count = SDL_NUM_SCANCODES;

    namespace TimePoints
    {
        static TimePoint_t array_key_down[key_count]{},
                           array_key_up[key_count]{},
                           array_key_repeat[key_count]{};

        TimePoint_t KeyDown(int index)
        {
            if (index < 0 || index >= key_count)
                return 0;
            return array_key_down[index];
        }
        TimePoint_t KeyUp(int index)
        {
            if (index < 0 || index >= key_count)
                return 0;
            return array_key_up[index];
        }
        TimePoint_t KeyRepeat(int index)
        {
            if (index < 0 || index >= key_count)
                return 0;
            return array_key_repeat[index];
        }
    }

    void Process()
    {
        if (exit_requested)
            Program::Exit();

        SDL_Event event;

        while (SDL_PollEvent(&event))
        {
            switch (event.type)
            {
              case SDL_QUIT:
                exit_requested = 1;
                break;

              case SDL_KEYDOWN:
                if (event.key.keysym.scancode <= 0 || event.key.keysym.scancode >= key_count) // Note the `<= 0`! Scancode 0 is 'undefined', so we use it for the last used key.
                    break;
                TimePoints::array_key_repeat[0] = tick_counter;
                TimePoints::array_key_repeat[event.key.keysym.scancode] = tick_counter;
                if (event.key.repeat)
                    break;
                TimePoints::array_key_down[0] = tick_counter;
                TimePoints::array_key_down[event.key.keysym.scancode] = tick_counter;
                break;
              case SDL_KEYUP:
                if (event.key.keysym.scancode <= 0 || event.key.keysym.scancode >= key_count) // Note the `<= 0`! Scancode 0 is 'undefined', so we use it for the last used key.
                    break;
                if (event.key.repeat) // Sic! We don't care about repeated releases.
                    break;
                TimePoints::array_key_up[0] = tick_counter;
                TimePoints::array_key_up[event.key.keysym.scancode] = tick_counter;
                break;

              case SDL_WINDOWEVENT:
                switch (event.window.event)
                {
                  case SDL_WINDOWEVENT_CLOSE:
                    if (auto ptr = Window::FromID(event.window.windowID); ptr)
                        ptr->closure_requested = 1;
                    break;
                }
                break;
            }
        }

        tick_counter++;
    }
}
