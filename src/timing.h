#ifndef TIMING_H_INCLUDED
#define TIMING_H_INCLUDED

#include <SDL2/SDL.h>

namespace Timing
{
    inline uint64_t Clock()                        {return SDL_GetPerformanceCounter();}
    inline uint64_t Tps()                          {static uint64_t ret = SDL_GetPerformanceFrequency(); return ret;}
    inline uint64_t Tpms()                         {static uint64_t ret = Tps()/1000; return ret;}
    inline uint64_t SecsToTicks(long double secs)  {return secs * Tps();}
    inline long double TicksToSecs(uint64_t units) {return (long double)units / (long double)Tps();}

    inline void WaitTicks(uint64_t delay)
    {
        uint64_t begin = Clock();
        while (Clock() - begin < delay) {}
    }
    inline void WaitSecs(long double secs)
    {
        WaitTicks(SecsToTicks(secs));
    }

    class TickStabilizer
    {
        uint64_t tick_len;
        int max_ticks;

        uint64_t accumulator;
        bool new_frame;

        bool lag;

      public:
        uint64_t ticks = 0;

        TickStabilizer() : TickStabilizer(60) {}
        TickStabilizer(double freq, int max_ticks_per_frame = 8)
        {
            SetFrequency(freq);
            SetMaxTicksPerFrame(max_ticks_per_frame);
            Reset();
        }

        void SetFrequency(double freq)
        {
            tick_len = Tps() / freq;
        }
        void SetMaxTicksPerFrame(int n) // Set to 0 to disable the limit.
        {
            max_ticks = n;
        }
        void Reset()
        {
            accumulator = 0;
            new_frame = 1;
            lag = 0;
            ticks = 0;
        }

        bool Lag() // Flag resets after this function is called. The flag is set to 1 if the amount of ticks per last frame was limited due to reaching the limit.
        {
            if (lag)
            {
                lag = 0;
                return 1;
            }
            return 0;
        }

        double Frequency() const
        {
            return Tps() / double(tick_len);
        }
        uint64_t ClockTicksPerTick() const
        {
            return tick_len;
        }
        int MaxTicksPerFrame() const
        {
            return max_ticks;
        }

        bool Tick(uint64_t delta)
        {
            if (new_frame)
                accumulator += delta;

            if (accumulator >= tick_len)
            {
                if (max_ticks && accumulator > tick_len * max_ticks)
                {
                    accumulator = tick_len * max_ticks;
                    lag = 1;
                }
                accumulator -= tick_len;
                new_frame = 0;
                ticks++;
                return 1;
            }
            else
            {
                new_frame = 1;
                return 0;
            }
        }

        double Time() const
        {
            return accumulator / double(tick_len);
        }
    };
}

#endif
