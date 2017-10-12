#ifndef EVENTS_H_INCLUDED
#define EVENTS_H_INCLUDED

#include <cstdint>

namespace Events
{
    using TimePoint_t = uint32_t;

    TimePoint_t TimePoint();

    // If this flag isn't handled, the program will be closed at the beginning of the next event tick.
    // If only one window is opened, this will be set when user tries to close it (but other exit reasons do exist).
    bool ExitRequested();
    void IgnoreExitRequest(); // Resets the above flag.

    namespace TimePoints
    {
        // `index == 0` has a special meaning and designates any (last) key.
        TimePoint_t KeyDown(int index);
        TimePoint_t KeyUp(int index);
        TimePoint_t KeyRepeat(int index);
    }

    void Process();
}

#endif
