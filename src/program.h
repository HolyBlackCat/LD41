#ifndef PROGRAM_H_INCLUDED
#define PROGRAM_H_INCLUDED

#include <string>

namespace Program
{
    void CleanUpOnExit(bool);

    void Exit(int = 0);
    void Error(std::string, int = 1);
    void SetErrorMessageBoxCaption(std::string);
    void SetErrorMessageBoxPrefix(std::string);
}

#ifdef NDEBUG
#  define DebugAssert(text_, /*expr*/...) do {} while (0)
#else
#  define DebugAssert(text_, /*expr*/...) do {if (!bool(__VA_ARGS__)) ::Program::Error("Assertion failed: " text_);} while (0)
#endif

#endif
