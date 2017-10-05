#ifndef PROGRAM_H_INCLUDED
#define PROGRAM_H_INCLUDED

#include <string>

namespace Program
{
    void Exit(int = 0);
    void Exit_NoCleanup(int = 0);
    void Error(std::string, int = 1);
    void Error_NoCleanup(std::string, int = 1);

    void SetErrorMessageBoxCaption(std::string);
    void SetErrorMessageBoxPrefix(std::string);
}

#endif
