#include "program.h"

#include <cstdlib>

#include "ui.h"

namespace Program
{
    static std::string error_messagebox_caption = "Error!",
                       error_messagebox_prefix  = "Error: ";

    void Exit(int code)
    {
        std::exit(code);
    }
    void Exit_NoCleanup(int code)
    {
        std::_Exit(code);
    }

    void Error(std::string text, int code)
    {
        UI::MessageBox(error_messagebox_caption, error_messagebox_prefix + text, UI::error);
        Exit(code);
    }
    void Error_NoCleanup(std::string text, int code)
    {
        UI::MessageBox(error_messagebox_caption, error_messagebox_prefix + text, UI::error);
        Exit_NoCleanup(code);
    }

    void SetErrorMessageBoxCaption(std::string caption)
    {
        error_messagebox_caption = caption;
    }
    void SetErrorMessageBoxPrefix(std::string prefix)
    {
        error_messagebox_prefix = prefix;
    }

}
