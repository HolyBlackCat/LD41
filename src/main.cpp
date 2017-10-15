#include <iostream>

#include "events.h"
#include "exceptions.h"
#include "mat.h"
#include "preprocessor.h"
#include "program.h"
#include "reflection.h"
#include "string.h"
#include "template_utils.h"
#include "ui.h"
#include "window.h"
#include "wrappers.h"

int main(int, char **)
{
    Window win("Woah", {800,600});

    while (1)
    {
        Events::Process();

        std::cout << Events::Input::KeyTime(Events::Input::keyboard, 0, Events::Input::pressed, 0) << '\n';

        win.Swap();
    }

    return 0;
}
