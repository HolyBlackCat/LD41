#include <iostream>

#include "events.h"
#include "exceptions.h"
#include "input.h"
#include "mat.h"
#include "preprocessor.h"
#include "program.h"
#include "reflection.h"
#include "string.h"
#include "template_utils.h"
#include "ui.h"
#include "window.h"
#include "wrappers.h"

using namespace Input;

int main(int, char **)
{
    Window win("Woah", {800,600}, Window::Settings{}.Resizable(1));

    while (1)
    {
        Events::Process();

        if (Keys::f11.pressed())
            win.ToggleFullscreen();

        if (win.size_changed)
        {
            win.size_changed = 0;
            std::cout << win.Size() << '\n';
        }

        glBegin(GL_TRIANGLES);
        glColor3f(0.1, 0.2, 0.3);
        glVertex3f(0, 0, 0);
        glVertex3f(1, 0, 0);
        glVertex3f(0, 1, 0);
        glEnd();

        win.Swap();
    }

    return 0;
}
