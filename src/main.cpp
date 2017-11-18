#include "everything.h"

#include <iostream>

using namespace Graphics;
using namespace Input;

Mouse mouse;

ReflectStruct(Attributes, (
    (fvec3)(pos),
    (fvec3)(normal),
))

int main(int, char **)
{
    Window win("Woah", {800,600});

    Blending::Enable();
    Blending::FuncNormalPre();

    while (1)
    {
        Events::Process();

        if (Keys::f11.pressed())
            win.ToggleFullscreen();

        Graphics::CheckErrors();
        Clear(color);

        win.Swap();
    }

    return 0;
}
