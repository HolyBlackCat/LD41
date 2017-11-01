#include <cstddef>
#include <iostream>

#include "events.h"
#include "exceptions.h"
#include "graphics.h"
#include "input.h"
#include "mat.h"
#include "preprocessor.h"
#include "program.h"
#include "reflection.h"
#include "renderers2d.h"
#include "string.h"
#include "template_utils.h"
#include "ui.h"
#include "window.h"
#include "utils.h"

using namespace Graphics;
using namespace Input;

Mouse mouse;

int main(int, char **)
{
    Window win("Woah", {800,600});

    Blending::Enable();
    Blending::FuncNormalPre();

    Renderers::Poly2D re(0x10000);

    Texture tex(Image("test.png"), Texture::linear);
    re.SetTexture(tex);
    re.SetMatrix(fmat4::ortho({0,600},{800,0},-1,1));

    while (1)
    {
        Events::Process();

        if (Keys::f11.pressed())
            win.ToggleFullscreen();

        Graphics::CheckErrors();
        Clear(color);

        re.Quad(mouse.pos(), ivec2(191)).tex({0,0}).center().rotate(Events::Time() / 60.);
        re.Finish();

        win.Swap();
    }

    return 0;
}
