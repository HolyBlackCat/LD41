#include <cstddef>
#include <iostream>
#include <numeric>

#include "events.h"
#include "exceptions.h"
#include "graphics.h"
#include "input.h"
#include "mat.h"
#include "preprocessor.h"
#include "program.h"
#include "reflection.h"
#include "renderers2d.h"
#include "strings.h"
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

    Image img("test.png");
    Font font("CatIV15.ttf", 15);
    CharMap ch_map;
    Font::MakeAtlas(img, {8,4}, {128,128}, {{font, ch_map, Font::normal, (uint16_t(&)[127])Strings::cp1251()}});
    Texture tex(img, Texture::nearest);
    re.SetTexture(tex);
    re.SetMatrix(fmat4::ortho({0,600},{800,0},-1,1));
    re.SetDefaultFont(ch_map);

    while (1)
    {
        Events::Process();

        if (Keys::f11.pressed())
            win.ToggleFullscreen();

        Graphics::CheckErrors();
        Clear(color);

        re.Text(mouse.pos(), "Hello").color({0,0.5,1}).align({-1,-1}).append("world!").color({1,0.5,0});
        re.Finish();

        win.Swap();
    }

    return 0;
}
