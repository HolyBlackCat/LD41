#include "everything.h"

#include <cstddef>
#include <iostream>
#include <numeric>

using namespace Graphics;
using namespace Input;

Mouse mouse;

int main(int, char **)
{
    Window win("Woah", {800,600});

    Blending::Enable();
    Blending::FuncNormalPre();

    Renderers::Poly2D re(0x10000);

    Image img = Image::Memory({1024,1024});
    Font font("CatIV15.ttf", 60);
    CharMap ch_map;
    Font::MakeAtlas(img, {0,0}, {1024,1024}, {{font, ch_map, Font::normal, Strings::cp1251()}});
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

        re.Text(mouse.pos(), "Hello").color({0,0.5,1}).append("world!").color({1,0.5,0});
        re.Finish();

        win.Swap();
    }

    return 0;
}
