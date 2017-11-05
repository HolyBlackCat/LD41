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

    Image img("test.png");

    TruetypeFont ttfont("CatIV15.ttf");
    Font font;
    int arr[26*2+3];
    std::iota(arr, arr+26, 'a');
    std::iota(arr+26, arr+26*2, 'A');
    arr[26*2+0] = ' ';
    arr[26*2+1] = ',';
    arr[26*2+2] = '!';
    img.CreateFontAtlas({16,8}, {96,44}, {{ttfont, font, 15, 0, arr}}, 128);

    Texture tex(img, Texture::linear);
    re.SetTexture(tex);
    re.SetMatrix(fmat4::ortho({0,600},{800,0},-1,1));

    auto RenderTextSimple = [&](ivec2 pos, const char *string)
    {
        while (*string)
        {
            if (font.Available(*string))
            {
                const auto &ref = font.Get(*string);
                re.Quad(pos + ref.offset, ref.size).tex(ref.tex_pos);
                pos.x += ref.advance;
            }
            string++;
        }
    };

    while (1)
    {
        Events::Process();

        if (Keys::f11.pressed())
            win.ToggleFullscreen();

        Graphics::CheckErrors();
        Clear(color);

        re.Quad(mouse.pos() + ivec2(0,-110), ivec2(191)).tex({0,0}).center();
        RenderTextSimple(mouse.pos(), "Hello, world!");
        re.Finish();

        win.Swap();
    }

    return 0;
}
