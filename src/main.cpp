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
    uint16_t chars[26*2+1]{};
    std::iota(chars, chars+26, 'a');
    std::iota(chars+26, chars+26*2, 'A');
    chars[26*2] = 0xffff;
    Font::MakeAtlas(img, {16,8}, {128,64}, {{font, ch_map, Font::normal, chars}});
    Texture tex(img, Texture::linear);
    re.SetTexture(tex);
    re.SetMatrix(fmat4::ortho({0,600},{800,0},-1,1));

    auto RenderTextSimple = [&](ivec2 pos, const char *string)
    {
        while (*string)
        {
            const auto &ref = ch_map.Get(*string);
            re.Quad(pos + ref.offset, ref.size).tex(ref.tex_pos);
            pos.x += ref.advance;
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
