#include "everything.h"

#include <cstddef>
#include <iostream>
#include <numeric>
#include <random>

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

    struct Tri
    {
        fvec2 pos;
        float angle;
        fvec3 color;
    };
    static constexpr int tri_count = 500;
    static constexpr float max_tri_size = 30;
    int pos = 0;
    Tri arr[tri_count];

    std::mt19937 rng;

    while (1)
    {
        Events::Process();

        if (Keys::f11.pressed())
            win.ToggleFullscreen();

        Graphics::CheckErrors();
        Clear(color);

        int c = min(pos, tri_count);
        for (int i = 0; i < c; i++)
        {
            float phase = (pos - i) % tri_count / float(tri_count);
            re.Triangle_f(arr[i].pos + fmat2::rotate2D(arr[i].angle) /mul/ fvec2(ipow(phase, 2) * 60, 0), fvec2(max_tri_size, 0), fvec2(-max_tri_size/2, max_tri_size*std::sin(f_pi/3)), fvec2(-max_tri_size/2, -max_tri_size*std::sin(f_pi/3))).color(arr[i].color).scale_f(smoothstep(1-2*abs(phase - 0.5)) * (arr[i].pos / fvec2(400,300) - fvec2(1)).len()).rotate(arr[i].angle + phase*10 * (arr[i].pos / fvec2(400,300) - fvec2(1)).product());
        }

        for (int i = 0; i < 3; i++)
        {
            int p = pos % tri_count;
            arr[p].pos = ivec2(rng() % 800u, rng() % 600u);
            arr[p].angle = (rng() % 1000u) / 500. * f_pi;
            float r = rng() % 2000u / 1000. - 1;
            arr[p].color = (fvec2(arr[p].pos.x / 800., arr[p].pos.y / 600.) + r * 0.25).to_vec3(1);
            pos++;
        }


        //re.Text({400,300}, "Hello").color({0,0.5,1}).append("world!").color({1,0.5,0});
        //re.Triangle(mouse.pos(), {0,-30},{30,30},{-30,30}).tex({30,0},{60,60},{0,60}).color({1,0,0},{0,1,0},{0,0,1}).mix(0.5).alpha(0.5).rotate(Events::Time() / 30.);

        re.Finish();

        win.Swap();
    }

    return 0;
}
