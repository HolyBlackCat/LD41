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
#include "string.h"
#include "template_utils.h"
#include "ui.h"
#include "window.h"
#include "utils.h"

using namespace Graphics;
using namespace Input;

ReflectStruct(Attributes, (
    (fvec2)(pos,tex_coord),
))

ReflectStruct(Uniforms, (
    (Shader::VertexUniform<fmat2>)(matrix),
    (Shader::VertexUniform<fvec2>)(screen_size),
    (Shader::FragmentUniform<Texture>)(texture),
))

int main(int, char **)
{
    Window win("Woah", {800,600});

    Shader sh;
    Uniforms uni;
    sh.Create<Attributes>(
        {"vec2 tex_coord"},
        R"(void main()
        {
            gl_Position = vec4(u_matrix * a_pos / u_screen_size, 0, 1);
            v_tex_coord = a_tex_coord;
        })",
        R"(void main()
        {
            gl_FragColor = texture2D(u_texture, v_tex_coord);
        })",
        &uni
    );
    uni.screen_size = fvec2(4,3);
    Texture tex(Image("test.png"), Texture::linear);
    uni.texture = tex;

    Attributes data[]
    {
        {{-1,-1},{0,0}},
        {{1,-1},{1,0}},
        {{-1,1},{0,1}},
        {{-1,1},{0,1}},
        {{1,-1},{1,0}},
        {{1,1},{1,1}},
    };
    VertexBuffer buf(6, data);

    while (1)
    {
        Events::Process();

        if (Keys::f11.pressed())
            win.ToggleFullscreen();

        Graphics::CheckErrors();
        glClear(GL_COLOR_BUFFER_BIT);
        uni.matrix = fmat2::rotate2D(Events::Time() / 60.);
        buf.Draw(triangles);

        win.Swap();
    }

    return 0;
}
