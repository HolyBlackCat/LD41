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
#include "wrappers.h"

using namespace Graphics;
using namespace Input;

ReflectStruct(Attributes, (
    (fvec2)(pos),
    (fvec3)(color),
))

ReflectStruct(Uniforms, (
    (Shader::VertexUniform<fvec2>)(screen_size),
))

int main(int, char **)
{
    Window win("Woah", {800,600});

    Shader sh;
    Uniforms uni;
    sh.Create<Attributes, Uniforms>(
        {"vec3 color"},
        R"(void main()
        {
            gl_Position = vec4(a_pos / u_screen_size, 0, 1);
            v_color = a_color;
        })",
        R"(void main()
        {
            gl_FragColor = vec4(v_color,1);
        })",
        &uni
    );
    uni.screen_size << fvec2(4,3);

    Attributes data[]
    {
        {{0,0},{1,0,0}},
        {{1,0},{0,1,0}},
        {{0,1},{0,0,1}},
        {{0,0},{0.5,0,0}},
        {{-0.5,0},{0,0.5,0}},
        {{0,-0.5},{0,0,0.5}},
    };
    VertexBuffer buf(6, data);

    while (1)
    {
        Events::Process();

        if (Keys::f11.pressed())
            win.ToggleFullscreen();

        Graphics::CheckErrors();
        glClear(GL_COLOR_BUFFER_BIT);
        buf.Draw(triangles);

        win.Swap();
    }

    return 0;
}
