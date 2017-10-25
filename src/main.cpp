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
ReflectStruct(Varyings, (
    (fvec3)(color),
))
ReflectStruct(Uniforms, (
    (Shader::VertexUniform<fvec2>)(screen_size),
))

int main(int, char **)
{
    Window win("Woah", {800,600}, Window::Settings{}.Resizable(0));

    Shader sh;
    Uniforms uni;
    try
    {
        sh.Create<Attributes, Varyings, Uniforms>("void main(){gl_Position = vec4(a_pos / u_screen_size,0,1); v_color = a_color;}", "void main(){gl_FragColor = vec4(v_color,1);}", &uni);
    }
    catch (std::exception &e)
    {
        std::cout << e.what();
        std::system("pause");
        return 0;
    }
    sh.Bind();
    glUniform2f(uni.screen_size.location, 4, 3);

    float buffer[]
    {
        0,0, 1,0,0,
        1,0, 0,1,0,
        0,1, 0,0,1,
        0,0, 0.5,0,0,
        -0.5,0, 0,0.5,0,
        0,-0.5, 0,0,0.5,
    };
    Graphics::VertexByteBuffer buf(sizeof buffer, (const char *)buffer);
    buf.Bind();
    glEnableVertexAttribArray(0);
    glEnableVertexAttribArray(1);
    glVertexAttribPointer(0, 2, GL_FLOAT, 0, sizeof (Attributes), (void *)0);
    glVertexAttribPointer(1, 3, GL_FLOAT, 0, sizeof (Attributes), (void *)(2*sizeof (float)));

    while (1)
    {
        Events::Process();

        if (Keys::f11.pressed())
            win.ToggleFullscreen();

        glClear(GL_COLOR_BUFFER_BIT);
        /*
        glBegin(GL_TRIANGLES);
        glColor3f(0.1, 0.2, 0.3);
        glVertex3f(0, 0, 0);
        glVertex3f(1, 0, 0);
        glVertex3f(0, 1, 0);
        glEnd();*/
        glDrawArrays(GL_TRIANGLES, 0, 6);

        win.Swap();
    }

    return 0;
}
