#include "everything.h"

#include <iostream>

constexpr ivec2 screen_sz = ivec2(1920,1080)/3;

Events::AutoErrorHandles error_handlers;

Window win("Meow", screen_sz * 2, Window::Settings{}.MinSize(screen_sz).Resizable());
Timing::TickStabilizer tick_stabilizer(60);

Graphics::Texture texture_main(Graphics::Texture::nearest),
                  texture_fbuf_main(Graphics::Texture::nearest, screen_sz), texture_fbuf_scaled(Graphics::Texture::linear);
Graphics::FrameBuffer framebuffer_main = nullptr, framebuffer_scaled = nullptr;

Graphics::Font font_object_main;
Graphics::CharMap font_main;

Renderers::Poly2D r;

Input::Mouse mouse;

namespace Shaders
{
    using Graphics::Shader;
    template <typename T> using Uniform   = Graphics::Shader::Uniform<T>;
    template <typename T> using Uniform_v = Graphics::Shader::Uniform_v<T>;
    template <typename T> using Uniform_f = Graphics::Shader::Uniform_f<T>;
    using Graphics::Texture;
    namespace Identity
    {
        ReflectStruct(Att, (
            (fvec2)(pos),
        ))
        ReflectStruct(Uni, (
            (Uniform_f<Texture>)(texture),
        ))
        Uni uni;
        Shader object = Shader::Make<Att>("Identity", R"(
VARYING(vec2, pos)
void main()
{
    v_pos = a_pos;
    gl_Position = vec4(a_pos * 2. - 1., 0, 1);
})", R"(
VARYING(vec2, pos)
void main()
{
    gl_FragColor = texture2D(u_texture, v_pos);
})", &uni);
    }
}

namespace Draw
{
    ivec2 scaled_size;

    void FullscreenQuad()
    {
        using Shaders::Identity::Att;
        Att array[] {{{-2,-2}},{{-2,10}},{{10,-2}}};
        static Graphics::VertexBuffer<Att> buf(std::extent_v<decltype(array)>, array);
        buf.Draw(Graphics::triangles);
    }
}

int main(int, char **)
{
    auto Tick = [&]
    {

    };
    auto Render = [&]
    {
        Graphics::Clear(Graphics::color);
        //r.Quad(mouse.pos(), ivec2(32)).tex(ivec2(0));
        r.Text(mouse.pos(), "Hello, world!\n12""\xff""34\n###").callback(
            [&](uint16_t ch, uint16_t prev, Renderers::Poly2D::Text_t &obj, Graphics::CharMap::Char &info, fmat3 &out_mat)
            {
                (void)prev;

                if (ch == '2')
                    obj.color({1,0,0});
                if (ch == '4')
                    obj.color({1,1,1});
                if (obj.state().color == fvec3(1,0,0))
                {
                    float f = std::sin(tick_stabilizer.ticks / 40.) / 2 + 0.5;
                    int new_advance = iround(info.advance * f);
                    out_mat = fmat3::scale(fvec2(new_advance / float(info.advance), 1));
                    info.advance = new_advance;
                }
            }).align_v(1);
    };


    auto Resize = [&]
    {
        float scale = (win.Size() / fvec2(screen_sz)).min();
        Draw::scaled_size = iround(screen_sz * scale) * 2 / 2;
        float scale_fl = floor(scale);
        ivec2 scaled_size_fl = iround(screen_sz * scale_fl) * 2 / 2;
        texture_fbuf_scaled.SetData(scaled_size_fl);
        mouse.offset = win.Size() / 2;
        mouse.scale = screen_sz.x / float(Draw::scaled_size.x);
    };

    { // Init
        Graphics::Image img("assets/texture.png");
        font_object_main.Create("assets/CatIV15.ttf", 15);
        Graphics::Font::MakeAtlas(img, ivec2(0), ivec2(256), { {font_object_main, font_main, Graphics::Font::light, Strings::Encodings::cp1251()} });
        //font_main.EnableLineGap(0);

        texture_main.SetData(img);

        r.Create(0x10000);
        r.SetTexture(texture_main);
        r.SetMatrix(fmat4::ortho2D(screen_sz / ivec2(-2,2), screen_sz / ivec2(2,-2)));
        r.SetDefaultFont(font_main);

        Resize();

        framebuffer_main  .Attach(texture_fbuf_main);
        framebuffer_scaled.Attach(texture_fbuf_scaled);
    }

    uint64_t frame_start = Timing::Clock(), frame_delta;

    while (1)
    {
        uint64_t time = Timing::Clock();
        frame_delta = time - frame_start;
        frame_start = time;

        while (tick_stabilizer.Tick(frame_delta))
        {
            Events::Process();
            if (win.size_changed)
            {
                win.size_changed = 0;
                Resize();
            }
            Tick();
        }

        Graphics::CheckErrors();

        framebuffer_main.Bind();
        r.BindShader();
        Graphics::Viewport(screen_sz);

        Render();
        r.Finish();

        framebuffer_scaled.Bind();
        Shaders::Identity::object.Bind();
        Shaders::Identity::uni.texture = texture_fbuf_main;
        Graphics::Viewport(texture_fbuf_scaled.Size());

        Draw::FullscreenQuad();

        Graphics::FrameBuffer::Unbind();
        Shaders::Identity::uni.texture = texture_fbuf_scaled;
        Graphics::Viewport((win.Size() - Draw::scaled_size) / 2, Draw::scaled_size);

        Draw::FullscreenQuad();

        win.Swap();
    }

    return 0;
}
