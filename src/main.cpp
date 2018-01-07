#include "everything.h"

#include <bitset>
#include <iostream>

constexpr ivec2 screen_sz = ivec2(1920,1080)/3;
constexpr int tile_size = 16;

Events::AutoErrorHandles error_handlers;

Window win("Meow", screen_sz * 2, Window::Settings{}.MinSize(screen_sz).Resizable());
Timing::TickStabilizer tick_stabilizer(60);

Graphics::Image textureimage_main("assets/texture.png");
Graphics::Texture texture_main(Graphics::Texture::nearest),
                  texture_fbuf_main(Graphics::Texture::nearest, screen_sz), texture_fbuf_scaled(Graphics::Texture::linear);
Graphics::FrameBuffer framebuffer_main = nullptr, framebuffer_scaled = nullptr;

Graphics::Font font_object_main;
Graphics::Font font_object_tiny;
Graphics::CharMap font_main;
Graphics::CharMap font_tiny;

Renderers::Poly2D r;

namespace Keys = Input::Keys;
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

    void Resize()
    {
        float scale = (win.Size() / fvec2(screen_sz)).min();
        Draw::scaled_size = iround(screen_sz * scale) * 2 / 2;
        float scale_fl = floor(scale);
        ivec2 scaled_size_fl = iround(screen_sz * scale_fl) * 2 / 2;
        texture_fbuf_scaled.SetData(scaled_size_fl);
        mouse.offset = win.Size() / 2;
        mouse.scale = screen_sz.x / float(Draw::scaled_size.x);
    }

    void Init()
    {
        static bool once = 1;
        if (once) once = 0;
        else Program::Error("Draw::Init() was called twice.");

        Graphics::ClearColor(fvec3(0));
        Graphics::Blending::Enable();
        Graphics::Blending::FuncNormalPre();

        font_object_main.Create("assets/CatIV15.ttf", 15);
        font_object_tiny.Create("assets/CatTiny11.ttf", 11);
        Graphics::Font::MakeAtlas(textureimage_main, ivec2(0,256), ivec2(256,256),
        {
            {font_object_main, font_main, Graphics::Font::light, Strings::Encodings::cp1251()},
            {font_object_tiny, font_tiny, Graphics::Font::light, Strings::Encodings::cp1251()},
        });
        //font_main.EnableLineGap(0);

        texture_main.SetData(textureimage_main);

        r.Create(0x10000);
        r.SetTexture(texture_main);
        r.SetMatrix(fmat4::ortho2D(screen_sz / ivec2(-2,2), screen_sz / ivec2(2,-2)));
        r.SetDefaultFont(font_main);

        Resize();

        framebuffer_main  .Attach(texture_fbuf_main);
        framebuffer_scaled.Attach(texture_fbuf_scaled);
    }

    void FullscreenQuad()
    {
        using Shaders::Identity::Att;
        Att array[] {{{-2,-2}},{{-2,10}},{{10,-2}}};
        static Graphics::VertexBuffer<Att> buf(std::extent_v<decltype(array)>, array);
        buf.Draw(Graphics::triangles);
    }
}

class TileSheet
{
    ivec2 size;
    ivec2 tex_pos;

    using mask_t = std::bitset<tile_size * tile_size>;
    std::vector<mask_t> masks;

  public:
    TileSheet(const Graphics::Image &image, ivec2 size, ivec2 tex_pos) : size(size), tex_pos(tex_pos), masks(size.product())
    {
        for (int i = 0; i < size.product(); i++)
        {
            ivec2 tex_pos = TileTexPos(i);
            auto &mask = masks[i];
            for (int y = 0; y < tile_size; y++)
            for (int x = 0; x < tile_size; x++)
            {
                if (image.FastGet(tex_pos + ivec2(x,y)).a == 255)
                    mask.set(x + y * tile_size);
            }
        }
    }

    ivec2 TexPos() const
    {
        return tex_pos;
    }
    ivec2 Size() const
    {
        return size;
    }

    ivec2 TileTexPos(int id) const
    {
        DebugAssert("Tile id is out of range.", id >= 0 && id < size.product());
        return ivec2(id / size.y, id % size.y) * tile_size + tex_pos;
    }

    bool TileObscuresTile(int big_id, int small_id) const
    {
        auto &big = masks[big_id], &small = masks[small_id];
        return (small & big) == small;
    }
};
TileSheet tilesheet_main(textureimage_main, ivec2(32), ivec2(0,512));

namespace Objects
{
    class Camera
    {
      public:
        ivec2 pos;

        Camera(ivec2 pos) : pos(pos) {}
    };

    class Map
    {
      public:
        enum LayerEnum
        {
            back, front
        };

      private:
        static constexpr int segment_size = 4;

        struct Tile
        {
            int id;
            ivec2 pos;
        };

        struct Segment
        {
            std::vector<Tile> tiles;
        };

        struct Layer
        {
            ivec2 size = ivec2(0);
            int seg_pitch = 0;
            std::vector<Segment> segments;

                  Segment &GetSeg(ivec2 seg_pos)       {return segments[seg_pos.x + seg_pos.y * seg_pitch];}
            const Segment &GetSeg(ivec2 seg_pos) const {return segments[seg_pos.x + seg_pos.y * seg_pitch];}
        };

        TileSheet *tile_sheet = 0;
        Layer la_back, la_front;

        const Layer &GetLayer(LayerEnum layer) const
        {
            switch (layer)
            {
                case back : return la_back ;
                case front: return la_front;
            }
            Program::Error("Invalid layer enum.");
        }
        Layer &GetLayer(LayerEnum layer)
        {
            return (Layer &) ((const Map &)*this).GetLayer(layer);
        }

      public:
        Map() {}
        Map(TileSheet &sheet, ivec2 size) : tile_sheet(&sheet)
        {
            ivec2 seg_size = (size + segment_size - 1) / segment_size;
            int seg_count = seg_size.product();
            for (auto layer : {&la_back, &la_front})
            {
                layer->size = size;
                layer->seg_pitch = seg_size.x;
                layer->segments.resize(seg_count);
            }
        }

        const TileSheet &TileSheet() const
        {
            return *tile_sheet;
        }

        void AddTile(LayerEnum layer, ivec2 pos, int id) // If the new tile obscures some of the old ones, they are removed.
        {
            auto &la = GetLayer(layer);
            if ((pos < 0).any() || (pos >= la.size).any())
                return;
            auto &tiles = la.GetSeg(pos / segment_size).tiles;
            auto it = tiles.begin();
            while (it != tiles.end())
            {
                if (it->pos == pos)
                {
                    if (tile_sheet->TileObscuresTile(id, it->id))
                    {
                        it = tiles.erase(it);
                        continue;
                    }
                }
                it++;
            }
            tiles.push_back(Tile{id, pos});
        }
        void RemoveTile(LayerEnum layer, ivec2 pos, bool remove_all = 0)
        {
            auto &la = GetLayer(layer);
            if ((pos < 0).any() || (pos >= la.size).any())
                return;
            auto &tiles = la.GetSeg(pos / segment_size).tiles;
            auto it = tiles.end();
            while (it != tiles.begin())
            {
                it--;
                if (it->pos == pos)
                {
                    it = tiles.erase(it);
                    if (!remove_all)
                        return;
                }
            }
        }

        void Render(const Scene &scene, LayerEnum layer) const
        {
            auto camera_ptr = scene.GetOpt<Camera>();
            ivec2 camera_pos;
            if (camera_ptr)
                camera_pos = camera_ptr->pos;
            else
                camera_pos = ivec2(0);
            constexpr ivec2 half_extent = screen_sz / 2;
            auto &la = GetLayer(layer);
            ivec2 first_seg = div_ex((camera_pos - half_extent), segment_size),
                  last_seg  = div_ex((camera_pos + half_extent), segment_size),
                  max_seg   = (la.size - 1) / segment_size;

            first_seg = clamp(first_seg, ivec2(0), max_seg);
            last_seg  = clamp(last_seg , ivec2(0), max_seg);

            for (int y = first_seg.y; y <= last_seg.y; y++)
            for (int x = first_seg.x; x <= last_seg.x; x++)
            for (const auto &tile : la.GetSeg({x,y}).tiles)
            {
                DebugAssert(Str("Tile at ", tile.pos, " is incorrectly placed into segment ", ivec2(x,y), "."), (tile.pos >= ivec2(x,y) * segment_size).all() && (tile.pos < (ivec2(x,y) + 1) * segment_size).all());
                r.Quad(tile.pos * tile_size - camera_pos, ivec2(tile_size)).tex(tile_sheet->TileTexPos(tile.id));
            }
        }
    };

    class MapEditor
    {
        bool selecting_tile = 0;
        float selecting_tile_alpha = 0;
      public:
        void Tick(const Scene &scene)
        {
            auto &map = scene.Get<Map>();

            if (mouse.wheel().y > 0)
                selecting_tile = 1;
            if (mouse.wheel().y < 0)
                selecting_tile = 0;

            selecting_tile_alpha = clamp(selecting_tile_alpha + (selecting_tile ? 1 : -1) * 0.12, 0, 1);


            if (mouse.left.pressed())
                map.AddTile(Map::front, div_ex(mouse.pos(), tile_size), 0);
            if (mouse.right.pressed())
                map.RemoveTile(Map::front, div_ex(mouse.pos(), tile_size), Keys::l_ctrl.down());
        }
        void Render(const Scene &scene)
        {
            (void)scene;
            auto &map = scene.Get<Map>();

            if (selecting_tile_alpha)
            {
                // Background
                ivec2 half_extent_tiles = (screen_sz / 2 + tile_size - 1) / tile_size;
                for (int y = -half_extent_tiles.y; y < half_extent_tiles.y; y++)
                for (int x = -half_extent_tiles.x; x < half_extent_tiles.x; x++)
                    r.Quad(ivec2(x,y) * tile_size, ivec2(tile_size)).tex({0,0}).alpha(selecting_tile_alpha);

                // Tile sheet
                ivec2 tilesheet_offset = -max((map.TileSheet().Size() * tile_size - screen_sz).max()/2, 0) * (fvec2(mouse.pos()) / (screen_sz.min()/2)) - map.TileSheet().Size() * tile_size/2;
                r.Quad(tilesheet_offset, map.TileSheet().Size() * tile_size).tex(map.TileSheet().TexPos()).alpha(selecting_tile_alpha);
            }
        }
    };

    class TestObject
    {
        ivec2 pos = ivec2(0);
      public:
        void Tick()
        {
            pos = mouse.pos();
        }
        void Render()
        {
            r.Text(mouse.pos(), "Hello, world!\nYou are {[not] }alone\n1234\n###").callback(
                [&, pos = 0](bool render_pass, uint16_t ch, uint16_t prev, Renderers::Poly2D::Text_t &obj, Graphics::CharMap::Char &glyph, std::vector<Renderers::Poly2D::Text_t::RenderData> &render) mutable
                {
                    (void)prev;

                    if (ch == '{' || ch == '}')
                    {
                        if (ch == '{')
                            obj.color({1,0,0});
                        else
                            obj.color({1,1,1});
                        glyph.advance = 0;
                        render = {};
                    }
                    if (obj.state().color == fvec3(1,0,0))
                    {
                        float f = std::sin(tick_stabilizer.ticks / 40.) / 2 + 0.5;
                        int new_advance = iround(glyph.advance * f);
                        if (render_pass && render.size())
                        {
                            render[0].matrix = render[0].matrix /mul/ fmat3::scale(fvec2(new_advance / float(glyph.advance), 1));
                            render.push_back(render[0]);
                            render[0].matrix = render[0].matrix /mul/ fmat3::translate2D(fvec2(0,1));
                            render[0].color /= 3;
                        }
                        glyph.advance = new_advance;
                    }

                    pos++;
                }).align_v(1);
        }
    };
}

namespace Scenes
{
    using namespace Objects;

    const Scene game = []
    {
        Scene s;
        s.Add<Camera>(ivec2(0));
        s.Add<Map>(tilesheet_main, ivec2(11,7));
        if (1) s.Add<MapEditor>();
        s.Add<TestObject>();
        s.SetTick([](const Scene &s)
        {
            if (auto ptr = s.GetOpt<MapEditor>()) ptr->Tick(s);
            s.Get<TestObject>().Tick();
        });
        s.SetRender([](const Scene &s)
        {
            s.Get<Map>().Render(s, Map::back);
            s.Get<Map>().Render(s, Map::front);
            if (auto ptr = s.GetOpt<MapEditor>()) ptr->Render(s);
            s.Get<TestObject>().Render();
        });
        return s;
    }();
}

Scene current_scene = Scenes::game;

int main(int, char **)
{
    Draw::Init();

    auto Tick = [&]
    {
        current_scene.Tick();
    };
    auto Render = [&]
    {
        Graphics::Clear(Graphics::color);
        current_scene.Render();
    };

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
                Draw::Resize();
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
