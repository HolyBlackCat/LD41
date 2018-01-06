#include "everything.h"

#include <iostream>

constexpr ivec2 screen_sz = ivec2(1920,1080)/3;
constexpr int tile_size = 16;

Events::AutoErrorHandles error_handlers;

Window win("Meow", screen_sz * 2, Window::Settings{}.MinSize(screen_sz).Resizable());
Timing::TickStabilizer tick_stabilizer(60);

Graphics::Texture texture_main(Graphics::Texture::nearest),
                  texture_fbuf_main(Graphics::Texture::nearest, screen_sz), texture_fbuf_scaled(Graphics::Texture::linear);
Graphics::FrameBuffer framebuffer_main = nullptr, framebuffer_scaled = nullptr;

Graphics::Font font_object_main;
Graphics::Font font_object_tiny;
Graphics::CharMap font_main;
Graphics::CharMap font_tiny;

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

        Graphics::Image img("assets/texture.png");
        font_object_main.Create("assets/CatIV15.ttf", 15);
        font_object_tiny.Create("assets/CatTiny11.ttf", 11);
        Graphics::Font::MakeAtlas(img, ivec2(0), ivec2(256),
        {
            {font_object_main, font_main, Graphics::Font::light, Strings::Encodings::cp1251()},
            {font_object_tiny, font_tiny, Graphics::Font::light, Strings::Encodings::cp1251()},
        });
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

  public:
    TileSheet(ivec2 size, ivec2 tex_pos) : size(size), tex_pos(tex_pos) {}

    ivec2 GetTileTexPos(int id)
    {
        DebugAssert("Tile id is out of range.", id >= 0 && id < size.product());
        return ivec2(id / size.y, id % size.y) * tile_size + tex_pos;
    }
};

class Map
{
  public:
    enum LayerEnum
    {
        layer_back, layer_front
    };
    enum RemoveTileMode
    {
        front, back, all
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
            case layer_back : return la_back ;
            case layer_front: return la_front;
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

    void AddTile(LayerEnum layer, ivec2 pos, int id)
    {
        auto &la = GetLayer(layer);
        if ((pos < 0).any() || (pos >= la.size).any())
            return;
        la.GetSeg(pos / segment_size).tiles.push_back(Tile{id, pos});
    }
    void RemoveTile(LayerEnum layer, ivec2 pos, RemoveTileMode mode, int id = -1)
    {
        auto &la = GetLayer(layer);
        if ((pos < 0).any() || (pos >= la.size).any())
            return;
        auto &seg = la.GetSeg(pos / segment_size);
        switch (mode)
        {
          case front:
          case all:
            {
                auto it = seg.tiles.begin();
                while (it != seg.tiles.end())
                {
                    if (it->pos == pos && (id == -1 || id == it->id))
                    {
                        if (mode == front)
                        {
                            seg.tiles.erase(it);
                            return;
                        }
                        it = seg.tiles.erase(it);
                        continue;
                    }
                    it++;
                }
            }
            break;
          case back:
            {
                auto it = seg.tiles.rbegin();
                while (it != seg.tiles.rend())
                {
                    if (it->pos == pos && (id == -1 || id == it->id))
                    {
                        seg.tiles.erase(it.base()-1);
                        return;
                    }
                    it++;
                }
            }
            break;
        }
    }

    void Render(LayerEnum layer, ivec2 camera_pos, ivec2 half_extent) const
    {
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
            r.Quad(tile.pos * tile_size - camera_pos, ivec2(tile_size)).tex(tile_sheet->GetTileTexPos(tile.id));
        }
    }
};

class TestObject
{
    ivec2 pos = ivec2(0);
  public:
    void Tick(Scenes::Scene &)
    {
        pos = mouse.pos();
    }
    void Render(Scenes::Scene &)
    {
        r.Text(mouse.pos(), "Hello, world!\nYou are {[not] }alone\n1234\n###").callback(
            [&, pos = 0](bool render_pass, uint16_t ch, uint16_t prev, Renderers::Poly2D::Text_t &obj, Graphics::CharMap::Char &glyph, std::vector<Renderers::Poly2D::Text_t::RenderData> &render) mutable
            {
                (void)render;
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

namespace Scenes
{
    using Game = Scenes::SceneTemplate<Scenes::value_list<&TestObject::Tick>,
                                       Scenes::value_list<&TestObject::Render>>;
}

std::unique_ptr<Scenes::Scene> scene = std::make_unique<Scenes::Game>();

int main(int, char **)
{
    Draw::Init();

    scene->GetContainer<TestObject>().push_back({});

    TileSheet tile_sheet(ivec2(32), ivec2(0,512));
    Map map(tile_sheet, ivec2(11,7));

    auto Tick = [&]
    {
        if (mouse.left.pressed())
            map.AddTile(map.layer_front, div_ex(mouse.pos(), tile_size), 0);
        if (mouse.right.pressed())
            map.RemoveTile(map.layer_front, div_ex(mouse.pos(), tile_size), map.front);;

        scene->Tick();
    };
    auto Render = [&]
    {
        Graphics::Clear(Graphics::color);

        map.Render(map.layer_back , ivec2(0), screen_sz/2);
        map.Render(map.layer_front, ivec2(0), screen_sz/2);

//        r.Quad(mouse.pos(), ivec2(32)).tex(ivec2(0));

        scene->Render();
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
