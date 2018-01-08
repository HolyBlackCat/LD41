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
    inline namespace TextPresets
    {
        void TinyWithBlackOutline(Renderers::Poly2D::Text_t &obj)
        {
            obj.color(fvec3(1)).font(font_tiny)
               .callback([](bool render_pass, uint16_t ch, uint16_t prev, Renderers::Poly2D::Text_t &obj, Graphics::CharMap::Char &glyph, std::vector<Renderers::Poly2D::Text_t::RenderData> &render)
            {
                (void)ch;
                (void)prev;
                (void)obj;
                (void)glyph;

                if (render_pass)
                {
                    constexpr ivec2 offset_list[]{{1,0},{1,1},{0,1},{-1,1},{-1,0},{-1,-1},{0,-1},{1,-1}};
                    auto copy = render[0];
                    fvec3 saved_color = copy.color;
                    float saved_alpha = copy.alpha;
                    copy.color = fvec3(0);
                    copy.alpha /= 2;
                    render.clear();
                    for (ivec2 offset : offset_list)
                    {
                        auto tmp = copy;
                        tmp.matrix.z.x += offset.x;
                        tmp.matrix.z.y += offset.y;
                        render.push_back(tmp);
                    }
                    copy.color = saved_color;
                    copy.alpha = saved_alpha;
                    render.push_back(copy);
                }
            });
        }
    }

    ivec2 scaled_size;

    void Resize()
    {
        float scale = (win.Size() / fvec2(screen_sz)).min();
        Draw::scaled_size = iround(screen_sz * scale) * 2 / 2;
        float scale_fl = floor(scale);
        ivec2 scaled_size_fl = iround(screen_sz * scale_fl) * 2 / 2;
        texture_fbuf_scaled.SetData(scaled_size_fl);
        mouse.Transform(win.Size() / 2, screen_sz.x / float(Draw::scaled_size.x));
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

    void Rect(ivec2 offset, ivec2 a, ivec2 b, int width, fvec3 color, float alpha = 1, float beta = 1)
    {
        b += a;
        r.Quad(ivec2(a.x-width,a.y) + offset, ivec2(b.x,a.y-width) + offset).absolute().color(color).alpha(alpha).beta(beta);
        r.Quad(ivec2(a.x,a.y) + offset, ivec2(a.x-width,b.y+width) + offset).absolute().color(color).alpha(alpha).beta(beta);
        r.Quad(ivec2(a.x,b.y) + offset, ivec2(b.x+width,b.y+width) + offset).absolute().color(color).alpha(alpha).beta(beta);
        r.Quad(ivec2(b.x,a.y-width) + offset, ivec2(b.x+width,b.y) + offset).absolute().color(color).alpha(alpha).beta(beta);
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

    int TileIdFromSheetPos(ivec2 pos) const
    {
        pos = clamp(pos, 0, size);
        return pos.y + pos.x * size.y;
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

        ivec2 Size()
        {
            return la_back.size;
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
            auto &cam = scene.Get<Camera>();
            constexpr ivec2 half_extent = screen_sz / 2;
            auto &la = GetLayer(layer);
            ivec2 first_seg = div_ex((cam.pos - half_extent), segment_size * tile_size),
                  last_seg  = div_ex((cam.pos + half_extent), segment_size * tile_size),
                  max_seg   = (la.size - 1) / segment_size;

            first_seg = clamp(first_seg, ivec2(0), max_seg);
            last_seg  = clamp(last_seg , ivec2(0), max_seg);

            for (int y = first_seg.y; y <= last_seg.y; y++)
            for (int x = first_seg.x; x <= last_seg.x; x++)
            for (const auto &tile : la.GetSeg({x,y}).tiles)
            {
                DebugAssert(Str("Tile at ", tile.pos, " is incorrectly placed into segment ", ivec2(x,y), "."), (tile.pos >= ivec2(x,y) * segment_size).all() && (tile.pos < (ivec2(x,y) + 1) * segment_size).all());
                r.Quad(tile.pos * tile_size - cam.pos, ivec2(tile_size)).tex(tile_sheet->TileTexPos(tile.id));
            }
        }
    };

    class MapEditor
    {
        bool enabled = 0;
        ivec2 editor_cam_pos = ivec2(0);

        Map::LayerEnum target_layer = Map::back;

        bool selecting_tiles = 0;
        float selecting_tiles_alpha = 0;

        ivec2 selecting_tiles_sheet_offset = ivec2(0),
              selecting_tiles_pos_start = ivec2(0),
              selecting_tiles_pos_end   = ivec2(0);
        bool selecting_tiles_sheet_hovered = 0,
             selecting_tiles_button_down = 0;

        struct GrabbedTile
        {
            int id;
            ivec2 offset;
        };

        std::vector<GrabbedTile> grabbed_tiles;
        ivec2 grabbed_tiles_size = ivec2(0),
              grabbed_tiles_base_offset = ivec2(0),
              grabbed_tiles_base_offset_prev = ivec2(0);

        ivec2 mouse_pos = ivec2(0);

        ivec2 map_selection_start = ivec2(0),
              map_selection_end = ivec2(0);

      public:
        void Enable(const Scene &scene, bool e = 1)
        {
            enabled = e;
            if (enabled)
            {
                editor_cam_pos = scene.Get<Camera>().pos;
            }
        }

        void Tick(const Scene &scene)
        {
            if (Keys::tab.pressed())
                Enable(scene, !enabled);
            if (!enabled)
                return;

            auto &map = scene.Get<Map>();
            auto &cam = scene.Get<Camera>();

            { // Open/close tile sheet
                if (mouse.wheel().y > 0)
                    selecting_tiles = 1;
                if (mouse.wheel().y < 0 && !selecting_tiles_button_down)
                    selecting_tiles = 0;
                selecting_tiles_alpha = clamp(selecting_tiles_alpha + (selecting_tiles ? 1 : -1) * 0.12, 0, 1);
            }

            ivec2 wasd = ivec2(Keys::d.down() - Keys::a.down(), Keys::s.down() - Keys::w.down());

            { // Camera
                if (!selecting_tiles)
                {
                    int speed = 6;
                    if (Keys::l_shift.down())
                        speed = 1;
                    if (Keys::l_ctrl.down())
                        speed = 20;
                    clamp_assign(editor_cam_pos += wasd * speed, 0, map.Size() * tile_size);
                }
                cam.pos = editor_cam_pos;
            }

            // Tile sheet
            if (selecting_tiles)
            {
                { // Tile sheet camera panning
                    ivec2 sheet_half_extent = map.TileSheet().Size() * tile_size/2;
                    clamp_assign(selecting_tiles_sheet_offset -= wasd * 2, -sheet_half_extent, sheet_half_extent);
                }

                ivec2 cur_pos = div_ex(mouse.pos() - selecting_tiles_sheet_offset + map.TileSheet().Size() * tile_size/2, tile_size);
                selecting_tiles_sheet_hovered = (cur_pos >= 0).all() && (cur_pos < map.TileSheet().Size()).all();
                cur_pos = clamp(cur_pos, 0, map.TileSheet().Size()-1);

                selecting_tiles_pos_end = cur_pos;
                if (mouse.left.pressed() && selecting_tiles_sheet_hovered)
                {
                    selecting_tiles_button_down = 1;
                    selecting_tiles_pos_start = cur_pos;
                }
                if (selecting_tiles_button_down && mouse.left.released())
                {
                    selecting_tiles_button_down = 0;
                    ivec2 a = selecting_tiles_pos_start,
                          b = selecting_tiles_pos_end;
                    if (a.x > b.x) std::swap(a.x, b.x);
                    if (a.y > b.y) std::swap(a.y, b.y);

                    grabbed_tiles_size = b - a + 1;
                    grabbed_tiles = {};
                    for (int y = a.y; y <= b.y; y++)
                    for (int x = a.x; x <= b.x; x++)
                        grabbed_tiles.push_back({map.TileSheet().TileIdFromSheetPos({x,y}), ivec2(x,y) - a});

                    selecting_tiles = 0;
                }
            }

            // Editing the map
            if (!selecting_tiles) // Not `else`.
            {
                mouse_pos = div_ex(mouse.pos() + cam.pos, tile_size);

                if (grabbed_tiles.size())
                {
                    grabbed_tiles_base_offset_prev = grabbed_tiles_base_offset;
                    grabbed_tiles_base_offset = div_ex(-grabbed_tiles_size * tile_size/2 + mouse.pos() + cam.pos + tile_size/2, tile_size);

                    if (mouse.left.pressed() || (mouse.left.down() && grabbed_tiles_base_offset != grabbed_tiles_base_offset_prev))
                        for (const auto &it : grabbed_tiles)
                            map.AddTile(target_layer, grabbed_tiles_base_offset + it.offset, it.id);

                    if (mouse.right.pressed())
                        grabbed_tiles = {};
                }

                /*
                if (mouse.left.pressed())
                    map.AddTile(Map::front, div_ex(mouse.pos(), tile_size), 0);
                if (mouse.right.pressed())
                    map.RemoveTile(Map::front, div_ex(mouse.pos(), tile_size), Keys::l_ctrl.down());
                */
            }
        }
        void Render(const Scene &scene)
        {
            if (!enabled)
                return;

            auto &map = scene.Get<Map>();
            auto &cam = scene.Get<Camera>();

            constexpr int period = 60;
            float t = tick_stabilizer.ticks % period / float(period/2);
            t = (t < 1 ? smoothstep(t) : smoothstep(2-t));

            // Grabbed tiles
            if (!selecting_tiles && grabbed_tiles.size() > 0)
            {
                for (ivec2 offset : {ivec2( 1, 1), ivec2(-1, 1), ivec2(-1,-1), ivec2( 1,-1),
                                     ivec2( 1, 0), ivec2( 0, 1), ivec2(-1, 0), ivec2( 0,-1)})
                {
                    for (const auto &it : grabbed_tiles)
                    {
                        ivec2 tile_pos = grabbed_tiles_base_offset + it.offset;
                        float t = mod_ex(tick_stabilizer.ticks - tile_pos.sum()*4, period) / float(period/2);
                        t = (t < 1 ? smoothstep(t) : smoothstep(2-t));
                        r.Quad(tile_pos * tile_size - cam.pos + offset, ivec2(16)).tex(map.TileSheet().TileTexPos(it.id)).color(fvec3(t)).mix(0);
                    }
                }
                for (const auto &it : grabbed_tiles)
                {
                    r.Quad((grabbed_tiles_base_offset + it.offset) * tile_size - cam.pos, ivec2(16)).tex(map.TileSheet().TileTexPos(it.id));
                }

            }

            { // Map border
                constexpr int width = 2;
                for (int i = 0; i <= 1; i++)
                    Draw::Rect(-cam.pos, ivec2(width * -i), map.Size() * tile_size + ivec2(width * (1-i)), width, fvec3(i), 0.75);
            }

            // Mouse cursor
            if (!selecting_tiles && grabbed_tiles.size() == 0)
                Draw::Rect(mouse_pos * tile_size - cam.pos, ivec2(0), ivec2(tile_size), 1, fvec3(t), 0.75);

            // Tile selector
            if (selecting_tiles_alpha)
            {
                // Background
                ivec2 half_extent_tiles = (screen_sz / 2 + tile_size - 1) / tile_size;
                for (int y = -half_extent_tiles.y; y < half_extent_tiles.y; y++)
                for (int x = -half_extent_tiles.x; x < half_extent_tiles.x; x++)
                    r.Quad(ivec2(x,y) * tile_size, ivec2(tile_size)).tex({0,0}).alpha(selecting_tiles_alpha);

                // Tile sheet
                r.Quad(selecting_tiles_sheet_offset, map.TileSheet().Size() * tile_size).tex(map.TileSheet().TexPos()).alpha(selecting_tiles_alpha).center();

                ivec2 pos = selecting_tiles_sheet_offset - map.TileSheet().Size() * tile_size/2;

                // Selection rectangle
                if (!selecting_tiles_button_down)
                {
                    if (selecting_tiles_sheet_hovered)
                        r.Quad(selecting_tiles_pos_end * tile_size + tile_size/2 + pos, ivec2(32)).tex({16,0}).center().alpha(selecting_tiles_alpha/2);
                }
                else
                {
                    ivec2 at = selecting_tiles_pos_start,
                          bt = selecting_tiles_pos_end;
                    if (at.x > bt.x) std::swap(at.x, bt.x);
                    if (at.y > bt.y) std::swap(at.y, bt.y);

                    ivec2 a = at * tile_size + tile_size/2 + pos,
                          b = bt * tile_size + tile_size/2 + pos;

                    // Corners
                    r.Quad(ivec2(a.x,a.y), ivec2(8)).tex({16   ,0   }).center({16,16}).alpha(selecting_tiles_alpha);
                    r.Quad(ivec2(b.x,a.y), ivec2(8)).tex({16+24,0   }).center({-8,16}).alpha(selecting_tiles_alpha);
                    r.Quad(ivec2(a.x,b.y), ivec2(8)).tex({16   ,0+24}).center({16,-8}).alpha(selecting_tiles_alpha);
                    r.Quad(ivec2(b.x,b.y), ivec2(8)).tex({16+24,0+24}).center({-8,-8}).alpha(selecting_tiles_alpha);
                    // Sides
                    for (int x = at.x; x <= bt.x; x++)
                    {
                        r.Quad(ivec2(x,at.y) * tile_size + tile_size/2 + pos, {16,8}).tex({16+8,0   }).center({8,16}).alpha(selecting_tiles_alpha);
                        r.Quad(ivec2(x,bt.y) * tile_size + tile_size/2 + pos, {16,8}).tex({16+8,0+24}).center({8,-8}).alpha(selecting_tiles_alpha);
                    }
                    for (int y = at.y; y <= bt.y; y++)
                    {
                        r.Quad(ivec2(at.x,y) * tile_size + tile_size/2 + pos, {8,16}).tex({16   ,0+8}).center({16,8}).alpha(selecting_tiles_alpha);
                        r.Quad(ivec2(bt.x,y) * tile_size + tile_size/2 + pos, {8,16}).tex({16+24,0+8}).center({-8,8}).alpha(selecting_tiles_alpha);
                    }
                }
            }

            { // Text
                std::string top_left;
                if (!selecting_tiles)
                    top_left = "Editing: tiles";
                else
                    top_left = "Tile sheet";

                ivec2 cam_pos = div_ex(cam.pos, tile_size),
                      pos = (grabbed_tiles.size() > 0 ? grabbed_tiles_base_offset : mouse_pos);
                std::string bottom_left = Str("Camera center: [", std::setw(5), cam_pos.x, ",", std::setw(5), cam_pos.y, "]",
                                              "          Pos: [", std::setw(5), pos.x, ",", std::setw(5), pos.y, "]");
                if (grabbed_tiles.size() > 0 && grabbed_tiles_size != ivec2(1))
                    bottom_left += Str("          Size: [", std::setw(3), grabbed_tiles_size.x, ",", std::setw(3), grabbed_tiles_size.y, "]");

                r.Text(-screen_sz/2 + 2         , top_left     ).preset(Draw::TinyWithBlackOutline).align({-1,-1});
                r.Text((screen_sz/2-2).mul_x(-1), bottom_left  ).preset(Draw::TinyWithBlackOutline).align({-1,1});
            }
        }
    };

    class TestObject
    {
        ivec2 pos = ivec2(0);
      public:
        void Tick()
        {
            return;
            pos = mouse.pos();
        }
        void Render()
        {
            return;
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
        bool map_editor = 1;

        Scene s;
        s.Add<Camera>(ivec2(0));
        s.Add<Map>(tilesheet_main, ivec2(11,7));
        if (map_editor) s.Add<MapEditor>();

        s.Get<Camera>().pos = s.Get<Map>().Size() * tile_size / 2;
        if (map_editor) s.Get<MapEditor>().Enable(s);

        s.SetTick([](const Scene &s)
        {
            if (auto ptr = s.GetOpt<MapEditor>()) ptr->Tick(s);
        });
        s.SetRender([](const Scene &s)
        {
            s.Get<Map>().Render(s, Map::back);
            s.Get<Map>().Render(s, Map::front);
            if (auto ptr = s.GetOpt<MapEditor>()) ptr->Render(s);
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
