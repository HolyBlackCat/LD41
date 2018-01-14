#include "everything.h"

#include <bitset>
#include <iostream>

constexpr ivec2 screen_sz = ivec2(1920,1080)/3;
constexpr int tile_size = 16;

Events::AutoErrorHandles error_handlers;

Window win("Meow", screen_sz * 2, Window::Settings{}.MinSize(screen_sz).Resizable());
Timing::TickStabilizer tick_stabilizer(60);

Graphics::Image textureimage_main;
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

class TileSheet
{
    ivec2 size = ivec2(0);
    ivec2 tex_pos = ivec2(0);

    using mask_t = std::bitset<tile_size * tile_size>;
    std::vector<mask_t> masks;

  public:
    TileSheet() {}
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
    const mask_t TileMask(int id) const
    {
        return masks[id];
    }
};
TileSheet tilesheet_main;

namespace Draw
{
    inline namespace TextPresets
    {
        void TinyWithBlackOutline(Renderers::Poly2D::Text_t &obj) // Is a preset
        {
            obj.color(fvec3(1)).font(font_tiny)
               .callback([](Renderers::Poly2D::Text_t::CallbackParams params)
            {
                if (params.render_pass && params.render.size())
                {
                    constexpr ivec2 offset_list[]{{1,0},{1,1},{0,1},{-1,1},{-1,0},{-1,-1},{0,-1},{1,-1}};
                    auto copy = params.render[0];
                    fvec3 saved_color = copy.color;
                    float saved_alpha = copy.alpha;
                    copy.color = fvec3(0);
                    copy.alpha *= 0.6;
                    params.render.clear();
                    for (ivec2 offset : offset_list)
                    {
                        auto tmp = copy;
                        tmp.matrix.z.x += offset.x;
                        tmp.matrix.z.y += offset.y;
                        params.render.push_back(tmp);
                    }
                    copy.color = saved_color;
                    copy.alpha = saved_alpha;
                    params.render.push_back(copy);
                }
            });
        }

        auto WithCursor(int index, fvec3 color = fvec3(tick_stabilizer.ticks % 60 < 30)) // Returns a preset
        {
            return [=](Renderers::Poly2D::Text_t &obj)
            {
                obj.callback([&](Renderers::Poly2D::Text_t::CallbackParams params)
                {
                    constexpr int width = 1;
                    if (params.render_pass && params.index == index)
                    {
                        r.Quad(params.pos - ivec2(width, params.obj.state().ch_map->Ascent()), ivec2(1, params.obj.state().ch_map->Height()))
                         .color(color).alpha(params.render[0].alpha).beta(params.render[0].beta);
                    }
                });
            };
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

    void ReloadTextures()
    {
        textureimage_main.FromFile("assets/texture.png");
        Graphics::Font::MakeAtlas(textureimage_main, ivec2(0,256), ivec2(256,256),
        {
            {font_object_main, font_main, Graphics::Font::light, Strings::Encodings::cp1251()},
            {font_object_tiny, font_tiny, Graphics::Font::light, Strings::Encodings::cp1251()},
        });
        /*
        font_main.EnableLineGap(0);
        font_tiny.EnableLineGap(0);
        */

        texture_main.SetData(textureimage_main);

        tilesheet_main = TileSheet(textureimage_main, ivec2(32), ivec2(0,512));
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

        ReloadTextures();

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

namespace Objects
{
    class Camera
    {
      public:
        ivec2 pos;

        Camera(ivec2 pos) : pos(pos) {}
    };

    class Background
    {
      public:
        void Tick()
        {

        }
        void Render()
        {
            constexpr fvec3 c1 = fvec3(209,255,255)/255,
                            c2 = fvec3(174,210,226)/255;
            r.Quad(-screen_sz/2, screen_sz).color(c1,c1,c2,c2);
        }
    };

    class Map
    {
      public:
        enum LayerEnum
        {
            back, front
        };

      private:
        static constexpr int segment_size = 8;

        struct Tile
        {
            Reflect(Tile)
            (
                (int)(id),
                (ivec2)(pos),
            )
        };

        struct Segment
        {
            Reflect(Segment)
            (
                (std::vector<Tile>)(tiles),
            )
        };

        struct Layer
        {
            Reflect(Layer)
            (
                (ivec2)(size)(= ivec2(0)),
                (int)(seg_pitch)(= 0),
                (std::vector<Segment>)(segments),
            )

                  Segment &GetSeg(ivec2 seg_pos)       {return segments[seg_pos.x + seg_pos.y * seg_pitch];}
            const Segment &GetSeg(ivec2 seg_pos) const {return segments[seg_pos.x + seg_pos.y * seg_pitch];}
        };

        TileSheet *tile_sheet = 0;

        ReflectStruct(Data, (
            (Layer)(la_back, la_front),
        ))

        Data data;

        const Layer &GetLayer(LayerEnum layer) const
        {
            switch (layer)
            {
                case back : return data.la_back ;
                case front: return data.la_front;
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
            for (auto layer : {&data.la_back, &data.la_front})
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
            return data.la_back.size;
        }

        void AddTile(LayerEnum layer, ivec2 pos, int id) // If the new tile obscures some of the old ones, they are removed.
        {
            auto &la = GetLayer(layer);
            if ((pos < 0).any() || (pos >= la.size).any())
                return;
            auto mask = TileSheet().TileMask(id);
            auto &tiles = la.GetSeg(pos / segment_size).tiles;
            auto it = tiles.end();
            while (it != tiles.begin())
            {
                it--;
                if (it->pos == pos)
                {
                    auto new_mask = mask | TileSheet().TileMask(it->id);
                    if (mask == new_mask)
                    {
                        it = tiles.erase(it);
                        continue;
                    }
                    else
                        mask = new_mask;
                }
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

        std::vector<int> Get(LayerEnum layer, ivec2 pos) const
        {
            const auto &la = GetLayer(layer);
            if ((pos < 0).any() || (pos >= la.size).any())
                return {};
            const auto &tiles = la.GetSeg(pos / segment_size).tiles;
            std::vector<int> ret;
            for (const auto &it : tiles)
                if (it.pos == pos)
                    ret.push_back(it.id);
            return ret;
        }
        int GetTop(LayerEnum layer, ivec2 pos) const
        {
            const auto &la = GetLayer(layer);
            if ((pos < 0).any() || (pos >= la.size).any())
                return {};
            const auto &tiles = la.GetSeg(pos / segment_size).tiles;
            int ret = -1;
            for (const auto &it : tiles)
                if (it.pos == pos)
                    ret = it.id;
            return ret;
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

        bool SaveToFile(std::string fname)
        {
            std::string str = Reflection::to_string(data);
            return Utils::WriteToFile(fname, (uint8_t *)str.data(), str.size(), Utils::compressed);
        }
        bool LoadFromFile(std::string fname)
        {
            Utils::MemoryFile file;
            try
            {
                file.Create(fname, Utils::compressed);
            }
            catch(decltype(Utils::file_input_error("","")) &e)
            {
                return 0;
            }

            auto data_copy = data;
            if (!Reflection::from_string(data, (char *)file.Data())) // `MemoryFile::Data()` is null-terminated, so we're fine.
            {
                data = data_copy;
                return 0;
            }

            return 1;
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
              grabbed_tiles_base_offset = ivec2(0);

        ivec2 mouse_pos = ivec2(0);

        ivec2 map_selection_start = ivec2(0),
              map_selection_end = ivec2(0);
        bool map_selection_button_down = 0,
             map_selection_map_hovered = 0,
             map_selection_multiple_tiles = 0;

        enum class Mode {copy, erase, select};
        Mode current_mode = Mode::copy;

        bool show_help = 1;

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
            if (Keys::grave.pressed())
                Enable(scene, !enabled);
            if (!enabled)
                return;

            auto &map = scene.Get<Map>();
            auto &cam = scene.Get<Camera>();

            { // Reload textures if needed
                if (Keys::f5.pressed())
                    Draw::ReloadTextures();
            }

            { // Open/close tile sheet
                if (!selecting_tiles && Keys::tab.pressed() && !map_selection_button_down)
                {
                    selecting_tiles = 1;
                    current_mode = Mode::copy;
                }
                else if (selecting_tiles && Keys::tab.pressed() && !selecting_tiles_button_down)
                    selecting_tiles = 0;
                selecting_tiles_alpha = clamp(selecting_tiles_alpha + (selecting_tiles ? 1 : -1) * 0.12, 0, 1);
            }

            { // Show/hide help text
                if (Keys::f1.pressed())
                    show_help = !show_help;
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
                { // Switching modes
                    Mode prev_mode = current_mode;

                    if (Keys::q.pressed())
                    {
                        current_mode = Mode::copy;
                    }
                    if (Keys::e.pressed())
                    {
                        if (current_mode != Mode::erase)
                            current_mode = Mode::erase;
                        else
                            current_mode = Mode::copy;
                    }
                    if (Keys::r.pressed())
                    {
                        if (current_mode != Mode::select)
                            current_mode = Mode::select;
                        else
                            current_mode = Mode::copy;
                    }

                    if (current_mode != prev_mode)
                    {
                        grabbed_tiles = {};
                        map_selection_button_down = 0;
                        map_selection_start = ivec2(0);
                        map_selection_end = ivec2(0);
                        map_selection_multiple_tiles = 0;
                    }
                }

                bool mouse_pos_changed = 0;
                { // Calculating hovered tile
                    ivec2 mouse_pos_prev = mouse_pos;
                    mouse_pos = div_ex(mouse.pos() + cam.pos, tile_size);
                    if (mouse_pos != mouse_pos_prev)
                        mouse_pos_changed = 1;

                    map_selection_map_hovered = (mouse_pos >= 0).all() && (mouse_pos < map.Size()).all();
                }

                // Selecting map region
                if (grabbed_tiles.empty() && mouse.right.pressed() && map_selection_map_hovered)
                {
                    map_selection_button_down = 1;
                    map_selection_start = clamp(mouse_pos, 0, map.Size()-1);
                    map_selection_multiple_tiles = 0;
                }
                if (map_selection_button_down)
                {
                    map_selection_end = clamp(mouse_pos, 0, map.Size()-1);

                    if (!map_selection_multiple_tiles && map_selection_end != map_selection_start)
                        map_selection_multiple_tiles = 1;

                    if (mouse.right.released())
                    {
                        map_selection_button_down = 0;

                        ivec2 a = map_selection_start,
                              b = map_selection_end;
                        if (a.x > b.x) std::swap(a.x, b.x);
                        if (a.y > b.y) std::swap(a.y, b.y);

                        switch (current_mode)
                        {
                          case Mode::copy:
                            grabbed_tiles = {};
                            grabbed_tiles_size = b - a + 1;
                            if (!map_selection_multiple_tiles)
                            {
                                int id = map.GetTop(target_layer, map_selection_end);
                                if (id != -1)
                                    grabbed_tiles.push_back({id, ivec2(0)});
                            }
                            else
                            {
                                for (int y = a.y; y <= b.y; y++)
                                for (int x = a.x; x <= b.x; x++)
                                for (const auto &it : map.Get(target_layer, ivec2(x,y)))
                                    grabbed_tiles.push_back({it, ivec2(x,y)-a});
                            }
                            break;
                          case Mode::erase:
                            if (map_selection_multiple_tiles)
                            {
                                for (int y = a.y; y <= b.y; y++)
                                for (int x = a.x; x <= b.x; x++)
                                    map.RemoveTile(target_layer, ivec2(x,y), 1);
                            }
                            break;
                        }
                    }
                }

                // Moving and placing grabbed tiles
                if (current_mode == Mode::copy && grabbed_tiles.size())
                {
                    ivec2 base_offset_prev = grabbed_tiles_base_offset;
                    grabbed_tiles_base_offset = div_ex(-grabbed_tiles_size * tile_size/2 + mouse.pos() + cam.pos + tile_size/2, tile_size);

                    if (mouse.left.pressed() || (mouse.left.down() && grabbed_tiles_base_offset != base_offset_prev))
                        for (const auto &it : grabbed_tiles)
                            map.AddTile(target_layer, grabbed_tiles_base_offset + it.offset, it.id);

                    if (mouse.right.pressed())
                        grabbed_tiles = {};
                }

                // Erasing
                if ((current_mode == Mode::copy || current_mode == Mode::erase) && mouse.left.down() && grabbed_tiles.empty() &&
                    !map_selection_button_down && (mouse_pos_changed || mouse.left.pressed()) && map_selection_map_hovered)
                    map.RemoveTile(target_layer, mouse_pos);



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

            fvec3 mode_color = fvec3(0.5);
            switch (current_mode)
            {
              case Mode::copy:
                mode_color = fvec3(0,0.6,1);
                break;
              case Mode::erase:
                mode_color = fvec3(1,0.2,0.2);
                break;
              case Mode::select:
                mode_color = fvec3(0.75,1,0.5);
                break;
            }

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
                constexpr int width = 4;
                for (int i = 0; i <= 1; i++)
                    Draw::Rect(-cam.pos, ivec2(width * -i), map.Size() * tile_size + width, width, fvec3(i), 0.5);
            }

            // Map selection
            if ((map_selection_button_down || current_mode == Mode::select) && map_selection_multiple_tiles)
            {
                constexpr int width = 2;

                ivec2 a = map_selection_start,
                      b = map_selection_end;
                if (a.x > b.x) std::swap(a.x, b.x);
                if (a.y > b.y) std::swap(a.y, b.y);

                for (int i = 0; i <= 1; i++)
                    Draw::Rect(-cam.pos - ivec2(i * width), a * tile_size, (b - a + 1) * tile_size + width, width, i ? mode_color : fvec3(0), 1);
            }

            // Mouse cursor
            if (!selecting_tiles && grabbed_tiles.size() == 0 && (!map_selection_button_down || !map_selection_multiple_tiles))
                Draw::Rect(mouse_pos * tile_size - cam.pos, ivec2(0), ivec2(tile_size), 1, mode_color * t, 0.75);

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

                // Selection rectangle
                if (selecting_tiles_sheet_hovered || selecting_tiles_button_down)
                {
                    ivec2 pos = selecting_tiles_sheet_offset - map.TileSheet().Size() * tile_size/2;

                    ivec2 at, bt;
                    if (selecting_tiles_button_down)
                    {
                        at = selecting_tiles_pos_start,
                        bt = selecting_tiles_pos_end;

                        if (at.x > bt.x) std::swap(at.x, bt.x);
                        if (at.y > bt.y) std::swap(at.y, bt.y);
                    }
                    else
                        at = bt = selecting_tiles_pos_end;

                    constexpr int width = 2;

                    for (int i = 0; i <= 1; i++)
                        Draw::Rect(pos - ivec2(i*width), at * tile_size, (bt - at + 1) * tile_size + width, 2, fvec3(i), (selecting_tiles_button_down ? 1 : 0.5) * selecting_tiles_alpha);
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

                std::string bottom_right;
                if (show_help)
                {
                    if (selecting_tiles)
                        bottom_right = "WASD to move\nHold LMB to select\nTAB to close\nF5 to reload textures\n";
                    else
                        bottom_right = "WASD to move\n(hold SHIFT for slow speed)\n(hold CTRL for fast speed)\nTAB to open tile sheet\nRMB to select tiles\nLMB to draw or erase\nQ,E,R to change modes\nF5 to reload textures\n";
                    bottom_right += "F1 to hide this text";
                }
                else
                    bottom_right = "F1 to show help";

                std::string top_middle;
                if (!selecting_tiles)
                {
                    switch (current_mode)
                    {
                      case Mode::copy:
                        // Nothing
                        break;
                      case Mode::erase:
                        top_middle = "Erasing";
                        break;
                      case Mode::select:
                        top_middle = "Selecting";
                        break;
                    }
                }

                r.Text(-screen_sz/2 + 2          , top_left     ).preset(Draw::TinyWithBlackOutline).align({-1,-1});
                r.Text((screen_sz/2-2).mul_x(-1) , bottom_left  ).preset(Draw::TinyWithBlackOutline).align({-1,1});
                r.Text(screen_sz/2 - 2           , bottom_right ).preset(Draw::TinyWithBlackOutline).align({1,1});
                r.Text(ivec2(0,-screen_sz.y/2+20), top_middle   ).preset(Draw::TinyWithBlackOutline).align({0,0}).color(mode_color);
            }
        }
    };

    class TestObject
    {
        std::string str = "Hello, world!";
      public:
        void Tick(const Scene &scene)
        {
            "";
            if (Keys::space.pressed())
            {
                bool load = Keys::l_ctrl.down();
                int status;
                if (load)
                    status = scene.Get<Map>().LoadFromFile("out.txt");
                else
                    status = scene.Get<Map>().SaveToFile("out.txt");
                if (!status)
                    std::cout << "Unable to " << (load ? "load" : "save") << '\n';
            }
        }
        void Render(const Scene &scene)
        {
            (void)scene;
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
        s.Add<Background>();
        s.Add<Map>(tilesheet_main, ivec2(26,15));
        if (map_editor) s.Add<MapEditor>();

        s.Add<TestObject>();

        s.Get<Camera>().pos = s.Get<Map>().Size() * tile_size / 2;
        if (map_editor) s.Get<MapEditor>().Enable(s);

        s.SetTick([](const Scene &s)
        {
            s.Get<Background>().Tick();
            if (auto ptr = s.GetOpt<MapEditor>()) ptr->Tick(s);

            s.Get<TestObject>().Tick(s);
        });
        s.SetRender([](const Scene &s)
        {
            s.Get<Background>().Render();
            s.Get<Map>().Render(s, Map::back);
            s.Get<Map>().Render(s, Map::front);
            if (auto ptr = s.GetOpt<MapEditor>()) ptr->Render(s);

            s.Get<TestObject>().Render(s);
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
