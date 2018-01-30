#include "everything.h"

#include <bitset>
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
        void WithBlackOutline(Renderers::Poly2D::Text_t &obj) // Is a preset
        {
            obj.callback([](Renderers::Poly2D::Text_t::CallbackParams params)
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

        /* Colors:
         * /r - grey
         * /1 - white
         * /2 - green
         * /3 - red
         * /4 - gold
         * /5 - blue
         */
        [[nodiscard]] auto WithColors(const std::vector<fvec3> &colors = {fvec3(1), fvec3(0.25,1,0.5), fvec3(1,0.25,0.25), fvec3(1,5/6.,1/6.), fvec3(0,0.5,1)}) // Returns a preset
        {
            if (colors.size() > 7)
                Program::Error("Too many text colors.");
            return [=](Renderers::Poly2D::Text_t &obj)
            {
                obj.color(fvec3(0.6)).callback([=, color_index = 0](Renderers::Poly2D::Text_t::CallbackParams params) mutable
                {
                    if (params.ch == '\r' || (params.ch >= '\1' && params.ch <= '\7'))
                    {
                        if (params.render.size())
                            params.render.clear();
                        params.glyph.advance = 0;

                        if (params.ch == '\r')
                        {
                            color_index = 0;
                        }
                        else
                        {
                            color_index = params.ch - '\0';
                            if (color_index > int(colors.size()))
                                Program::Error("Text color index is out of range.");
                        }
                    }

                    if (params.render_pass && color_index != 0)
                    {
                        for (auto &it : params.render)
                            it.color = colors[color_index-1];
                    }
                });
            };
        }

        [[nodiscard]] auto WithCursor(int index, fvec3 color = fvec3(tick_stabilizer.ticks % 60 < 30)) // Returns a preset
        {
            return [=](Renderers::Poly2D::Text_t &obj)
            {
                obj.callback([=](Renderers::Poly2D::Text_t::CallbackParams params)
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
        Graphics::Image textureimage_main("assets/texture.png");
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
            constexpr fvec3 c1 = fvec3(0.1),
                            c2 = fvec3(0.2);
            r.Quad(-screen_sz/2, screen_sz).color(c1,c1,c2,c2);
        }
    };

    class Map
    {
      public:
        static constexpr uint32_t version_magic = 2; // This should be changed when map binary structure changes.

        static constexpr ivec2 sheet_size = ivec2(32), sheet_tex_pos = ivec2(0,512);


        using tile_id_t = short;

        ReflectStruct(Tile, (
            (tile_id_t)(front,mid,back)(=-1),
        ))


        using layer_mem_ptr_t = tile_id_t Tile::*;

        static constexpr layer_mem_ptr_t front = &Tile::front,
                                         mid   = &Tile::mid,
                                         back  = &Tile::back;

        static constexpr layer_mem_ptr_t layer_list[] {front, mid, back};
        static constexpr int layer_count = std::extent_v<decltype(layer_list)>;


        enum SafetyMode {Safe, Unsafe};


        class TilingSettings
        {
            struct Data
            {
                ReflectMemberEnum(LayerEnum, (front)(mid)(back))
                static_assert(layer_list[front] == Map::front &&
                              layer_list[mid  ] == Map::mid   &&
                              layer_list[back ] == Map::back    );

                struct Tile
                {
                    struct Rule
                    {
                        struct Requirement
                        {
                            Reflect(Requirement)
                            (
                                (std::string)(group)(="\1"),
                                (ivec2)(offset),
                            )
                        };

                        Reflect(Rule)
                        (
                            (std::string)(from, to),
                            (float)(chance)(=1),
                            (std::vector<Requirement>)(requires),
                        )
                    };

                    Reflect(Tile)
                    (
                        (std::string)(name),
                        (LayerEnum)(layer),
                        (std::vector<std::pair<std::string, ivec2>>)(variations),
                        (std::vector<Rule>)(rules),
                    )
                };

                Reflect(Data)
                (
                    (std::vector<std::pair<std::string, std::vector<std::string>>>)(groups),
                    (std::vector<Tile>)(tiles),
                )
            };
            Data data;

          public:
            TilingSettings(std::string file_name)
            {
                Utils::MemoryFile file(file_name);

                Reflection::ParsingErrorContext context;

                if (!Reflection::from_string(data, (char *)file.Data(), Reflection::overwrite, context))
                    Program::Error(Str("Unable to load tiling settings.\n", context.to_string()));

                std::sort(data.groups.begin(), data.groups.end(), [](auto &a, auto &b){return a.first < b.first;});
                for (auto &it : data.groups)
                    std::sort(it.second.begin(), it.second.end());

                std::sort(data.tiles.begin(), data.tiles.end(), [](auto &a, auto &b){return a.name < b.name;});
                for (auto &it : data.tiles)
                {
                    std::sort(it.variations.begin(), it.variations.end(), [](auto &a, auto &b){return a.first < b.first;});
                    std::sort(it.rules.begin(), it.rules.end(), [](auto &a, auto &b){return a.from < b.from;});
                }
            }
        };
        inline static TilingSettings tiling_settings{"assets/tiling"};


      private:
        std::string file_name;

        struct Data
        {
            Reflect(Data)
            (
                (ivec2)(size),
                (std::vector<Tile>)(tiles),
            )

            template <SafetyMode Mode = Safe> void Set(ivec2 pos, const Tile &tile)
            {
                if constexpr (Mode != Unsafe)
                    if ((pos < 0).any() || (pos >= size).any())
                        return;
                tiles[pos.x + size.x * pos.y] = tile;
            }
            template <SafetyMode Mode = Safe> void Set(ivec2 pos, layer_mem_ptr_t layer, tile_id_t id)
            {
                if constexpr (Mode != Unsafe)
                    if ((pos < 0).any() || (pos >= size).any())
                        return;
                tiles[pos.x + size.x * pos.y].*layer = id;
            }

            template <SafetyMode Mode = Safe> Tile Get(ivec2 pos) const
            {
                if constexpr (Mode != Unsafe)
                    clamp_assign(pos, 0, size-1);
                return tiles[pos.x + size.x * pos.y];
            }
            template <SafetyMode Mode = Safe> tile_id_t Get(ivec2 pos, layer_mem_ptr_t layer) const
            {
                if constexpr (Mode != Unsafe)
                    clamp_assign(pos, 0, size-1);
                return tiles[pos.x + size.x * pos.y].*layer;
            }
        };

        Data data;

      public:
        Map() {}
        Map(std::string file_name) : file_name(file_name)
        {
            // Trying to load the map from binary
            if (LoadFromFile())
                return;

            // Trying to load the map from forward-compatible format
            if (LoadFromFile(1))
            {
                SaveToFile();
                return;
            }

            data.size = ivec2(10);
            data.tiles.resize(data.size.product());
        }

        ivec2 Size() const
        {
            return data.size;
        }

        void Resize(ivec2 new_size, ivec2 offset)
        {
            if (new_size == data.size && offset == ivec2(0))
                return;
            if ((new_size < 1).any())
                return;

            Data new_data;
            new_data.size = new_size;
            new_data.tiles.resize(new_size.product());

            ivec2 a = max(ivec2(0), -offset), b = min(new_size - offset, data.size);

            for (int y = a.y; y < b.y; y++)
            for (int x = a.x; x < b.x; x++)
                new_data.Set<Unsafe>(ivec2(x,y) + offset, data.Get<Unsafe>(ivec2(x,y)));
            data = std::move(new_data);
        }

        void Set(ivec2 pos, const Tile &tile)
        {
            return data.Set(pos, tile);
        }
        void Set(ivec2 pos, layer_mem_ptr_t layer, tile_id_t id)
        {
            return data.Set(pos, layer, id);
        }

        Tile Get(ivec2 pos) const
        {
            return data.Get(pos);
        }
        tile_id_t Get(ivec2 pos, layer_mem_ptr_t layer) const
        {
            return data.Get(pos, layer);
        }

        void Render(const Scene &scene, layer_mem_ptr_t layer, bool transparent = 0) const
        {
            constexpr int period = 120;

            auto &cam = scene.Get<Camera>();

            ivec2 first = div_ex(cam.pos - screen_sz / 2, tile_size),
                  last  = div_ex(cam.pos + screen_sz / 2, tile_size);

            float t;
            if (transparent)
            {
                t = tick_stabilizer.ticks % period / float(period/2);
                t = (t < 1 ? smoothstep(t) : smoothstep(2-t));
                t *= 0.5;
            }

            for (int y = first.y; y <= last.y; y++)
            for (int x = first.x; x <= last.x; x++)
            {
                tile_id_t id = Get(ivec2(x,y), layer);
                if (id < 0)
                    continue;

                auto quad = r.Quad(ivec2(x,y) * tile_size - cam.pos, ivec2(tile_size)).tex(ivec2(id / sheet_size.x, id % sheet_size.y) * tile_size + sheet_tex_pos);
                if (transparent)
                    quad.alpha(t);
            }
        }

        const std::string &FileName() const
        {
            return file_name;
        }
        void SetFileName(std::string new_file_name)
        {
            file_name = new_file_name;
        }

        bool SaveToFile(bool forward_compat = 0) const
        {
            if (forward_compat)
            {
                std::string str = Reflection::to_string(data);
                return Utils::WriteToFile(file_name + ".fwdcompat", (uint8_t *)str.data(), str.size(), Utils::compressed);
            }
            else
            {
                auto len = sizeof(uint32_t) + Reflection::byte_buffer_size(data);
                auto buf = std::make_unique<uint8_t[]>(len);
                uint8_t *ptr = Reflection::to_bytes<uint32_t>(version_magic, buf.get());
                ptr = Reflection::to_bytes(data, ptr);
                if (ptr != buf.get() + len)
                    return 0;
                return Utils::WriteToFile(file_name, buf.get(), len, Utils::compressed);
            }
        }
        bool LoadFromFile(bool forward_compat = 0)
        {
            Utils::MemoryFile file;
            try
            {
                file.Create((forward_compat ? file_name + ".fwdcompat" : file_name), Utils::compressed);
            }
            catch(decltype(Utils::file_input_error("","")) &e)
            {
                return 0;
            }

            auto data_copy = data;
            bool ok;
            if (forward_compat)
            {
                ok = Reflection::from_string(data, (char *)file.Data()); // `MemoryFile::Data()` is null-terminated, so we're fine.
            }
            else
            {
                ok = 0;
                const uint8_t *begin = file.Data(), *end = file.Data() + file.Size();
                uint32_t magic;
                begin = Reflection::from_bytes<uint32_t>(magic, begin, end);
                if (begin && magic == version_magic)
                {
                    begin = Reflection::from_bytes(data, begin, end);
                    if (begin == end)
                        ok = 1;
                }
            }

            if (!ok)
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

        Map::layer_mem_ptr_t target_layer = Map::mid;

        enum class OtherLayersHandling {show, transparent, hide};
        OtherLayersHandling other_layers_handling = OtherLayersHandling::show;

        bool selecting_tiles = 0;
        float selecting_tiles_alpha = 0;

        ivec2 selecting_tiles_sheet_offset = ivec2(0),
              selecting_tiles_pos_start = ivec2(0),
              selecting_tiles_pos_end   = ivec2(0);
        bool selecting_tiles_sheet_hovered = 0,
             selecting_tiles_button_down = 0;


        class
        {
            ivec2 size;
            std::vector<Map::tile_id_t> tiles;
          public:
            void Grab(const Map &map, Map::layer_mem_ptr_t layer, ivec2 a, ivec2 b)
            {
                if (a.x > b.x) std::swap(a.x, b.x);
                if (a.y > b.y) std::swap(a.y, b.y);

                size = b - a + 1;
                tiles.resize(size.product());

                for (int y = 0; y < size.y; y++)
                for (int x = 0; x < size.x; x++)
                    tiles[x + y * size.x] = map.Get(ivec2(x,y) + a, layer);
            }
            void Grab(ivec2 a, ivec2 b)
            {
                if (a.x > b.x) std::swap(a.x, b.x);
                if (a.y > b.y) std::swap(a.y, b.y);

                size = b - a + 1;
                tiles.resize(size.product());

                for (int y = 0; y < size.y; y++)
                for (int x = 0; x < size.x; x++)
                    tiles[x + y * size.x] = (ivec2(x,y) + a).mul_x(Map::sheet_size.y).sum();
            }
            void Release()
            {
                size = ivec2(0);
                tiles = {};
            }
            bool Grabbed() const
            {
                return tiles.size() != 0;
            }
            ivec2 Size() const
            {
                return size;
            }

            void Paste(Map &map, Map::layer_mem_ptr_t layer, ivec2 offset) const
            {
                for (int y = 0; y < size.y; y++)
                for (int x = 0; x < size.x; x++)
                    map.Set(ivec2(x,y) + offset, layer, tiles[x + size.x * y]);
            }

            void Render(ivec2 cam_pos)
            {
                constexpr int period = 60, air_margin = 1;
                constexpr float highlight = 1/3., alpha = 3/4., air_alpha = 1/4.;

                ivec2 cam_a = div_ex(cam_pos - screen_sz/2, tile_size),
                      cam_b = div_ex(cam_pos + screen_sz/2, tile_size);

                ivec2 render_offset = max(cam_a - base_offset, 0),
                      render_size   = min(cam_b - cam_a + 1, size);

                float t = tick_stabilizer.ticks % period / float(period/2);
                t = (t < 1 ? smoothstep(t) : smoothstep(2-t));

                for (int y = render_offset.y; y < render_size.y; y++)
                for (int x = render_offset.x; x < render_size.x; x++)
                {
                    Map::tile_id_t id = tiles[x + size.x * y];
                    if (id < 0)
                        r.Quad((ivec2(x,y) + base_offset) * tile_size - cam_pos + air_margin, ivec2(tile_size-air_margin*2)).color(fvec3(t)).alpha(air_alpha);
                    else
                        r.Quad((ivec2(x,y) + base_offset) * tile_size - cam_pos, ivec2(tile_size)).tex(ivec2(id / Map::sheet_size.x, id % Map::sheet_size.y) * tile_size + Map::sheet_tex_pos).color(fvec3(t)).mix(1-highlight).alpha(alpha);
                }
            }

            ivec2 base_offset = ivec2(0);
        }
        grab;


        ivec2 mouse_pos = ivec2(0);

        ivec2 map_selection_start = ivec2(0),
              map_selection_end = ivec2(0);
        bool map_selection_button_down = 0,
             map_selection_map_hovered = 0,
             map_selection_multiple_tiles = 0;

        bool eraser_mode = 0;

        bool resize_started = 0;
        ivec2 resize_type = ivec2(0);
        std::string resize_string;

        bool show_help = 1;

        std::string message_text;
        float message_alpha = 0;

        void ShowMessage(std::string text)
        {
            message_text = text;
            message_alpha = 1;
        }

        void SaveMap(const Scene &scene, bool forward_compat = 0)
        {
            auto &map = scene.Get<Map>();
            bool ok = map.SaveToFile(forward_compat);
            if (ok)
                ShowMessage(Str("\2Map \1", map.FileName(), "\2 was successfully saved", (forward_compat ? " \4(compatibility mode)" : "")));
            else
                ShowMessage(Str("\3Map \1", map.FileName(), "\3 couldn't be saved", (forward_compat ? " \4(compatibility mode)" : "")));
        }
        void LoadMap(const Scene &scene, bool forward_compat = 0)
        {
            auto &map = scene.Get<Map>();
            bool ok = map.LoadFromFile(forward_compat);
            if (ok)
                ShowMessage(Str("\2Map \1", map.FileName(), "\2 was successfully loaded", (forward_compat ? " \4(compatibility mode)" : "")));
            else
                ShowMessage(Str("\3Map \1", map.FileName(), "\3 couldn't be loaded", (forward_compat ? " \4(compatibility mode)" : "")));
        }

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
            auto &map = scene.Get<Map>();
            auto &cam = scene.Get<Camera>();

            { // Save on exit
                if (Events::ExitRequested())
                    SaveMap(scene);
            }

            { // Timers
                if (message_alpha > 0)
                {
                    message_alpha -= 0.01;
                    if (message_alpha < 0)
                        message_alpha = 0;
                }
            }

            if (resize_started) // Actually resizing
            {
                Input::Text(&resize_string, 5, "-0123456789");
                if (Keys::enter.pressed())
                {
                    resize_started = 0;
                    int value = 0;
                    Reflection::Interface::primitive_from_string(value, resize_string.c_str());
                    resize_string = "";

                    ivec2 new_size = map.Size() + abs(resize_type) * value;
                    ivec2 offset = abs((resize_type < 0).to<int>()) * value;
                    map.Resize(new_size, offset);
                    editor_cam_pos += offset * tile_size;
                }
                else
                    return;
            }

            { // Show/hide GUI
                if (Keys::grave.pressed())
                {
                    Enable(scene, !enabled);
                    if (!enabled)
                        SaveMap(scene);
                }
                if (!enabled)
                    return;
            }

            { // Reload textures if needed
                if (Keys::f5.pressed())
                    Draw::ReloadTextures();
            }

            if (enabled)
            {
                { // Saving/loading the map
                    if (Keys::space.pressed())
                        SaveMap(scene, Keys::l_alt.down());
                    if (Keys::l_ctrl.down() && Keys::f5.pressed())
                        LoadMap(scene, Keys::l_alt.down());
                }

                { // Open/close tile sheet
                    if (!selecting_tiles && Keys::tab.pressed() && !map_selection_button_down)
                    {
                        selecting_tiles = 1;
                        eraser_mode = 0;
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
                { // WASD modifiers
                    if (!selecting_tiles)
                    {
                        int speed = 6;
                        if (Keys::l_alt.down())
                            speed = 100;
                        if (Keys::l_ctrl.down())
                            speed = 20;
                        if (Keys::l_shift.down())
                            speed = 1;
                        wasd *= speed;
                    }
                }

                { // Camera
                    clamp_assign(editor_cam_pos += wasd, 0, map.Size() * tile_size);

                    cam.pos = editor_cam_pos;
                }

                // Tile sheet
                if (selecting_tiles)
                {
                    { // Tile sheet camera panning
                        ivec2 sheet_half_extent = Map::sheet_size * tile_size/2;
                        clamp_assign(selecting_tiles_sheet_offset -= wasd * 2, -sheet_half_extent, sheet_half_extent);
                    }

                    ivec2 cur_pos = div_ex(mouse.pos() - selecting_tiles_sheet_offset + Map::sheet_size * tile_size/2, tile_size);
                    selecting_tiles_sheet_hovered = (cur_pos >= 0).all() && (cur_pos < Map::sheet_size).all();
                    cur_pos = clamp(cur_pos, 0, Map::sheet_size-1);

                    selecting_tiles_pos_end = cur_pos;
                    if (mouse.left.pressed() && selecting_tiles_sheet_hovered)
                    {
                        selecting_tiles_button_down = 1;
                        selecting_tiles_pos_start = cur_pos;
                    }
                    if (selecting_tiles_button_down && mouse.left.released())
                    {
                        selecting_tiles_button_down = 0;
                        grab.Grab(selecting_tiles_pos_start, selecting_tiles_pos_end);
                        selecting_tiles = 0;
                    }
                }

                // Editing the map
                if (!selecting_tiles) // Not `else`.
                {
                    { // Switching modes
                        if (Keys::e.pressed())
                        {
                            eraser_mode = !eraser_mode;
                            grab.Release();
                        }
                    }

                    { // Switching layers
                        if (Keys::_1.pressed())
                            target_layer = Map::front;
                        if (Keys::_2.pressed())
                            target_layer = Map::mid;
                        if (Keys::_3.pressed())
                            target_layer = Map::back;
                    }

                    { // Switching layer transparency
                        if (Keys::z.pressed())
                            other_layers_handling = OtherLayersHandling::hide;
                        if (Keys::x.pressed())
                            other_layers_handling = OtherLayersHandling::transparent;
                        if (Keys::c.pressed())
                            other_layers_handling = OtherLayersHandling::show;
                    }

                    { // Starting resize
                        if (Keys::l_alt.down())
                        {
                            if (Keys::left.pressed())
                            {
                                resize_started = 1;
                                resize_type = ivec2(-1,0);
                            }
                            if (Keys::right.pressed())
                            {
                                resize_started = 1;
                                resize_type = ivec2(1,0);
                            }
                            if (Keys::up.pressed())
                            {
                                resize_started = 1;
                                resize_type = ivec2(0,-1);
                            }
                            if (Keys::down.pressed())
                            {
                                resize_started = 1;
                                resize_type = ivec2(0,1);
                            }
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
                    if (!grab.Grabbed() && mouse.right.pressed() && map_selection_map_hovered)
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

                            if (!eraser_mode)
                            {
                                grab.Grab(map, target_layer, a, b);
                            }
                            else
                            {
                                if (map_selection_multiple_tiles)
                                {
                                    if (a.x > b.x) std::swap(a.x, b.x);
                                    if (a.y > b.y) std::swap(a.y, b.y);

                                    for (int y = a.y; y <= b.y; y++)
                                    for (int x = a.x; x <= b.x; x++)
                                        map.Set(ivec2(x,y), target_layer, -1);
                                }
                            }
                        }
                    }

                    // Moving and placing grabbed tiles
                    if (!eraser_mode && grab.Grabbed())
                    {
                        ivec2 base_offset_prev = grab.base_offset;
                        grab.base_offset = div_ex(-grab.Size() * tile_size/2 + mouse.pos() + cam.pos + tile_size/2, tile_size);

                        if (mouse.left.pressed() || (mouse.left.down() && grab.base_offset != base_offset_prev))
                            grab.Paste(map, target_layer, grab.base_offset);

                        if (mouse.right.pressed())
                            grab.Release();
                    }

                    // Erasing
                    if (mouse.left.down() && !grab.Grabbed() && !map_selection_button_down && (mouse_pos_changed || mouse.left.pressed()) && map_selection_map_hovered)
                        map.Set(mouse_pos, target_layer, -1);
                }
            }
        }
        void Render(const Scene &scene)
        {
            auto &map = scene.Get<Map>();
            auto &cam = scene.Get<Camera>();

            if (enabled)
            {
                constexpr int period = 60;

                float t = tick_stabilizer.ticks % period / float(period/2);
                t = (t < 1 ? smoothstep(t) : smoothstep(2-t));

                fvec3 mode_color = fvec3(0.5);
                if (!eraser_mode)
                    mode_color = fvec3(0,0.6,1);
                else
                    mode_color = fvec3(1,0.2,0.2);

                // Grabbed tiles
                if (!selecting_tiles && grab.Grabbed())
                    grab.Render(cam.pos);

                { // Map border
                    constexpr int width = 4;
                    for (int i = 0; i <= 1; i++)
                        Draw::Rect(-cam.pos, ivec2(width * -i), map.Size() * tile_size + width, width, fvec3(i), 0.5);
                }


                // Map selection
                bool show_selection_rect = map_selection_button_down && (map_selection_multiple_tiles || eraser_mode);
                if (show_selection_rect)
                {
                    constexpr int width = 2;

                    ivec2 a = map_selection_start,
                          b = map_selection_end;
                    if (a.x > b.x) std::swap(a.x, b.x);
                    if (a.y > b.y) std::swap(a.y, b.y);

                    Draw::Rect(-cam.pos, a * tile_size, (b - a + 1) * tile_size, width*3, fvec3(1));

                    for (int i = 0; i <= 1; i++)
                        Draw::Rect(-cam.pos - ivec2(i * width), a * tile_size, (b - a + 1) * tile_size + width, width, i ? mode_color : fvec3(0));
                }

                // Mouse cursor
                if (!selecting_tiles && !grab.Grabbed() && !show_selection_rect)
                {
                    constexpr int cursor_width = 4, cursor_length = 4, cursor_corner_size = 4, cursor_outline_size = 2;

                    Draw::Rect(mouse_pos * tile_size - cam.pos, ivec2(0), ivec2(16), cursor_outline_size, fvec3(1));

                    int i = 0;
                    for (const auto &m : {fmat2(1,0,0,1),fmat2(0,-1,1,0),fmat2(-1,0,0,-1),fmat2(0,1,-1,0)})
                    {
                        r.Quad(mouse_pos * tile_size + tile_size/2 - cam.pos, ivec2(1, tile_size)).center(ivec2(1+tile_size/2,tile_size/2)).color(mode_color * t).matrix(m);
                        r.Quad(mouse_pos * tile_size + tile_size/2 - cam.pos, ivec2(cursor_length, cursor_width)).center(ivec2(cursor_length+tile_size/2, cursor_width/2)).color(mode_color * t).matrix(m);
                        r.Quad(mouse_pos * tile_size + tile_size/2 - cam.pos, ivec2(cursor_corner_size)).center(ivec2(cursor_corner_size+tile_size/2)).color(mode_color * (1-t)).matrix(m);
                        i++;
                    }
                }

                // Tile selector
                if (selecting_tiles_alpha)
                {
                    // Background
                    ivec2 half_extent_tiles = (screen_sz / 2 + tile_size - 1) / tile_size;
                    for (int y = -half_extent_tiles.y; y < half_extent_tiles.y; y++)
                    for (int x = -half_extent_tiles.x; x < half_extent_tiles.x; x++)
                        r.Quad(ivec2(x,y) * tile_size, ivec2(tile_size)).tex({0,0}).alpha(selecting_tiles_alpha);

                    // Tile sheet
                    r.Quad(selecting_tiles_sheet_offset, Map::sheet_size * tile_size).tex(Map::sheet_tex_pos).alpha(selecting_tiles_alpha).center();

                    // Selection rectangle
                    if (selecting_tiles_sheet_hovered || selecting_tiles_button_down)
                    {
                        ivec2 pos = selecting_tiles_sheet_offset - Map::sheet_size * tile_size/2;

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
                    // Top left
                    std::string top_left;
                    if (!selecting_tiles)
                        top_left = "Editing: tiles";
                    else
                        top_left = "Tile sheet";

                    // Bottom left
                    ivec2 cam_pos = div_ex(cam.pos, tile_size),
                          pos = (grab.Grabbed() ? grab.base_offset : mouse_pos);
                    std::string bottom_left = Str("Map size: [", std::setw(5), map.Size().x, ",", std::setw(5), map.Size().y, "]",
                                                  "     Camera center: [", std::setw(5), cam_pos.x, ",", std::setw(5), cam_pos.y, "]",
                                                  "          Pos: [", std::setw(5), pos.x, ",", std::setw(5), pos.y, "]");
                    ivec2 size(0);
                    if (grab.Grabbed())
                        size = grab.Size();
                    else if (map_selection_button_down && map_selection_multiple_tiles)
                        size = abs(map_selection_end - map_selection_start) + 1;
                    if (size != ivec2(0))
                        bottom_left += Str("          Size: [", std::setw(3), size.x, ",", std::setw(3), size.y, "]");

                    // Bottom right
                    std::string bottom_right;
                    if (show_help)
                    {
                        if (selecting_tiles)
                            bottom_right = "WASD to move\n"
                                           "Hold LMB to select\n"
                                           "TAB to close\n";
                        else
                            bottom_right = "WASD to move\n"
                                           "(+SHIFT - slow, +CTRL - fast, +ALT - faster)\n"
                                           "TAB to open tile sheet\n"
                                           "LMB to draw or erase\n"
                                           "RMB to select tiles\n"
                                           "E to switch to eraser mode\n"
                                           "1,2,3 to change layer\n"
                                           "Z,X,C to change visiblity of other layers\n"
                                           "ALT+<^>v to resize map\n";

                        bottom_right += "F5 to reload textures\n"
                                        "SPACE to save\n"
                                        "CTRL+F5 to reload\n"
                                        "(+ALT to save/load in forward-compatible mode)\n"
                                        "F1 to hide this text";
                    }
                    else
                        bottom_right = "F1 to show help";

                    // Top middle
                    std::string top_middle;
                    if (!selecting_tiles && eraser_mode)
                        top_middle = "Erasing";

                    // Top right
                    std::string top_right;
                    if (!selecting_tiles)
                        top_right = Str("\r\4"[target_layer == Map::front], "Front\n",
                                        "\r\4"[target_layer == Map::mid  ], "Middle\n",
                                        "\r\4"[target_layer == Map::back ], "Back\n");

                    // Top right 2
                    std::string top_right_2;
                    if (!selecting_tiles)
                        top_right_2 = Str("\1Other layers:\n\n",
                                          "\r\5"[other_layers_handling == OtherLayersHandling::hide       ], "Hidden\n",
                                          "\r\5"[other_layers_handling == OtherLayersHandling::transparent], "Transparent\n",
                                          "\r\5"[other_layers_handling == OtherLayersHandling::show       ], "Shown\n");

                    // Render
                    r.Text(-screen_sz/2 + 2                   , top_left     ).preset(Draw::WithBlackOutline).font(font_tiny).align({-1,-1});
                    r.Text((screen_sz/2-2).mul_x(-1)          , bottom_left  ).preset(Draw::WithBlackOutline).font(font_tiny).align({-1,1});
                    r.Text(screen_sz/2 - 2                    , bottom_right ).preset(Draw::WithBlackOutline).font(font_tiny).align({1,1});
                    r.Text(ivec2(0,-screen_sz.y/2+20)         , top_middle   ).preset(Draw::WithBlackOutline).align({0,0}).color(mode_color);
                    r.Text((screen_sz/2-2).mul_y(-1)          , top_right    ).preset(Draw::WithColors()).preset(Draw::WithBlackOutline).align({1,-1});
                    r.Text((screen_sz/2-2).mul_y(-1).add_y(72), top_right_2  ).preset(Draw::WithColors()).preset(Draw::WithBlackOutline).font(font_tiny).align({1,-1});
                }
            }

            { // Map resizing gui
                if (resize_started)
                {
                    r.Quad(ivec2(0), ivec2(screen_sz.x, 60)).color(fvec3(0)).center();
                    std::string msg = "Extent \1";
                         if (resize_type == ivec2(-1,0)) msg += "left";
                    else if (resize_type == ivec2( 1,0)) msg += "right";
                    else if (resize_type == ivec2(0,-1)) msg += "top";
                    else/*(resize_type == ivec2(0, 1))*/ msg += "bottom";
                    msg += "\r border by:\n\n";
                    r.Text(ivec2(0), msg).preset(Draw::WithColors());
                    r.Text(ivec2(0), '\4'+resize_string).preset(Draw::WithColors()).preset(Draw::WithCursor(1+Input::TextCursorPos()));
                    int value = 0;
                    Reflection::Interface::primitive_from_string(value, resize_string.c_str());
                    ivec2 new_size = map.Size() + abs(resize_type)*value;
                    bool invalid_size = (new_size < 1).any();
                    r.Text(ivec2(0), Str("\n\nNew map size: \1", (!invalid_size ? Str(new_size) : "???"))).preset(Draw::WithColors());
                }
            }

            { // Messages
                if (message_alpha > 0)
                {
                    float a = min(1, 10 * min(message_alpha, 1 - message_alpha));
                    r.Quad(ivec2(0,-screen_sz.y/3), ivec2(screen_sz.x,30)).color(fvec3(0)).alpha(a).center();
                    r.Text(ivec2(0,-screen_sz.y/3), message_text).color(fvec3(0.5)).preset(Draw::WithColors()).alpha(a);
                }
            }
        }

        bool ShouldShowLayer(Map::layer_mem_ptr_t layer)
        {
            return other_layers_handling != OtherLayersHandling::hide || layer == target_layer;
        }
        bool ShouldMakeLayerTransparent(Map::layer_mem_ptr_t layer)
        {
            return other_layers_handling == OtherLayersHandling::transparent && layer != target_layer;
        }
    };

    class MapRenderer
    {
      public:
        void Render(const Scene &scene, Map::layer_mem_ptr_t layer)
        {
            auto &map = scene.Get<Map>();
            bool visible = 1, transparent = 0;
            if (auto map_ed = scene.GetOpt<MapEditor>())
            {
                visible     = map_ed->ShouldShowLayer(layer);
                transparent = map_ed->ShouldMakeLayerTransparent(layer);
            }

            if (!visible)
                return;
            map.Render(scene, layer, transparent);
        }
    };

    class TestObject
    {
        std::string str = "Hello, world!";
      public:
        void Tick(const Scene &scene)
        {
            (void)scene;
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
        s.Add<Map>("test.map");
        if (map_editor) s.Add<MapEditor>();
        s.Add<MapRenderer>();

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
            s.Get<MapRenderer>().Render(s, Map::back);
            s.Get<MapRenderer>().Render(s, Map::mid);
            s.Get<MapRenderer>().Render(s, Map::front);
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
