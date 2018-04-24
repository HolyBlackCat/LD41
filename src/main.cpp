#include "everything.h"
#include "map.h"

#include <bitset>
#include <deque>
#include <iostream>
#include <list>
#include <map>
#include <numeric>
#include <set>
#include <unordered_map>

constexpr ivec2 screen_sz = ivec2(1920,1080)/4;

Events::AutoErrorHandlers error_handlers;

Window win("LD41 - g3ms", screen_sz * 2, Window::Settings{}.MinSize(screen_sz).Resizable());
Audio::Context audio;

Timing::TickStabilizer tick_stabilizer(60);

Graphics::Texture texture_main(Graphics::Texture::nearest),
                  texture_fbuf_main(Graphics::Texture::nearest, screen_sz), texture_fbuf_scaled(Graphics::Texture::linear);
Graphics::FrameBuffer framebuffer_main = nullptr, framebuffer_scaled = nullptr;

Graphics::Font font_object_main;
Graphics::CharMap font_main;

Renderers::Poly2D r;

Input::Mouse mouse;

namespace Sounds
{
    #define SOUND_LIST \
        SOUND( jump               , 0.2  ) \
        SOUND( land               , 0.0  ) \
        SOUND( get_gun            , 0.0  ) \
        SOUND( death              , 0.2  ) \
        SOUND( respawn            , 0.2  ) \
        SOUND( door               , 0.2  ) \
        SOUND( pew                , 0.4  ) \
        SOUND( bullet_hits        , 0.4  ) \
        SOUND( bullet_deflected   , 0.2  ) \
        SOUND( block_breaks       , 0.4  ) \
        SOUND( gem_moves          , 0.2  ) \
        SOUND( gem_stops          , 0.2  ) \
        SOUND( gem_breaks         , 0.2  ) \
        SOUND( turret_starts      , 0.4  ) \
        SOUND( turret_shoots      , 0.4  ) \
        SOUND( turret_bullet_hits , 0.4  ) \
        SOUND( enemy_hurt         , 0.3  ) \
        SOUND( enemy_dies         , 0.3  ) \
        SOUND( cube_hits_wall     , 0.3  ) \
        SOUND( end                , 0.0  ) \

    namespace Buffers
    {
        #define SOUND(NAME, RAND) \
            Audio::Buffer NAME = Audio::Sound::WAV("assets/" #NAME ".wav");
        SOUND_LIST
        #undef SOUND
    }

    #define SOUND(NAME, RAND) \
        auto NAME(fvec2 pos, float vol = 1, float pitch = 0) \
        { \
            return Buffers::NAME(vol, std::pow(2, pitch + random_real_range(-1,1) * RAND)).pos(pos.to_vec3()); \
        }
    SOUND_LIST
    #undef SOUND

    #undef SOUND_LIST

    Audio::Buffer theme_buf;
    Audio::Source theme;
    constexpr float theme_vol = 1/9.;
    bool theme_enabled = 1;

    void Init()
    {
        static bool once = 1;
        if (once) once = 0;
        else Program::Error("Sounds::Init() was called twice.");

        Audio::Source::DefaultRefDistance(200);
        Audio::Source::DefaultRolloffFactor(1);
        Audio::Volume(3);

        theme_buf.Create();
        theme_buf.SetData(Audio::Sound::OGG("assets/theme.ogg"));
        theme.Create(theme_buf);
        theme.loop(1).volume(theme_vol).play();
    }
}

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
                    float saved_beta  = copy.beta;
                    copy.color = fvec3(0);
                    copy.alpha *= 0.6;
                    copy.beta = 1;
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
                    copy.beta  = saved_beta;
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
        });
        /*
        font_main.EnableLineGap(0);
        font_tiny.EnableLineGap(0);
        */

        texture_main.SetData(textureimage_main);
        texture_main.Wrap(Graphics::Texture::clamp);
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

        ReloadTextures();

        r.Create(0x10000);
        r.SetTexture(texture_main);
        r.SetMatrix(fmat4::ortho2D(screen_sz / ivec2(-2,2), screen_sz / ivec2(2,-2)));
        r.SetDefaultFont(font_main);

        Resize();

        framebuffer_main  .Attach(texture_fbuf_main);
        framebuffer_scaled.Attach(texture_fbuf_scaled);

        mouse.Show(0);
    }

    void FullscreenQuad()
    {
        using Shaders::Identity::Att;
        Att array[] {{{-2,-2}},{{-2,10}},{{10,-2}}};
        static Graphics::VertexBuffer<Att> buf(std::extent_v<decltype(array)>, array);
        buf.Draw(Graphics::triangles);
    }
}


class Hitbox
{
    std::vector<ivec2> points;

  public:
    Hitbox() {}
    Hitbox(ivec2 size, ivec2 offset = ivec2(0))
    {
        if (size % 2 != ivec2{0,0} || (size < 1).any())
            Program::Error("Hitbox size must be positive and a multiple of two.");

        ivec2 half_extent = size / 2;

        constexpr int step = 4;

        for (int y = -half_extent.y; y < half_extent.y + Map::tile_size - 1; y += step)
        for (int x = -half_extent.x; x < half_extent.x + Map::tile_size - 1; x += step)
            points.push_back(min(ivec2(x,y), half_extent-1) + offset);
    }

    static Hitbox Line(ivec2 from, ivec2 to, float step)
    {
        Hitbox ret;
        float len = (from - to).len();
        ret.points.push_back(to);
        for (float i = 0; i < len; i += step)
            ret.points.push_back(from + (to - from) * i / len);
        return ret;
    }

    bool Hits(const Map &map, ivec2 pos, int flag = Map::flag_solid) const
    {
        for (const auto &point : points)
            if (map.TileWithFlagExistsAt(flag, div_ex(pos + point, Map::tile_size)))
                return 1;
        return 0;
    }

    template <typename T> bool HitsContainer(const T &cont, ivec2 pos)
    {
        for (const auto &point : points)
            if (cont.find(div_ex(pos + point, Map::tile_size)) != cont.end())
                return 1;
        return 0;
    }

    const auto &Points() const
    {
        return points;
    }
};

namespace Hitboxes
{
    const Hitbox player(ivec2(12,28), ivec2(0,2)),
                 player_small(ivec2(12,20), ivec2(0,2)),
                 enemy_cube(ivec2(30,30));
}


namespace Maps
{
    Map array[]
    {
        Map("assets/1_tutorial.map"),
        Map("assets/2_gun.map"),
        Map("assets/3_gems.map"),
        Map("assets/4_gem_puzzle.map"),
        Map("assets/5_enemies.map"),
        Map("assets/6_another_puzzle.map"),
        Map("assets/7_cubes.map"),
        Map("assets/8_woo.map"),
        Map("assets/9_and_another_puzzle.map"),
        Map("assets/10_lasers.map"),
        Map("assets/11_a_puzzle.map"),
        Map("assets/12_smoke_intro.map"),
        Map("assets/13_smoke.map"),
        Map("assets/14_relax.map"),
        Map("assets/15_its_something.map"),
        Map("assets/final.map"),
    };

    constexpr int map_count = std::extent_v<decltype(array)>;
}


struct Player
{
    ivec2 spawn_pos;
    ivec2 pos;
    fvec2 vel = ivec2(0), vel_lag = ivec2(0);
    int hc = 0;
    bool ground = 0, prev_ground = 0;
    bool jumps = 0;
    bool jump_in_progress = 0;
    bool left = 0;
    int anim_state = 0;
    int anim_frame = 0;
    int ticks_since_anim_state_change = 0;
    bool has_gun = 0;
    int aim = 0; // -2 = 90 deg up, -1 = 45 deg up, 0 = horizontal, 1 = 45 deg down, 2 = 90 deg down
    bool no_movement = 0;
    int gun_cooldown = 0;
    ivec2 gun_offset = ivec2(0);
    ivec2 gem_dir = ivec2(0);
    ivec2 nearest_selected_gem = ivec2(0);

    bool respawning = 1;
    bool dead = 0;
    bool prev_dead = 0;
    int death_timer = 0;
    bool can_respawn = 0;
    bool changing_level = 0;
    int level_change_timer = 0;
};

struct Camera
{
    fvec2 real_pos = ivec2(0), vel = fvec2(0);
    ivec2 pos = ivec2(0);
};

struct Objects
{
    bool gun_exists = 0;
    ivec2 gun_pos;

    ivec2 help_move_pos;
    ivec2 help_jump_pos;
    ivec2 help_gun_pos;
    ivec2 help_restart_pos;
    ivec2 help_gems_pos;
    ivec2 help_gems_row_pos;
    ivec2 help_dmg_pos;
};

struct Bullet
{
    fvec2 pos;
    fvec2 vel;
    int age = 0;
    bool remove = 0;
    bool deflected = 0;
};

struct EnemyBullet
{
    fvec2 pos;
    fvec2 vel;
    int age = 0;
};

struct Gem
{
    int index;
    ivec2 pos, dir;
};

struct Turret
{
    int ticks_since_player_visible = 0;
    float angle = f_pi/2;
    bool barrel = 0;
    int hp = 3;
    bool dead = 0;
};

struct EnemyCube
{
    ivec2 pos = ivec2(0);
    ivec2 dir = ivec2(0);
    int hp = 3;
    bool dead = 0;

    EnemyCube(ivec2 pos, ivec2 dir) : pos(pos), dir(dir) {}
};

struct SmokeSource
{
    int hp = 3;
    bool dead = 0;
};

struct LaserTurret
{
    float angle = 0;
    int dir = 0;
    int hp = 3;
    bool dead = 0;
    float len = 0, len_back = 0;

    LaserTurret() {}
    LaserTurret(int dir) : dir(dir) {}
};

struct Particle
{
    fvec3 color;
    float alpha, beta;
    fvec2 pos, vel, acc;
    float angle, av;
    float size;
    int cur_frames, max_frames;
    float size_power;
};

struct World
{
    static constexpr ivec2 camera_offset = ivec2(0,-32);

    Player p;
    Camera cam;
    Objects obj;
    Map map;

    float darkness = 1;

    std::string message;

    std::vector<Bullet> bullets;
    std::vector<EnemyBullet> enemy_bullets;
    std::unordered_map<ivec2, int> static_gems;
    std::unordered_map<ivec2, int> gem_walls;
    std::unordered_set<ivec2> selected_static_gems;
    std::vector<Gem> moving_gems;
    std::unordered_set<ivec2> bullet_barriers;
    std::deque<Particle> particles, particles_front;
    std::unordered_map<ivec2, Turret> turrets;
    std::unordered_map<ivec2, LaserTurret> laser_turrets;
    std::vector<EnemyCube> enemy_cubes;
    std::unordered_map<ivec2, SmokeSource> smoke_sources;
    int gem_count[4]{};

    bool game_start = 0;
    int game_start_frames = 0;
    bool game_can_begin = 0;

    bool finale = 0;
    ivec2 goal_pos = ivec2(0);

    bool ending_scene = 0;
    int ending_scene_frames = 0;
    float ending_alpha = 1;

    void AddParticle(bool front, fvec3 color, float alpha, float beta, fvec2 pos, fvec2 vel, fvec2 acc, float av, float size, int cur_frames, int max_frames, float size_power = 1, bool push_front = 0)
    {
        Particle p;
        p.color = color;
        p.alpha = alpha;
        p.beta = beta;
        p.pos = pos;
        p.vel = vel;
        p.acc = acc;
        p.angle = 0;
        p.av = av;
        p.size = size;
        p.cur_frames = cur_frames;
        p.max_frames = max_frames;
        p.size_power = size_power;

        auto &container = (front ? particles_front : particles);
        if (!push_front)
            container.push_back(p);
        else
            container.insert(container.begin(), p);
    }

    void AddPlayerBullet(fvec2 pos, fvec2 vel)
    {
        Bullet b;
        b.pos = pos;
        b.vel = vel;
        bullets.push_back(b);
    }
    void AddEnemyBullet(fvec2 pos, fvec2 vel)
    {
        EnemyBullet b;
        b.pos = pos;
        b.vel = vel;
        enemy_bullets.push_back(b);
    }

    void EnemyShield(fvec2 pos, float rad, bool no_particles = 0)
    {
        // Effects
        if (!no_particles && (abs(pos - cam.pos) < screen_sz + rad + 16).all())
        {
            for (int i = 0; i < rad * 1.8; i++)
                AddParticle(0, fvec3(random_real_range(0.6,1)), random_real_range(0.1,0.5), random_real_range(0.25,0.75), pos + fmat2::rotate2D(random_real_range(f_pi)) /mul/ fvec2(rad + random_real_range(1.5), 0),
                            fmat2::rotate2D(random_real_range(f_pi)) /mul/ fvec2(random_real(0.07),0), fvec2(0), random_real_range(0.3), 3, random_int(3), 5, 0.5);
        }

        // Remove bullets
        auto it = bullets.begin();
        while (it != bullets.end())
        {
            if ((it->pos - pos).len_sqr() < ipow(rad,2) && !it->remove)
            {
                it->remove = 1;
                it->deflected = 1;
                continue;
            }
            it++;
        }
    }

    void SetMap(const Map &m)
    {
        bool had_gun = p.has_gun;

        *this = {};

        p.has_gun = had_gun;

        map = m;

        p.spawn_pos = map.FindSingleTile("player spawn") * Map::tile_size + Map::tile_size/2 + ivec2(0,-Map::tile_size);
        p.pos = p.spawn_pos;

        cam.real_pos = cam.pos = p.pos + camera_offset;

        obj.gun_pos = map.FindSingleTileOpt("gun spawn");
        obj.gun_exists = (obj.gun_pos != ivec2(-1));

        obj.help_move_pos = map.FindSingleTileOpt("help move");
        obj.help_jump_pos = map.FindSingleTileOpt("help jump");
        obj.help_gun_pos = map.FindSingleTileOpt("help gun");
        obj.help_restart_pos = map.FindSingleTileOpt("help restart");
        obj.help_gems_pos = map.FindSingleTileOpt("help gems");
        obj.help_gems_row_pos = map.FindSingleTileOpt("help gems row");
        obj.help_dmg_pos = map.FindSingleTileOpt("help damage");

        auto gems_orange = map.FindTiles("gem orange"),
             gems_magenta = map.FindTiles("gem magenta"),
             gems_blue = map.FindTiles("gem blue"),
             gems_green = map.FindTiles("gem green");

        int cont_index = 0;
        for (auto *cont : {&gems_orange, &gems_magenta, &gems_blue, &gems_green})
        {
            for (ivec2 pos : *cont)
                static_gems.insert({pos, cont_index});
            cont_index++;
        }

        auto gem_walls_orange = map.FindTiles("gem wall orange"),
             gem_walls_magenta = map.FindTiles("gem wall magenta"),
             gem_walls_blue = map.FindTiles("gem wall blue"),
             gem_walls_green = map.FindTiles("gem wall green");

        cont_index = 0;
        for (auto *cont : {&gem_walls_orange, &gem_walls_magenta, &gem_walls_blue, &gem_walls_green})
        {
            for (ivec2 pos : *cont)
                gem_walls.insert({pos, cont_index});
            cont_index++;
        }

        for (const auto &it : map.FindTiles("bullet barrier"))
            bullet_barriers.insert(it);

        for (const auto &it : map.FindTiles("enemy turret"))
            turrets.insert({it * Map::tile_size + Map::tile_size/2, {}});

        for (const auto &it : map.FindTiles("enemy cube"))
            enemy_cubes.push_back(EnemyCube(it * Map::tile_size + Map::tile_size/2, it % 2 * 2 - 1));

        for (const auto &it : map.FindTiles("enemy laser ccw"))
            laser_turrets.insert({it * Map::tile_size + Map::tile_size/2, LaserTurret(-1)});
        for (const auto &it : map.FindTiles("enemy laser cw"))
            laser_turrets.insert({it * Map::tile_size + Map::tile_size/2, LaserTurret(1)});

        for (const auto &it : map.FindTiles("death smoke"))
            smoke_sources.insert({it * Map::tile_size + Map::tile_size/2, {}});


        // Count gems
        for (const auto &gem : static_gems)
            gem_count[gem.second]++;


        if (auto pos = map.FindSingleTileOpt("finale"); pos != ivec2(-1))
        {
            finale = 1;
            goal_pos = pos * Map::tile_size + Map::tile_size/2;
            p.has_gun = 0;
        }
    }
};

int map_index = 0;

World w, checkpoint_w;

void SaveState()
{
    checkpoint_w = w;
    checkpoint_w.darkness = 1;
    checkpoint_w.cam.pos = checkpoint_w.cam.real_pos = checkpoint_w.p.pos + checkpoint_w.camera_offset;
}


int main(int, char **)
{
    #ifdef NDEBUG
    win.Fullscreen(1);
    #endif

    constexpr int start_from_level = -1;
    //constexpr int start_from_level = Maps::map_count-1;
    //constexpr int start_from_level = -1;
    constexpr bool start_with_gun = start_from_level >= 2;

    constexpr int cam_margin = 32;
    constexpr ivec2 plr_enemy_hitbox(8,24);
    constexpr fvec2 plr_vel_cap(2.5,7);
    constexpr float plr_walk_acc = 0.6, plr_grav_acc = 0.2, plr_extra_jump_stopping_grav = 0.25, plr_jump_speed = 5.4, plr_discard_lag_vel_th = 0.1;
    constexpr int plr_can_respawn_after = 20, plr_pipe_ticks_for_one_pixel = 3, plr_gun_cooldown = 15;
    constexpr float plr_bullet_speed = 4;
    constexpr int bullet_particle_period = 1;
    constexpr float darkness_step = 0.01;
    constexpr int gem_speed = 4; // Should be a power of two, not greater than tile size.
    constexpr fvec3 gem_colors[4] {fvec3(1,0.6,0), fvec3(0.9,0,0.4), fvec3(0,0.5,1), fvec3(0,1,0.5)};
    constexpr float tur_angle_step = 0.03, tur_bullet_speed = 5;
    constexpr int tur_start_time = 30, tur_cooldown = 12;
    constexpr float las_tur_ang_vel = 0.009, las_tur_ray_offset = 29, las_tur_len_cap = 1000, las_tur_ray_width = 8, las_tur_shorten_ray_hitbox = 5;
    constexpr float smoke_radius = Map::tile_size * 5;

    Draw::Init();
    Sounds::Init();

    if (start_from_level == -1)
        Sounds::theme.stop();

    map_index = max(0, start_from_level);

    if (start_with_gun)
        w.p.has_gun = 1;

    w.SetMap(Maps::array[map_index]);

    SaveState();

    w.game_start = (start_from_level < 0);

    auto Tick = [&]
    {
        auto &p = w.p;

        { // Toggle music
            if (Keys::m.pressed())
                Sounds::theme_enabled = !Sounds::theme_enabled;
            Sounds::theme.volume(Sounds::theme_vol * Sounds::theme_enabled);
        }

        { // Starting scene
            if (w.game_start)
            {
                w.game_start_frames++;
                if (w.game_start_frames > 120)
                    w.game_can_begin = 1;
                if (w.game_can_begin && Keys::any.pressed())
                {
                    w.game_start = 0;
                    Sounds::theme.play();
                }
                return;
            }
        }

        { // Ending scene
            if (w.ending_scene)
            {
                w.ending_scene_frames++;
                w.ending_alpha = smoothstep(clamp(1 - (w.ending_scene_frames - 180) / 270.f, 0, 1));
                if (w.ending_alpha < 0.001 && Keys::any.pressed())
                    Program::Exit();
                return;
            }
        }

        { // Misc
            clamp_assign(w.darkness += darkness_step * (p.changing_level ? 1 : -1), 0, 1);
        }

        { // Player
            auto IsSolid = [&](ivec2 offset) -> bool
            {
                bool hits = Hitboxes::player.Hits(w.map, p.pos + offset);
                if (hits)
                    return 1;
                for (const auto &point : Hitboxes::player.Points())
                {
                    ivec2 tile_pos = div_ex(point + p.pos + offset, Map::tile_size);
                    if (w.static_gems.find(tile_pos) != w.static_gems.end())
                        return 1;
                    if (w.gem_walls.find(tile_pos) != w.gem_walls.end())
                        return 1;
                }
                return 0;
            };

            auto MoveIfPossible = [&](ivec2 offset) -> bool
            {
                if (IsSolid(offset))
                {
                    for (int i = 0; i < 2; i++)
                    {
                        if (p.vel[i] * offset[i] > 0)
                            p.vel[i] = 0;
                        if (p.vel_lag[i] * offset[i] > 0)
                            p.vel_lag[i] = 0;
                    }
                    return 0;
                }

                p.pos += offset;
                return 1;
            };

            { // Misc
                w.message = "";

                p.prev_ground = p.ground;
                p.prev_dead = p.dead;
                p.ground = IsSolid(ivec2(0,1));

                p.gun_offset = ivec2(0);
                if ((p.anim_state == 1 || p.anim_state == 2) && (p.anim_frame == 1 || p.anim_frame == 2 || p.anim_frame == 6 || p.anim_frame == 7))
                    p.gun_offset.y -= 1;
                if (p.anim_state == 0 && p.anim_frame == 1)
                    p.gun_offset.y += 1;

                if (p.dead)
                {
                    p.death_timer++;
                    if (p.death_timer > plr_can_respawn_after)
                        p.can_respawn = 1;
                }
            }

            { // Push out of wall on respawn
                if (p.respawning)
                {
                    if (IsSolid(ivec2(p.left ? 5 : -5, 0)))
                    {
                        p.level_change_timer++;

                        if (p.level_change_timer % plr_pipe_ticks_for_one_pixel == 0)
                            p.pos.x += 1;
                    }
                    else
                    {
                        p.respawning = 0;
                        p.level_change_timer = 0;
                    }
                }
            }

            { // Changing level
                if (p.changing_level)
                {
                    if (p.level_change_timer % plr_pipe_ticks_for_one_pixel == 0)
                        p.pos.x += 1;

                    p.level_change_timer++;
                    if (p.level_change_timer > plr_pipe_ticks_for_one_pixel * 40)
                    {
                        if (map_index >= Maps::map_count)
                            Program::Exit();
                        w.SetMap(Maps::array[map_index]);
                        checkpoint_w = w;
                    }
                }
            }

            { // Controls
                // Respawn
                if ((p.dead && p.can_respawn && Keys::any.pressed()) || Keys::escape.pressed())
                {
                    w = checkpoint_w;
                    Sounds::respawn(p.pos);
                }

                p.no_movement = p.dead || p.respawning || p.changing_level;

                // Enter a pipe
                if (!p.no_movement && !p.respawning && Hitboxes::player.Hits(w.map, p.pos.add_x(1), Map::flag_changes_map))
                {
                    p.changing_level = 1;
                    map_index++;
                    Sounds::door(p.pos);
                }

                // Walk
                p.hc = Keys::right.down() - Keys::left.down();

                // Jump
                p.jumps = p.ground && Keys::z.pressed();

                // Aim
                if (p.has_gun)
                {
                    bool up = Keys::up.down(),
                         fwd = Keys::left.down() || Keys::right.down(),
                         down = Keys::down.down();

                    if (up)
                        p.aim = (fwd ? -1 : -2);
                    else if (down)
                        p.aim = (fwd ? 1 : 2);
                    else
                        p.aim = 0;
                }
                else
                    p.aim = 0;

                // Shoot
                if (p.has_gun && !p.no_movement)
                {
                    if (p.gun_cooldown > 0)
                        p.gun_cooldown--;

                    if (Keys::x.pressed() && p.gun_cooldown == 0)
                    {
                        float angle = p.aim * f_pi / 4;
                        if (p.left)
                            angle = f_pi - angle;
                        fvec2 dir = fmat2::rotate2D(angle) /mul/ fvec2(1,0);
                        w.AddPlayerBullet(p.pos + p.gun_offset + dir * 10 + ivec2(p.left ? 3 : -3,0), dir * plr_bullet_speed);
                        p.gun_cooldown = plr_gun_cooldown;
                        Sounds::pew(p.pos);
                    }
                }

                // Move gems
                if (w.selected_static_gems.size() > 0 && w.moving_gems.size() == 0)
                {
                    p.gem_dir = ivec2(0);

                    if (!p.no_movement)
                    {
                        if (Keys::up.down())
                            p.gem_dir = ivec2(0,-1);
                        else if (Keys::down.down())
                            p.gem_dir = ivec2(0,1);
                        else
                            p.gem_dir = ivec2(p.left ? -1 : 1, 0);
                    }

                    if (p.gem_dir != ivec2(0) && Keys::c.down())
                    {
                        bool can_move = 0;
                        ivec2 one_movable_gem;
                        for (const auto &it : w.selected_static_gems)
                        {
                            if (!w.map.TileWithFlagExistsAt(Map::flag_solid, it + p.gem_dir) &&
                                w.static_gems.find(it + p.gem_dir) == w.static_gems.end())
                            {
                                can_move = 1;
                                one_movable_gem = it;
                                break;
                            }
                        }

                        if (can_move)
                        {
                            Sounds::gem_moves(one_movable_gem * Map::tile_size + Map::tile_size/2);

                            for (const auto &it : w.selected_static_gems)
                            {
                                auto gem_it = w.static_gems.find(it);
                                if (gem_it == w.static_gems.end())
                                    continue;
                                Gem new_gem;
                                new_gem.pos = it * Map::tile_size + Map::tile_size/2;
                                new_gem.dir = p.gem_dir;
                                new_gem.index = gem_it->second;
                                w.moving_gems.push_back(new_gem);
                                w.static_gems.erase(gem_it);
                            }
                            w.selected_static_gems.clear();
                        }
                    }
                }

                // Cancel controls if needed
                if (p.no_movement)
                {
                    p.hc = 0;
                    p.jumps = 0;
                    p.aim = 0;
                }

                { // Object interaction
                    if (w.obj.gun_exists && !p.no_movement)
                    {
                        if (div_ex(p.pos, Map::tile_size) == w.obj.gun_pos && p.ground)
                        {
                            w.message = "Press " + Keys::x.name() + " to pick up the gun";
                            if (Keys::x.pressed())
                            {
                                w.obj.gun_exists = 0;
                                p.has_gun = 1;
                                Sounds::get_gun(p.pos);
                                SaveState();
                            }
                        }
                    }
                }
            }

            { // Movement particles
                if (!p.no_movement)
                {
                    // Walking
                    if (p.hc && p.ground)
                        if (tick_stabilizer.ticks % 5 == 0)
                            w.AddParticle(0, fvec3(random_real_range(0.9,1)), 0.3, 1, w.p.pos.add_y(16).add_x(random_real_range(4) + 5*(p.left?1:-1)), fvec2(0,-random_real_range(0.1,0.3)), fvec2(0), random_real_range(0.1), 3, random_int(15), 30, 0.5);

                    // Jumping
                    if (p.jumps)
                    {
                        for (int i = 0; i < 6; i++)
                        {
                            float x = random_real_range(1);
                            w.AddParticle(0, fvec3(random_real_range(0.9,1)), 0.3, 1, w.p.pos.add_y(16).add_x(x*10), fvec2(x*0.5,-random_real_range(0.6,1.2)), fvec2(0,0.01), random_real_range(0.1), 6, random_int(15), 30, 0.25);
                        }
                    }
                    if (p.ground && !p.prev_ground)
                    {
                        for (int i = 0; i < 6; i++)
                        {
                            float x = random_real_range(1);
                            w.AddParticle(0, fvec3(random_real_range(0.9,1)), 0.3, 1, w.p.pos.add_y(16).add_x(x*10), fvec2(x,-random_real_range(0.2,0.4)), fvec2(0,0.01), random_real_range(0.1), 6, random_int(15), 30, 0.25);
                        }
                    }
                }
            }

            { // Change speed
                { // Walk
                    if (p.hc)
                    {
                        w.p.vel.x += p.hc * plr_walk_acc;
                    }
                    else
                    {
                        if (abs(p.vel.x) < plr_walk_acc)
                            p.vel.x = 0;
                        else
                            p.vel.x -= sign(p.vel.x) * plr_walk_acc;
                    }
                }

                { // Jump
                    if (p.jumps)
                    {
                        p.vel.y = -plr_jump_speed;
                        p.jump_in_progress = 1;
                    }
                    if (!Keys::z.down() || IsSolid(ivec2(0,-1)))
                        p.jump_in_progress = 0;
                }

                { // Gravity
                    if (p.vel.y < 0 && !p.jump_in_progress)
                        p.vel.y += plr_extra_jump_stopping_grav;
                    p.vel.y += plr_grav_acc;
                }


                { // Limit speed
                    for (int i = 0; i < 2; i++)
                    {
                        if (abs(w.p.vel[i]) > plr_vel_cap[i])
                            w.p.vel[i] = sign(w.p.vel[i]) * plr_vel_cap[i];
                    }
                }
            }

            { // Movement
                if (p.no_movement)
                {
                    p.vel = p.vel_lag = fvec2(0);
                }

                { // Discard lag if needed
                    for (int i = 0; i < 2; i++)
                    {
                        if (abs(p.vel[i]) <= plr_discard_lag_vel_th)
                            p.vel_lag[i] = 0;
                    }
                }

                ivec2 vel_int;
                { // Speed calculation
                    fvec2 vel_sum = p.vel + p.vel_lag;
                    vel_int = iround(vel_sum);
                    p.vel_lag = vel_sum - vel_int;
                }

                { // Actual movement
                    while (vel_int != ivec2(0))
                    {
                        for (int i = 0; i < 2; i++)
                        {
                            if (vel_int[i] != 0)
                            {
                                ivec2 offset(0);
                                offset[i] = sign(vel_int[i]);
                                if (MoveIfPossible(offset))
                                    vel_int[i] -= sign(vel_int[i]);
                                else
                                    vel_int[i] = 0;
                            }
                        }
                    }
                }
            }

            { // Death by spikes
                if (Hitboxes::player_small.Hits(w.map, p.pos, Map::flag_kills))
                    p.dead = 1;
            }

            { // Death by suffocation
                if (IsSolid(ivec2(0)) && !p.no_movement)
                    p.dead = 1;
            }

            /*{ // Death by moving gems
                if (!p.dead)
                {
                    for (const auto &gem : w.moving_gems)
                    {
                        for (const auto &offset : Hitboxes::player_small.Points())
                        {
                            if ((abs(p.pos + offset - gem.pos) < 8).all())
                            {
                                p.dead = 1;
                                break;
                            }
                        }
                        if (p.dead)
                            break;
                    }
                }
            }*/

            { // Death by turret collision
                if (!p.dead)
                {
                    for (const auto &it : w.turrets)
                        if (!it.second.dead && (abs(it.first - p.pos) < plr_enemy_hitbox/2 + ivec2(10)).all())
                            p.dead = 1;
                }
            }

            { // Death by enemy cube collision
                if (!p.dead)
                {
                    for (const auto &it : w.enemy_cubes)
                        if (!it.dead && (abs(it.pos - p.pos) < plr_enemy_hitbox/2 + ivec2(15)).all())
                            p.dead = 1;
                }
            }

            { // Death by laser turret collision
                if (!p.dead)
                {
                    for (const auto &it : w.laser_turrets)
                        if (!it.second.dead && (abs(it.first - p.pos) < plr_enemy_hitbox/2 + ivec2(10)).all())
                            p.dead = 1;
                }
            }

            { // Death by enemy bullet
                if (!p.dead)
                {
                    for (const auto &it : w.enemy_bullets)
                    {
                        for (ivec2 point : {it.pos, it.pos + it.vel/2})
                        {
                            if ((abs(point - p.pos) < plr_enemy_hitbox).all())
                                p.dead = 1;
                        }
                    }
                }
            }

            { // Death by laser
                if (!p.dead)
                {
                    for (const auto &pair : w.laser_turrets)
                    {
                        const auto &pos = pair.first;
                        const auto &it = pair.second;
                        if (it.dead)
                            continue;
                        fvec2 d(std::cos(it.angle), std::sin(it.angle));
                        fvec2 n(-d.y, d.x);
                        for (auto offset : Hitboxes::player_small.Points())
                        {
                            fvec2 point = p.pos + offset - pos;
                            ivec2 loc(point /dot/ d, point /dot/ n);
                            if (loc.x < it.len + las_tur_ray_offset - las_tur_shorten_ray_hitbox &&
                                -loc.x < it.len_back + las_tur_ray_offset - las_tur_shorten_ray_hitbox && abs(loc.y) <= las_tur_ray_width/2)
                            {
                                p.dead = 1;
                            }
                        }
                    }
                }
            }

            { // Death by smoke
                if (!p.dead)
                {
                    for (const auto &pair : w.smoke_sources)
                    {
                        const auto &pos = pair.first;
                        const auto &it = pair.second;
                        if (it.dead)
                            continue;
                        if ((pos - p.pos).len_sqr() < ipow(smoke_radius-16,2))
                            p.dead = 1;
                    }
                }
            }

            { // Revive if player shouldn't be dead
                if (p.respawning || p.changing_level)
                    p.dead = 0;
            }

            { // Touching the goal
                if (w.finale && !w.ending_scene && (p.pos - w.goal_pos).len() < 20)
                {
                    w.ending_scene = 1;
                    Sounds::end(ivec2(0)).relative();
                    Sounds::theme.stop();
                }
            }

            { // Death particles
                if (p.dead && !p.prev_dead)
                {
                    for (int i = 0; i < 50; i++)
                    {
                        fvec2 offset = fmat2::rotate2D(random_real_range(f_pi)) /mul/ fvec2(1, 0);
                        float c = random_real_range(0.6,1);
                        w.AddParticle(1, fvec3(c, c*random_real_range(0.1,0.4), c*0.1), random_real_range(0.5,1), random_real(1), w.p.pos + offset * random_real(16), offset * random_real_range(0.01,2.2), fvec2(0,0.005), random_real_range(0.3), 10, random_int(40), 180, 0.25);
                    }
                }
            }

            { // Animation
                if (p.hc != 0)
                    p.left = p.hc < 0;

                int prev_state = p.anim_state;

                if (p.ground && (p.hc == 0 || !p.prev_ground))
                    p.anim_state = 0;
                else if (p.ground)
                    p.anim_state = 1;
                else
                    p.anim_state = 2;

                if (p.anim_state != prev_state)
                {
                    p.anim_frame = 0;
                    p.ticks_since_anim_state_change = 0;
                }

                p.ticks_since_anim_state_change++;

                int period = 0, cap = 1;

                if (p.anim_state == 0)
                    period = 56, cap = 2;
                else if (p.anim_state == 1)
                    period = 6, cap = 10;
                else if (p.anim_state == 2)
                    p.anim_frame = sign(iround(p.vel.y / 2)) + 1;

                if (period && p.ticks_since_anim_state_change % period == 0)
                    p.anim_frame = (p.anim_frame + 1) % cap;
            }

            { // Most sounds
                if (!p.no_movement)
                {
                    if (p.jumps)
                        Sounds::jump(p.pos);
                    if (p.ground && !p.prev_ground)
                        Sounds::land(p.pos);
                }

                if (p.dead && !p.prev_dead)
                    Sounds::death(p.pos);
            }

            { // Update audio listener position
                Audio::ListenerPos(p.pos.to_vec3(-250));
            }
        }

        { // Turrets
            auto iter = w.turrets.begin();
            while (iter != w.turrets.end())
            {
                auto &pos = iter->first;
                auto &it = iter->second;
                if (it.dead)
                {
                    iter++;
                    continue;
                }
                fvec2 target_dir = (p.pos - pos).norm();
                fvec2 cur_dir(std::cos(it.angle), std::sin(it.angle));
                float delta_angle = (target_dir != fvec2(0) ? std::acos(target_dir /dot/ cur_dir) * sign(cur_dir /cross/ target_dir) : 0);

                auto line_hitbox = Hitbox::Line(pos, p.pos, 2);

                bool player_visible = !line_hitbox.Hits(w.map, ivec2(0,0)) && !line_hitbox.HitsContainer(w.gem_walls, ivec2(0,0)) && !line_hitbox.HitsContainer(w.static_gems, ivec2(0,0));

                if (player_visible)
                {
                    if (abs(delta_angle) < tur_angle_step)
                        it.angle += delta_angle;
                    else
                        it.angle += tur_angle_step * sign(delta_angle);
                }

                if (player_visible && !p.dead)
                {
                    if (it.ticks_since_player_visible == 0)
                        Sounds::turret_starts(pos);
                    it.ticks_since_player_visible++;

                    if (it.ticks_since_player_visible >= tur_start_time && (it.ticks_since_player_visible - tur_start_time) % tur_cooldown == 0)
                    {
                        Sounds::turret_shoots(pos);
                        cur_dir = fvec2(std::cos(it.angle), std::sin(it.angle)); // We recompute this for new angle
                        w.AddEnemyBullet(pos + cur_dir * 20 + fvec2(-cur_dir.y, cur_dir.x) * (it.barrel ? 1 : -1) * 3, cur_dir * tur_bullet_speed);
                        it.barrel = !it.barrel;
                    }
                }
                else
                {
                    it.ticks_since_player_visible = 0;
                }

                // Health
                if (w.gem_count[0] > 0)
                {
                    w.EnemyShield(pos, 36);
                }
                else
                {
                    for (auto &bullet : w.bullets)
                    {
                        if ((bullet.pos - pos).len_sqr() < ipow(20,2))
                        {
                            it.hp--;
                            if (it.hp == 0)
                            {
                                constexpr int gem_color = 0;
                                Sounds::enemy_dies(pos);
                                it.dead = 1;
                                for (int i = 0; i < 40; i++)
                                {
                                    fvec2 d(random_real_range(1), random_real_range(1));
                                    float c = std::pow(random_int(3) / 2.f, 1.5);
                                    w.AddParticle(1, gem_colors[gem_color] * (1-c) + fvec3(1) * c, random_real_range(0.5,0.9), random_real_range(0.1,0.4), pos + d * random_real(16), d * random_real_range(0.1,2.8),
                                                  fvec2(0,0.002), random_real_range(0.3), 12, random_int(15), 30, 0.25);
                                }
                            }
                            else if (it.hp > 0)
                            {
                                Sounds::enemy_hurt(pos);
                            }
                            bullet.remove = 1;
                        }
                    }
                }

                iter++;
            }
        }

        { // Laser turrets
            auto PointSolid = [&](ivec2 pos) -> bool
            {
                ivec2 tile_pos = div_ex(pos, Map::tile_size);
                if (w.map.TileWithFlagExistsAt(Map::flag_solid, tile_pos))
                    return 1;
                if (w.static_gems.find(tile_pos) != w.static_gems.end())
                    return 1;
                if (w.gem_walls.find(tile_pos) != w.gem_walls.end())
                    return 1;
                return 0;
            };

            auto iter = w.laser_turrets.begin();
            while (iter != w.laser_turrets.end())
            {
                auto &pos = iter->first;
                auto &it = iter->second;
                if (it.dead)
                {
                    iter++;
                    continue;
                }

                it.angle += it.dir * las_tur_ang_vel;

                fvec2 d(std::cos(it.angle), std::sin(it.angle));

                // Lasers
                for (int s = -1; s <= 1; s += 2)
                {
                    float &l = (s > 0 ? it.len : it.len_back);
                    l = 0;
                    float step = 8; // Should be a power of two.
                    while (1)
                    {
                        fvec2 point = pos + d * s * (l + las_tur_ray_offset);
                        if (PointSolid(point))
                        {
                            step /= 2;
                            l -= step;
                            if (step < 0.1)
                                break;
                            else
                                continue;
                        }
                        l += step;

                        if (l > las_tur_len_cap)
                            break;
                    }
                }

                // Particles at laser ends
                for (int s = -1; s <= 1; s += 2)
                {
                    float c = std::pow(random_int(3) / 2.f, 1.5);
                    w.AddParticle(0, gem_colors[2] * (1-c) + fvec3(1) * c, random_real_range(0.5,0.9), random_real_range(0.1,0.4), pos + d * s * ((s > 0 ? it.len : it.len_back) + las_tur_ray_offset) + fvec2(-d.y,d.x) * random_real_range(2),
                                  -d * s * random_real_range(0.1,2.8) + fvec2(-d.y,d.x) * random_real_range(0.1), d * s * 0.02, random_real_range(0.3), 8, random_int(15), 30, 0.33);
                }

                // Health
                if (w.gem_count[2] > 0)
                {
                    w.EnemyShield(pos, 36);
                }
                else
                {
                    for (auto &bullet : w.bullets)
                    {
                        if ((bullet.pos - pos).len_sqr() < ipow(20,2))
                        {
                            it.hp--;
                            if (it.hp == 0)
                            {
                                constexpr int gem_color = 2;
                                Sounds::enemy_dies(pos);
                                it.dead = 1;
                                for (int i = 0; i < 40; i++)
                                {
                                    fvec2 d(random_real_range(1), random_real_range(1));
                                    float c = std::pow(random_int(3) / 2.f, 1.5);
                                    w.AddParticle(1, gem_colors[gem_color] * (1-c) + fvec3(1) * c, random_real_range(0.5,0.9), random_real_range(0.1,0.4), pos + d * random_real(16), d * random_real_range(0.1,2.8),
                                                  fvec2(0,0.002), random_real_range(0.3), 12, random_int(15), 30, 0.25);
                                }
                            }
                            else if (it.hp > 0)
                            {
                                Sounds::enemy_hurt(pos);
                            }
                            bullet.remove = 1;
                        }
                    }
                }

                iter++;
            }
        }

        { // Enemy cubes
            auto IsSolid = [&](decltype(w.enemy_cubes)::iterator this_it, ivec2 pos) -> bool
            {
                bool hits = Hitboxes::enemy_cube.Hits(w.map, pos);
                if (hits)
                    return 1;
                for (const auto &point : Hitboxes::enemy_cube.Points())
                {
                    ivec2 tile_pos = div_ex(point + pos, Map::tile_size);
                    if (w.static_gems.find(tile_pos) != w.static_gems.end())
                        return 1;
                    if (w.gem_walls.find(tile_pos) != w.gem_walls.end())
                        return 1;
                }
                auto it = w.enemy_cubes.begin();
                while (it != w.enemy_cubes.end())
                {
                    if (it == this_it || it->dead)
                    {
                        it++;
                        continue;
                    }

                    if ((abs(pos - it->pos) < 30).all())
                        return 1;

                    it++;
                }
                return 0;
            };

            auto iter = w.enemy_cubes.begin();
            while (iter != w.enemy_cubes.end())
            {
                auto &it = *iter;
                if (it.dead)
                {
                    iter++;
                    continue;
                }

                // Change direction
                if (!IsSolid(iter, it.pos + ivec2(0)))
                {
                    bool h = IsSolid(iter, it.pos + it.dir.set_y(0)),
                         v = IsSolid(iter, it.pos + it.dir.set_x(0));
                    bool dir_changed = 0;

                    if (h)
                    {
                        it.dir.x *= -1;
                        dir_changed = 1;
                    }
                    if (v)
                    {
                        it.dir.y *= -1;
                        dir_changed = 1;
                    }
                    if (!dir_changed && IsSolid(iter, it.pos + it.dir))
                    {
                        it.dir *= -1;
                        dir_changed = 1;
                    }


                    if (dir_changed)
                        Sounds::cube_hits_wall(it.pos);
                }

                // Move
                {
                    if (!IsSolid(iter, it.pos + it.dir))
                        it.pos += it.dir;
                }

                // Health
                if (w.gem_count[1] > 0)
                {
                    w.EnemyShield(it.pos, 28);
                }
                else
                {
                    for (auto &bullet : w.bullets)
                    {
                        if ((bullet.pos - it.pos).len_sqr() < ipow(20,2))
                        {
                            it.hp--;
                            if (it.hp == 0)
                            {
                                constexpr int gem_color = 1;
                                Sounds::enemy_dies(it.pos);
                                it.dead = 1;
                                for (int i = 0; i < 40; i++)
                                {
                                    fvec2 d(random_real_range(1), random_real_range(1));
                                    float c = std::pow(random_int(3) / 2.f, 1.5);
                                    w.AddParticle(1, gem_colors[gem_color] * (1-c) + fvec3(1) * c, random_real_range(0.5,0.9), random_real_range(0.1,0.4), it.pos + d * random_real(16), d * random_real_range(0.1,2.8),
                                                  fvec2(0,0.002), random_real_range(0.3), 12, random_int(15), 30, 0.25);
                                }
                            }
                            else if (it.hp > 0)
                            {
                                Sounds::enemy_hurt(it.pos);
                            }
                            bullet.remove = 1;
                        }
                    }
                }

                iter++;
            }
        }

        { // Smoke sources
            auto iter = w.smoke_sources.begin();
            while (iter != w.smoke_sources.end())
            {
                auto &pos = iter->first;
                auto &it = iter->second;
                if (it.dead)
                {
                    iter++;
                    continue;
                }

                // Particles
                for (int i = 0; i < 6; i++)
                {
                    float dist = random_real(1);
                    w.AddParticle(1, gem_colors[3] * random_real_range(0.1,1), random_real_range(0.8,1), random_real_range(0.9,1),
                                  pos + fmat2::rotate2D(random_real_range(f_pi)) /mul/ fvec2(std::pow(dist,0.5) * (smoke_radius + 16),0),
                                  fvec2(0), fvec2(0,-0.002), random_real_range(0.1), 30, random_int_range(0 + 13 * dist, 30)*6, 30*6, 0.33, 1);
                }

                // Health
                if (w.gem_count[3] > 0)
                {
                    w.EnemyShield(pos, smoke_radius);
                }
                else
                {
                    for (auto &bullet : w.bullets)
                    {
                        if ((bullet.pos - pos).len_sqr() < ipow(smoke_radius*0.8,2))
                        {
                            it.hp--;
                            if (it.hp == 0)
                            {
                                constexpr int gem_color = 3;
                                Sounds::enemy_dies(pos);
                                it.dead = 1;
                                for (int i = 0; i < 40; i++)
                                {
                                    fvec2 d(random_real_range(1), random_real_range(1));
                                    float c = std::pow(random_int(3) / 2.f, 1.5);
                                    w.AddParticle(1, gem_colors[gem_color] * (1-c) + fvec3(1) * c, random_real_range(0.5,0.9), random_real_range(0.1,0.4), pos + d * random_real(16), d * random_real_range(0.1,2.8),
                                                  fvec2(0,0.002), random_real_range(0.3), 12, random_int(15), 30, 0.25);
                                }
                            }
                            else if (it.hp > 0)
                            {
                                Sounds::enemy_hurt(pos);
                            }
                            bullet.remove = 1;
                        }
                    }
                }

                iter++;
            }
        }

        { // Bullets
            static const std::vector<ivec2> hitbox_offsets = {ivec2(-2,-2), ivec2(-2,2), ivec2(2,-2), ivec2(2,2)};
            auto it = w.bullets.begin();
            while (it != w.bullets.end())
            {
                fvec2 dir = it->vel.norm(), dir2(dir.y, -dir.x);
                bool &remove = it->remove, &was_deflected = it->deflected;
                bool tile_destroyed = 0;
                std::vector<ivec2> destroyed_tiles;
                std::vector<ivec2> hit_gems;

                if (!remove)
                {
                    for (ivec2 offset : hitbox_offsets)
                    {
                        ivec2 tile_pos = div_ex(iround(it->pos + offset), Map::tile_size);
                        auto tile = w.map.Get(tile_pos);
                        for (int i = 0; i < Map::layer_count; i++)
                        {
                            auto this_tile = tile.*Map::layer_list[i];
                            if (this_tile == Map::no_tile)
                                continue;
                            const auto &info = Map::tiling.GetTile(this_tile);
                            if (info.HasFlag(Map::flag_solid))
                            {
                                remove = 1;
                            }
                            if (info.HasFlag(Map::flag_destructable))
                            {
                                w.map.Set(tile_pos, Map::layer_list[i], Map::no_tile);
                                remove = 1;
                                tile_destroyed = 1;
                                destroyed_tiles.push_back(tile_pos);
                            }
                        }

                        if (w.static_gems.find(tile_pos) != w.static_gems.end())
                        {
                            remove = 1;
                            hit_gems.push_back(tile_pos);
                        }
                        if (w.gem_walls.find(tile_pos) != w.gem_walls.end())
                        {
                            remove = 1;
                        }
                        if (w.bullet_barriers.find(tile_pos) != w.bullet_barriers.end())
                        {
                            remove = 1;
                            was_deflected = 1;
                        }
                    }

                    if (hit_gems.size())
                    {
                        w.selected_static_gems.clear();

                        static void (*func)(ivec2, const decltype(w.static_gems) &, decltype(w.selected_static_gems) &)
                            = [](ivec2 pos, const decltype(w.static_gems) &gems, decltype(w.selected_static_gems) &selected)
                        {
                            if (gems.find(pos) == gems.end())
                                return;
                            selected.insert(pos);

                            ivec2 next[4]
                            {
                                pos + ivec2(1,0),
                                pos + ivec2(-1,0),
                                pos + ivec2(0,1),
                                pos + ivec2(0,-1),
                            };

                            for (const auto &it : next)
                            {
                                if (selected.find(it) != selected.end())
                                    continue;
                                func(it, gems, selected);
                            }
                        };

                        for (const auto &pos : hit_gems)
                            func(pos, w.static_gems, w.selected_static_gems);
                    }
                }

                if (remove)
                {
                    if (!tile_destroyed)
                    {
                        if (was_deflected)
                            Sounds::bullet_deflected(it->pos);
                        else
                            Sounds::bullet_hits(it->pos);
                    }
                    else
                    {
                        Sounds::block_breaks(it->pos);
                    }

                    for (int i = 0; i < 10; i++)
                    {
                        fvec2 dir = fmat2::rotate2D(random_real_range(f_pi)) /mul/ fvec2(1,0);
                        w.AddParticle(1, fvec3(0.7,1,0.2), 1, 0.5, it->pos + dir * random_real_range(3), dir * random_real(0.6), fvec2(0), random_real_range(0.2), 6, random_int(15), 30, 0.3);
                    }
                    if (tile_destroyed)
                    {
                        for (auto tile_pos : destroyed_tiles)
                        {
                            fvec2 base_pos = tile_pos * Map::tile_size + Map::tile_size/2;
                            for (int i = 0; i < 16; i++)
                            {
                                fvec2 offset(random_real_range(Map::tile_size/2), random_real_range(Map::tile_size/2));
                                fvec2 pos = base_pos + offset;
                                w.AddParticle(0, fvec3(random_real_range(0.9,1)), 0.3, 1, pos, offset.norm() * random_real(1.5), fvec2(0,0.03), random_real_range(0.1), 6, random_int(15), 30, 0.25);
                            }
                        }
                    }
                    it = w.bullets.erase(it);
                    continue;
                }

                if ((it->pos < ivec2(0)).any() || (it->pos > w.map.Size() * Map::tile_size).any())
                {
                    it = w.bullets.erase(it);
                    continue;
                }

                it->pos += it->vel;
                it->age++;
                if (it->age % bullet_particle_period == 0)
                    w.AddParticle(0, fvec3(0.7,1,0.2), 1, 0.5, it->pos + dir * random_real_range(1) + dir2 * random_real_range(1), fvec2(0), fvec2(0), random_real_range(0.1), 4, random_int(10), 18, 0.3);
                it++;
            }
        }

        { // Enemy bullets
            static const std::vector<ivec2> hitbox_offsets = {ivec2(-2,-2), ivec2(-2,2), ivec2(2,-2), ivec2(2,2)};
            auto it = w.enemy_bullets.begin();
            while (it != w.enemy_bullets.end())
            {
                fvec2 dir = it->vel.norm(), dir2(dir.y, -dir.x);
                bool remove = 0, tile_destroyed = 0;
                std::vector<ivec2> destroyed_tiles;

                for (ivec2 offset : hitbox_offsets)
                {
                    ivec2 tile_pos = div_ex(iround(it->pos + offset), Map::tile_size);
                    auto tile = w.map.Get(tile_pos);
                    for (int i = 0; i < Map::layer_count; i++)
                    {
                        auto this_tile = tile.*Map::layer_list[i];
                        if (this_tile == Map::no_tile)
                            continue;
                        const auto &info = Map::tiling.GetTile(this_tile);
                        if (info.HasFlag(Map::flag_solid))
                        {
                            remove = 1;
                        }
                        if (info.HasFlag(Map::flag_destructable))
                        {
                            w.map.Set(tile_pos, Map::layer_list[i], Map::no_tile);
                            remove = 1;
                            tile_destroyed = 1;
                            destroyed_tiles.push_back(tile_pos);
                        }
                    }

                    if (w.static_gems.find(tile_pos) != w.static_gems.end())
                        remove = 1;
                    if (w.gem_walls.find(tile_pos) != w.gem_walls.end())
                        remove = 1;
                    // We ignore barriers... if (w.bullet_barriers.find(tile_pos) != w.bullet_barriers.end())
                }

                if (remove)
                {
                    if (!tile_destroyed)
                    {
                        Sounds::turret_bullet_hits(it->pos);
                    }
                    else
                    {
                        Sounds::block_breaks(it->pos);
                    }

                    for (int i = 0; i < 10; i++)
                    {
                        fvec2 dir = fmat2::rotate2D(random_real_range(f_pi)) /mul/ fvec2(1,0);
                        w.AddParticle(1, fvec3(1,0.7,0.1), 1, 0.5, it->pos + dir * random_real_range(3), dir * random_real(0.6), fvec2(0), random_real_range(0.2), 4, random_int(15), 30, 0.3);
                    }
                    if (tile_destroyed)
                    {
                        for (auto tile_pos : destroyed_tiles)
                        {
                            fvec2 base_pos = tile_pos * Map::tile_size + Map::tile_size/2;
                            for (int i = 0; i < 16; i++)
                            {
                                fvec2 offset(random_real_range(Map::tile_size/2), random_real_range(Map::tile_size/2));
                                fvec2 pos = base_pos + offset;
                                w.AddParticle(0, fvec3(random_real_range(0.9,1)), 0.3, 1, pos, offset.norm() * random_real(1.5), fvec2(0,0.03), random_real_range(0.1), 6, random_int(15), 30, 0.25);
                            }
                        }
                    }
                    it = w.enemy_bullets.erase(it);
                    continue;
                }

                if ((it->pos < ivec2(0)).any() || (it->pos > w.map.Size() * Map::tile_size).any())
                {
                    it = w.enemy_bullets.erase(it);
                    continue;
                }

                it->pos += it->vel;
                it->age++;
                if (it->age % bullet_particle_period == 0)
                    w.AddParticle(0, fvec3(1,0.7,0.1), 1, 0.5, it->pos + dir * random_real_range(2.5) + dir2 * random_real_range(1), fvec2(0), fvec2(0), random_real_range(0.1), 3, random_int(10), 18, 0.3);
                it++;
            }
        }

        { // Bullet barrier particles
            for (const auto &it : w.bullet_barriers)
            {
                if ((abs(it * Map::tile_size + Map::tile_size/2 - w.cam.pos) > screen_sz/2 + Map::tile_size).any())
                    continue;
                for (int i = 0; i < 1; i++)
                {
                    w.AddParticle(0, fvec3(random_real_range(0.6,1)), random_real_range(0.1,0.5), random_real_range(0.25,0.75), it * Map::tile_size + Map::tile_size/2 + fvec2(random_real_range(8), random_real_range(8)),
                                  fmat2::rotate2D(random_real_range(f_pi)) /mul/ fvec2(random_real(0.1),0), fvec2(0), random_real_range(0.3), 3, random_int(15), 30, 0.5);
                }
            }
        }

        { // Moving gems
            // Make static if needed
            bool more_iterations = 1;
            bool any_gem_stopped = 0;
            ivec2 stopped_pos;
            while (more_iterations)
            {
                more_iterations = 0;
                auto it = w.moving_gems.begin();
                while (it != w.moving_gems.end())
                {
                    ivec2 test_pos = it->pos + it->dir * 15 - (it->dir < 0);
                    ivec2 tile_pos = div_ex(test_pos, Map::tile_size);
                    ivec2 this_tile_pos = div_ex(it->pos, Map::tile_size);
                    if (w.map.TileWithFlagExistsAt(Map::flag_solid, tile_pos) || w.static_gems.find(tile_pos) != w.static_gems.end() || w.gem_walls.find(tile_pos) != w.gem_walls.end())
                    {
                        more_iterations = 1;
                        any_gem_stopped = 1;
                        stopped_pos = this_tile_pos;
                        w.static_gems.insert({this_tile_pos, it->index});
                        it = w.moving_gems.erase(it);
                        continue;
                    }
                    it++;
                }
            }
            if (any_gem_stopped)
                Sounds::gem_stops(stopped_pos * Map::tile_size + Map::tile_size/2);

            // Remove groups if needed
            if (any_gem_stopped && w.moving_gems.size() == 0)
            {
                constexpr ivec2 dirs[] {ivec2(1,0), ivec2(1,1), ivec2(0,1), ivec2(-1,1)};
                std::unordered_set<ivec2> removed_gems;
                for (const auto &it : w.static_gems)
                {
                    ivec2 it_pos = it.first;
                    int it_index = it.second;

                    for (const auto &d : dirs)
                    {
                        std::vector<ivec2> found;

                        for (int s = -1; s <= 1; s += 2)
                        {
                            int l = 1;
                            while (1)
                            {
                                if (auto gem_it = w.static_gems.find(it_pos + d * s * l); gem_it != w.static_gems.end() && gem_it->second == it_index)
                                {
                                    l++;
                                    found.push_back(gem_it->first);
                                }
                                else
                                {
                                    break;
                                }
                            }
                        }
                        if (found.size() >= 2)
                        {
                            removed_gems.insert(it_pos);
                            for (const auto &gem : found)
                                removed_gems.insert(gem);
                        }
                    }
                }

                if (removed_gems.size() > 0)
                {
                    Sounds::gem_breaks(*removed_gems.begin() * Map::tile_size + Map::tile_size/2);

                    for (const auto &removed_gem : removed_gems)
                    {
                        auto it = w.static_gems.find(removed_gem);
                        if (it == w.static_gems.end())
                            continue;
                        for (int i = 0; i < 16; i++)
                        {
                            fvec2 d(random_real_range(1), random_real_range(1));
                            float c = std::pow(random_int(3) / 2.f, 1.5);
                            w.AddParticle(1, gem_colors[it->second] * (1-c) + fvec3(1) * c, 1, random_real_range(0.75,1),
                                it->first * Map::tile_size + Map::tile_size/2 + d * 4, d * random_real_range(0.2,0.7), fvec2(0,0.025), random_real_range(0.3), 6, random_int(20), 40, 0.25);
                        }
                        w.static_gems.erase(it);
                    }

                    for (int i = 0; i < 4; i++)
                        w.gem_count[i] = 0;
                    for (const auto &gem : w.static_gems)
                        w.gem_count[gem.second]++;

                    for (int i = 0; i < 4; i++)
                    {
                        if (w.gem_count[i] > 0)
                            continue;
                        auto it = w.gem_walls.begin();
                        while (it != w.gem_walls.end())
                        {
                            if (it->second == i)
                            {
                                for (int i = 0; i < 16; i++)
                                {
                                    fvec2 d(random_real_range(1), random_real_range(1));
                                    float c = std::pow(random_int(3) / 2.f, 1.5);
                                    w.AddParticle(1, gem_colors[it->second] * (1-c) + fvec3(1) * c, 1, random_real_range(0.75,1),
                                        it->first * Map::tile_size + Map::tile_size/2 + d * 4, d * random_real_range(0.2,0.7), fvec2(0,0.025), random_real_range(0.3), 6, random_int(20), 40, 0.25);
                                }
                                it = w.gem_walls.erase(it);
                                continue;
                            }
                            it++;
                        }
                    }
                }
            }

            // Move
            for (auto &it : w.moving_gems)
            {
                it.pos += it.dir * gem_speed;

                w.AddParticle(0, gem_colors[it.index], random_real_range(0.5,1), random_real(1), it.pos + fvec2(random_real_range(6), random_real_range(6)), fvec2(0), fvec2(0,0.005),
                              random_real_range(0.15), 7, random_int(20), 40, 0.33);
            }
        }

        { // Particles
            for (auto *list : {&w.particles, &w.particles_front})
            {
                auto it = list->begin();
                while (it != list->end())
                {
                    if (it->cur_frames > it->max_frames)
                    {
                        it = list->erase(it);
                        continue;
                    }

                    it->vel += it->acc;
                    it->pos += it->vel;
                    it->angle += it->av;
                    it->cur_frames++;

                    it++;
                }
            }
        }

        { // Camera
            ivec2 dst = w.p.pos + w.camera_offset;
            fvec2 delta = (dst - w.cam.real_pos);
            float d = delta.len();
            delta = delta.norm();

            fvec2 acc = delta * std::atan(d / 1000) / (f_pi/2) * 10;
            w.cam.vel += acc;

            float speed = w.cam.vel.len();
            w.cam.vel -= w.cam.vel.norm() * (0.1*speed + 0.01*ipow(speed,2));

            w.cam.real_pos += w.cam.vel;

            for (int i = 0; i < 2; i++)
            {
                if (abs(p.pos[i] - w.cam.real_pos[i]) > screen_sz[i]/2 - cam_margin)
                    w.cam.real_pos[i] = p.pos[i] - (screen_sz[i]/2 - cam_margin) * sign(p.pos[i] - w.cam.real_pos[i]);
            }

            w.cam.pos = iround(w.cam.real_pos);
        }
    };
    auto Render = [&]
    {
        Graphics::Clear(Graphics::color);

        { // Starting scene
            if (w.game_start)
            {
                r.Quad(ivec2(0), screen_sz).tex(ivec2(1568,1620)).center();
                r.Text(ivec2(0,screen_sz.y/2), "A game by HolyBlackCat (blckcat@inbox.ru) made for LD41, Apr 21-23, 2018").color(fvec3(0.2)).alpha(random_real_range(0.8,1)).align_v(1);
                if (w.game_can_begin)
                    r.Text(ivec2(0,64), "Press any key").color(fvec3(1)).alpha(random_real_range(0.8,1));
                return;
            }
        }

        { // Ending scene
            if (w.ending_scene)
            {
                r.Quad(ivec2(0), screen_sz).tex(ivec2(1568,1350)).center();
                r.Quad(ivec2(0), screen_sz).color(fvec3(1)).alpha(w.ending_alpha).center();
                return;
            }
        }

        { // Background
            constexpr float depth[4] {0,3.5,2.5,1.5};


            for (int z = 0; z < 4; z++)
            for (int x = -1; x < 1; x++)
            {
                int off = (w.finale && z != 0 ? 128 : 0);
                float offset = (depth[z] == 0 ? 0 : -w.cam.pos.x / depth[z]);
                int pos_x = x * screen_sz.x + mod_ex(iround(offset), screen_sz.x);
                r.Quad(-screen_sz/2 + ivec2(pos_x,off), screen_sz.sub_y(off)).tex(ivec2(2048 - screen_sz.x, screen_sz.y * z + off));
            }
        }

        { // Map background
            w.map.Render(r, screen_sz, w.cam.pos, Map::back);
        }

        { // Particles
            for (const auto &it : w.particles)
            {
                float size = it.size * std::pow(1 - it.cur_frames / float(it.max_frames), it.size_power);
                r.Quad(it.pos - w.cam.pos, fvec2(size)).color(it.color).alpha(it.alpha).beta(it.beta).rotate(it.angle).center();
            }
        }

        { // Map
            w.map.Render(r, screen_sz, w.cam.pos, Map::mid);
        }

        { // Turrets
            for (const auto &pair : w.turrets)
            {
                const auto &it = pair.second;
                if (it.dead)
                    continue;
                ivec2 pos = pair.first;
                if ((abs(pos - w.cam.pos) > screen_sz/2 + Map::tile_size*3).any())
                    continue;
                r.Quad(pos - w.cam.pos, ivec2(48-2)).tex(ivec2(32,32)+1).center(ivec2(14,24)-1).rotate(it.angle);
                r.Quad(pos - w.cam.pos, ivec2(48-2)).tex(ivec2(32+48,32)+1).center();
            }
        }

        { // Laser turrets
            for (const auto &pair : w.laser_turrets)
            {
                const auto &it = pair.second;
                if (it.dead)
                    continue;
                ivec2 pos = pair.first;

                r.Quad(pos - w.cam.pos, ivec2(80,48)-2).tex(ivec2(0,96)+1).center().rotate(it.angle);

                // Rays
                for (int s = -1; s <= 1; s += 2)
                {
                    r.Quad(pos - w.cam.pos, fvec2(max((s > 0 ? it.len : it.len_back) - 5, 0), 14)).tex(ivec2(49,81), ivec2(14,14)).pixel_center(ivec2(0,7)).rotate(s > 0 ? it.angle : it.angle + f_pi).translate(fvec2(las_tur_ray_offset,0))
                     .alpha(random_real_range(0.85,1)).beta(random_real_range(0.1,0.4));
                }

                // Ray ends
                fvec2 d(std::cos(it.angle), std::sin(it.angle));
                for (int s = -1; s <= 1; s += 2)
                {
                    r.Quad(pos - w.cam.pos + d * s * ((s > 0 ? it.len : it.len_back) + las_tur_ray_offset), fvec2(14)).tex(ivec2(65,81)).center().alpha(random_real_range(0.85,1)).beta(random_real_range(0.1,0.4));
                }

                r.Quad(pos - w.cam.pos, ivec2(48-2)).tex(ivec2(32+48,32+48)+1).center();
            }
        }

        { // Enemy cubes
            for (const auto &it : w.enemy_cubes)
            {
                if (it.dead)
                    continue;
                if ((abs(it.pos - w.cam.pos) > screen_sz/2 + Map::tile_size*3).any())
                    continue;
                r.Quad(it.pos - w.cam.pos, ivec2(30)).tex(ivec2(137,41)).alpha(random_real_range(0.9,1)).beta(random_real_range(0,1)).center();
                r.Quad(it.pos - w.cam.pos + iround(fvec2(random_real_range(0.6), random_real_range(0.6))), ivec2(30))
                 .tex(ivec2(137,41)).alpha(random_real_range(0.5,1)).beta(random_real_range(0,1)).rotate(random_real_range(0.1)).center();
            }
        }

        { // Gem walls
            // Static gems
            for (const auto &it : w.gem_walls)
            {
                ivec2 pos = it.first;
                int index = it.second;
                if ((abs(pos * Map::tile_size + Map::tile_size/2 - w.cam.pos) > screen_sz/2 + Map::tile_size).any())
                    continue;
                r.Quad(pos * Map::tile_size - w.cam.pos, ivec2(Map::tile_size)).tex(ivec2(48+16*index,976)).alpha(random_real_range(0.85,1)).beta(random_real_range(0.85,1));
            }
        }

        { // Gems
            // Link to the selection
            if (w.selected_static_gems.size() > 0)
            {
                int index = 0;
                float squared_dist;
                for (const auto &tile_pos : w.selected_static_gems)
                {
                    fvec2 pos = tile_pos * Map::tile_size + Map::tile_size/2;
                    float d = (pos - w.p.pos).len_sqr();
                    if (index == 0 || d < squared_dist)
                    {
                        squared_dist = d;
                        w.p.nearest_selected_gem = pos;
                    }
                    index++;
                }

                float dist = std::sqrt(squared_dist);
                constexpr float step = 6;
                float angle = 0;
                if (w.p.pos != w.p.nearest_selected_gem)
                    angle = std::atan2(w.p.nearest_selected_gem.y - w.p.pos.y, w.p.nearest_selected_gem.x - w.p.pos.x);
                fvec2 n(-std::sin(angle), std::cos(angle));
                for (float i = 0; i <= min(dist, screen_sz.x); i += step)
                {
                    float t = (i + step/2) / dist;
                    r.Quad(w.p.pos * (1 - t) + w.p.nearest_selected_gem * t - w.cam.pos + fvec2(random_real_range(0.5), random_real_range(0.5)) + n * random_real_range(8 * smoothstep(1-abs(t-0.5)*2)), fvec2(step, 0.5))
                     .rotate(angle).alpha(random_real(0.5)).beta(random_real(1)).center().color(fvec3(1));
                }
            }

            // Static gems
            for (const auto &it : w.static_gems)
            {
                ivec2 pos = it.first;
                int index = it.second;
                if ((abs(pos * Map::tile_size + Map::tile_size/2 - w.cam.pos) > screen_sz/2 + Map::tile_size).any())
                    continue;
                r.Quad(pos * Map::tile_size - w.cam.pos, ivec2(Map::tile_size)).tex(ivec2(48+16*index,992)).alpha(random_real_range(0.9,1)).beta(random_real_range(0,1));
                r.Quad(pos * Map::tile_size + Map::tile_size/2 - w.cam.pos + iround(fvec2(random_real_range(0.6), random_real_range(0.6))), ivec2(Map::tile_size))
                 .tex(ivec2(48+16*index,992)).alpha(random_real_range(0.5,1)).beta(random_real_range(0,1)).rotate(random_real_range(0.1)).center();
            }

            // Moving gems
            for (const auto &it : w.moving_gems)
            {
                if ((abs(it.pos - w.cam.pos) > screen_sz/2 + Map::tile_size).any())
                    continue;
                r.Quad(it.pos - w.cam.pos, ivec2(Map::tile_size)).center().tex(ivec2(48+16*it.index,992)).alpha(random_real_range(0.9,1)).beta(random_real_range(0,1));
                r.Quad(it.pos - w.cam.pos + iround(fvec2(random_real_range(0.6), random_real_range(0.6))), ivec2(Map::tile_size))
                 .tex(ivec2(48+16*it.index,992)).alpha(random_real_range(0.5,1)).beta(random_real_range(0,1)).rotate(random_real_range(0.1)).center();
            }

            // Selection
            for (const auto &pos : w.selected_static_gems)
            {
                if ((abs(pos * Map::tile_size + Map::tile_size/2 - w.cam.pos) > screen_sz/2 + Map::tile_size).any())
                    continue;
                r.Quad(pos * Map::tile_size + Map::tile_size/2 - w.cam.pos, fvec2(random_real_range(2,4))).rotate(random_real_range(f_pi)).center().color(fvec3(1));
                r.Quad(pos * Map::tile_size + Map::tile_size/2 - w.cam.pos, fvec2(random_real_range(2,4))).rotate(random_real_range(f_pi)).center().color(fvec3(0));
            }
        }

        { // Objects
            if (w.obj.gun_exists)
                r.Quad(w.obj.gun_pos * Map::tile_size - w.cam.pos, ivec2(16)).tex(ivec2(32,0));
        }

        { // Goal
            if (w.finale)
            {
                r.Quad(w.goal_pos - w.cam.pos + iround(fvec2(random_real_range(0.6), random_real_range(0.6))), ivec2(80)-1)
                 .tex(ivec2(0,832)+1).alpha(random_real_range(0.5,1)).beta(random_real_range(0,1)).rotate(random_real_range(0.1)).center();
                r.Quad(w.goal_pos - w.cam.pos, ivec2(80)-1).tex(ivec2(0,832)+1).center();
            }
        }

        { // Player
            float alpha = 1;
            if (w.p.dead)
                alpha = max(0, 1 - w.p.death_timer / 8.f);

            // Body
            r.Quad(w.p.pos - w.cam.pos, ivec2(32,48)).tex(ivec2(256 + 32 * w.p.anim_frame, 48 * (w.p.anim_state + w.p.has_gun * 3))).center().flip_x(w.p.left).alpha(alpha);

            // Gun
            if (w.p.has_gun)
            {
                float angle;
                int state;
                if (w.p.aim % 2 == 0)
                {
                    angle = w.p.aim * f_pi / 4 * (w.p.left ? -1 : 1);
                    state = 0;
                }
                else
                {
                    angle = (w.p.aim-1) * f_pi / 4 * (w.p.left ? -1 : 1);
                    state = 1;
                }
                r.Quad(w.p.pos - w.cam.pos + w.p.gun_offset, ivec2(32)).tex(ivec2(0,state * 32)).center().flip_x(w.p.left).translate(ivec2(3.1 * (w.p.left ? 1 : -1),0.1)).rotate(angle).alpha(alpha);
            }
        }

        { // Enemy bullets
            for (const auto &it : w.enemy_bullets)
            {
                float angle = std::atan2(it.vel.y, it.vel.x);
                r.Quad(it.pos - w.cam.pos, ivec2(30,14)).center().rotate(angle).tex(ivec2(1,65+16)).alpha(1).beta(0.5);
            }
        }

        { // Bullets
            for (const auto &it : w.bullets)
            {
                float angle = std::atan2(it.vel.y, it.vel.x);
                r.Quad(it.pos - w.cam.pos, ivec2(30,14)).center().rotate(angle).tex(ivec2(1,65)).alpha(1).beta(0.5);
            }
        }

        { // Map front
            w.map.Render(r, screen_sz, w.cam.pos, Map::front);
        }

        { // Particles front
            for (const auto &it : w.particles_front)
            {
                float size = it.size * std::pow(1 - it.cur_frames / float(it.max_frames), it.size_power);
                r.Quad(it.pos - w.cam.pos, fvec2(size)).color(it.color).alpha(it.alpha).beta(it.beta).rotate(it.angle).center();
            }
        }

        { // Signs
            constexpr fvec3 color(0.5,0.5,0.5);
            constexpr float alpha = 0.5, beta = 0;

            auto Preset = [&](Renderers::Poly2D::Text_t &obj)
            {
                obj.color(color).alpha(alpha).beta(beta);
                obj.preset(Draw::WithColors({fvec3(1,1,1)}));
            };

            if (w.obj.help_move_pos != ivec2(-1))
                r.Text(w.obj.help_move_pos * Map::tile_size + Map::tile_size/2 - w.cam.pos, Str("Press \1", Keys::left.name(), "\r and \1", Keys::right.name(), "\r to move")).preset(Preset);
            if (w.obj.help_jump_pos != ivec2(-1))
                r.Text(w.obj.help_jump_pos * Map::tile_size + Map::tile_size/2 - w.cam.pos, Str("Press \1", Keys::z.name(), "\r to jump\nHold to jump higher")).preset(Preset);
            if (w.obj.help_gun_pos != ivec2(-1))
                r.Text(w.obj.help_gun_pos * Map::tile_size + Map::tile_size/2 - w.cam.pos, Str("Press \1", Keys::x.name(), "\r to shoot\nHold \1", Keys::up.name(), "\r or \1", Keys::down.name(), "\r to aim")).preset(Preset);
            if (w.obj.help_restart_pos != ivec2(-1))
                r.Text(w.obj.help_restart_pos * Map::tile_size + Map::tile_size/2 - w.cam.pos, Str("\nPress \1", Keys::escape.name(), "\r to return\nto the last checkpoint\nif you're stuck\nPress \1", Keys::m.name(), "\r to mute music")).preset(Preset);
            if (w.obj.help_gems_pos != ivec2(-1))
                r.Text(w.obj.help_gems_pos * Map::tile_size + Map::tile_size/2 - w.cam.pos, Str("Shoot gems to select them\nPress \1", Keys::c.name(), "\r to move selected gems\nHold \1Arrows\r to change direction")).preset(Preset);
            if (w.obj.help_gems_row_pos != ivec2(-1))
                r.Text(w.obj.help_gems_row_pos * Map::tile_size + Map::tile_size/2 - w.cam.pos, Str("Gems explode if placed in a row of three or longer")).preset(Preset);
            if (w.obj.help_dmg_pos != ivec2(-1))
                r.Text(w.obj.help_dmg_pos * Map::tile_size + Map::tile_size/2 - w.cam.pos, Str("Enemies become vulnerable once all\ngems of the corresponding color are destroyed")).preset(Preset);
        }

        { // Gem movement direction indicator
            if (w.selected_static_gems.size() > 0 && w.p.gem_dir != ivec2(0))
            {
                r.Quad(w.p.nearest_selected_gem - w.cam.pos, ivec2(48-2,16-2)).tex(ivec2(32,16)+1).rotate(std::atan2(w.p.gem_dir.y, w.p.gem_dir.x)).alpha(random_real_range(0.2,0.35)).beta(random_real_range(0,1)).center(ivec2(7));
            }
        }

        { // Messages
            // Interaction text
            r.Text(ivec2(0,screen_sz.y/2-32), w.message).color(fvec3(1,1,0.8)).beta(random_real(1)).preset(Draw::WithBlackOutline);

            // Press any key to respawn
            if (w.p.dead)
            {
                float alpha = clamp((w.p.death_timer - 60) / 60.f, 0, 1);
                r.Text(ivec2(0), "Press any key to respawn").color(fvec3(1,0.9,0.8)).beta(random_real(1)).alpha(alpha).preset(Draw::WithBlackOutline);
            }
        }

        { // Tint
            r.Quad(ivec2(0), screen_sz).tex(ivec2(1568,1080)).center();
        }

        { // Darkness
            if (w.darkness > 0)
                r.Quad(ivec2(0), screen_sz).color(fvec3(0)).center().alpha(w.darkness);
        }
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
            if (Keys::f11.pressed() || (Keys::l_alt.down() && Keys::enter.pressed()))
                win.ToggleFullscreen();
            if (win.size_changed)
            {
                win.size_changed = 0;
                Draw::Resize();
            }
            Tick();

            audio.CheckErrors();
            Audio::Source::RemoveUnused();
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
