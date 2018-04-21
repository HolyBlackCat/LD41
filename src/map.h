#ifndef MAP_H_INCLUDED
#define MAP_H_INCLUDED

#include <map>
#include <numeric>
#include <set>
#include <unordered_map>

#include "mat.h"
#include "random.h"
#include "reflection.h"
#include "renderers2d.h"
#include "strings.h"
#include "ui.h"


class Map
{
  public:
    static constexpr int tile_size = 16;

    static constexpr uint32_t version_magic = 5; // This should be changed when map binary structure changes.

    static constexpr ivec2 sheet_size = ivec2(32), sheet_tex_pos = ivec2(0,512);


    enum SafetyMode {Safe, Unsafe};


    using tile_id_t = int;
    inline static constexpr tile_id_t no_tile = tile_id_t(-1);

    ReflectStruct(Tile, (
        (tile_id_t)(front,mid,back)(=no_tile),
    ))


    using layer_mem_ptr_t = tile_id_t Tile::*;

    static constexpr layer_mem_ptr_t front = &Tile::front,
                                     mid   = &Tile::mid,
                                     back  = &Tile::back;

    static constexpr layer_mem_ptr_t layer_list[] {front, mid, back};
    static constexpr int layer_count = std::extent_v<decltype(layer_list)>;


    ReflectMemberEnum(LayerEnum, (la_front)(la_mid)(la_back)(num_layers))

    static_assert(layer_list[la_front] == front &&
                  layer_list[la_mid  ] == mid   &&
                  layer_list[la_back ] == back    );

    class Tiling
    {
      public:
        struct Group
        {
            Reflect(Group)
            (
                (std::string)(name),
                (std::vector<std::string>)(tiles),
                (std::vector<int> indices = {};),
            )

            void Finalize();
            bool Contains(std::string name) const;
            bool Contains(int index) const;

            explicit operator const std::string &() const {return name;}
            template <typename A, typename B, Group * = nullptr> friend bool operator< (const A &a, const B &b) {return static_cast<const std::string &>(a) <  static_cast<const std::string &>(b);}
            template <typename A, typename B, Group * = nullptr> friend bool operator==(const A &a, const B &b) {return static_cast<const std::string &>(a) == static_cast<const std::string &>(b);}
            // Dummy template parameter stops compiler from complaining about redefinitions of these functions in other structures.
        };

        struct TileVariant
        {
            Reflect(TileVariant)
            (
                (std::string)(name),
                (ivec2)(texture),
                (ivec2)(size)(=ivec2(1)),
                (ivec2)(offset)(=ivec2(0)),
            )

            int global_index;

          private:
            bool small;
            ivec2 effective_texture_pixel_pos,
                  effective_texture_pixel_size,
                  effective_texture_pixel_offset;
          public:
            void Finalize(std::string tile_name); // This is not const to prevent calling it from outside.

            ivec2 TexturePos() const;
            ivec2 TextureSize() const;
            ivec2 TextureOffset() const;
            bool Small() const;

            explicit operator const std::string &() const {return name;}
            template <typename A, typename B, TileVariant * = nullptr> friend bool operator< (const A &a, const B &b) {return static_cast<const std::string &>(a) <  static_cast<const std::string &>(b);}
            template <typename A, typename B, TileVariant * = nullptr> friend bool operator==(const A &a, const B &b) {return static_cast<const std::string &>(a) == static_cast<const std::string &>(b);}
            // Dummy template parameter stops compiler from complaining about redefinitions of these functions in other structures.
        };

        struct TileRule
        {
            ReflectStruct(Result, (
                (std::string)(name),
                (float)(chance)(=-1),
                (int index;), // Sic! We don't reflect this.
            ))
            ReflectStruct(Requirement, (
                (std::string)(name), // This can be `""`, which means 'any tile` (then `index == -1`, `is_group == 0`).
                (ivec2)(offset),
                (int index;), // Tile index or group index.
                (bool is_group;),
            ))
            ReflectStruct(ModuloPosition, (
                (ivec2)(size),
                (std::set<ivec2>)(offsets),
                (bool apply = 0;),
            ))

            ReflectStruct(Dupe, (
                (imat2)(matrix), // This matrix will applied to each offset of the new rule.
                (std::vector<Result>)(results)(={}), // If this is not empty, it will be used instead of the original rule list.
            ))

            Reflect(TileRule)
            (
                (int original_index;),
                (std::vector<Result>)(results),
                (std::vector<Requirement>)(requires,requires_not)(={}),
                (std::vector<std::string>)(req_variants)(={}), // The current tile must have one of those variants for the rule to work.
                (std::vector<imat2>)(matrices)(={}), // All requirements will be copied with these matrices applied to offsets (after handling `duplicate`).
                (ModuloPosition)(modulo_pos)(={ivec2(1), {}}), // The current tile position modulo `size` must be one of `offsets` for this rule to work.
                (std::vector<Dupe>)(duplicate)(={}), // This rule will be duplicated for each element of this vector.
                (std::vector<int> req_variant_indices = {};), // This will be sorted.
            )

            bool CanBeAppliedToVariant(int variant_index) const;

            void Finalize(std::string tile_name);
        };

        struct Tile
        {
            Reflect(Tile)
            (
                (std::string)(name),
                (std::set<std::string>)(flags)(={}),
                (LayerEnum)(layer),
                (std::string)(va_default,va_display),
                (std::vector<TileVariant>)(variants),
                (std::vector<TileRule>)(rules),
            )

            std::vector<char> flag_array; // `char` is used instead of `bool` because I don't want packing.

            int original_index;
            int va_default_index, va_display_index;


            bool HasFlag(int flag_index) const;

            int VariantIndex(std::string variant_name); // This is not const to prevent calling it from outside.

            const TileVariant &Variant(std::string variant_name) const;

            const TileVariant &DefaultVariant() const;
            const TileVariant &DisplayVariant() const;

            void Finalize(int index);

            explicit operator const std::string &() const {return name;}
            template <typename A, typename B, Tile * = nullptr> friend bool operator< (const A &a, const B &b) {return static_cast<const std::string &>(a) <  static_cast<const std::string &>(b);}
            template <typename A, typename B, Tile * = nullptr> friend bool operator==(const A &a, const B &b) {return static_cast<const std::string &>(a) == static_cast<const std::string &>(b);}
            // Dummy template parameter stops compiler from complaining about redefinitions of these functions in other structures.
        };

        struct Data
        {
            Reflect(Data)
            (
                (std::vector<std::string>)(flags),
                (std::vector<Group>)(groups),
                (std::vector<Tile>)(tiles),
            )

            std::vector<int> layer_tile_indices[num_layers];

            struct TileInfo
            {
                int tile_index;
                int variant_index;
                std::string tile_name;
                std::string variant_name;
            };
            std::map<tile_id_t, TileInfo> tile_info;

            std::map<std::string, std::map<std::string, tile_id_t>> indices_by_name;
            tile_id_t global_index_count;

            ivec2 autotiling_range;


            ivec2 max_texture_offset_negative = ivec2(std::numeric_limits<int>::max()),
                  max_texture_offset_positive = ivec2(std::numeric_limits<int>::min());


            bool GroupExists(std::string name) const;
            bool TileExists(std::string name) const;

            // Those return -1 if there is no such object.
            int TileFlagIndex(std::string name) const;
            int GroupIndex(std::string name) const;
            int TileIndex(std::string name) const;

            void Finalize();
        };

      private:
        Data data;

        std::string file_name;

      public:
        Tiling(std::string file_name);

        void Reload(bool fatal_errors = 0);

        int FlagIndex(std::string name) const; // This fails with a error if such flag doesn't exist.
        int TileIndex(std::string name) const; // This fails with a error if such tile doesn't exist.

        int IndexCount() const; // Valid tile_id_t values are: 0 <= x < IndexCount().

        tile_id_t IndexByName(std::string tile_name, std::string variant_name) const; // Returns -1 if no such tile or variant.

        int GetTileIndex(tile_id_t id) const;
        int GetVariantIndex(tile_id_t id) const;

        const Tile &GetTile(tile_id_t id) const;
        const TileVariant &GetVariant(tile_id_t id) const;

        const std::vector<TileRule> &GetTileRules(int tile_index) const;

        // To make sure all the large tile textures get into your camera, increase rendered tile range by those values (add them to top-left and bottom-right corners respectively).
        ivec2 MaxTextureOffsetNegative() const;
        ivec2 MaxTextureOffsetPositive() const;

        int TileCount() const;

        const Tile &TileByIndex(int index) const;
        const Group &GroupByIndex(int index) const;

        const TileVariant &DefaultVariant(int tile_index) const;

        const std::vector<int> &TileIndicesForLayer(LayerEnum layer) const;

        ivec2 AutotilingRange() const;
    };
    inline static Tiling tiling{"assets/tiling"};
    inline static int flag_solid = tiling.FlagIndex("solid");
    inline static int flag_invisible = tiling.FlagIndex("invisible");
    inline static int flag_kills = tiling.FlagIndex("kills");
    inline static int flag_changes_map = tiling.FlagIndex("changes map");
    inline static int flag_destructable = tiling.FlagIndex("destructable");

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

    ReflectStruct(ReflectedData, (
        (ivec2)(size),
        (std::vector<std::string>)(tile_names, variant_names),
        (std::vector<int>[layer_count])(layers),
    ))

  public:
    Map() {data.size = ivec2(0);}
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

        Program::Error(Str("Can't load map `", file_name, "`."));
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

    // `tile_pos` is used only for visibility check.
    template <typename F> static void DrawTile(Renderers::Poly2D &r, const Tiling::TileVariant &variant, ivec2 pos, ivec2 tile_pos, ivec2 first_visible, ivec2 last_visible, F &&func)
    {
        if ((tile_pos + variant.offset + variant.size <= first_visible).any())
            return;
        if ((tile_pos + variant.offset > last_visible).any())
            return;

        auto quad = r.Quad(pos + variant.TextureOffset(), variant.TextureSize()).tex(variant.TexturePos());
        func(quad);
    }

    void Render(Renderers::Poly2D &r, ivec2 screen_sz, ivec2 cam_pos, layer_mem_ptr_t layer) const
    {
        ivec2 first_visible = div_ex(cam_pos - screen_sz / 2, tile_size),
              last_visible  = div_ex(cam_pos + screen_sz / 2, tile_size);

        ivec2 first = first_visible + tiling.MaxTextureOffsetNegative(),
              last  = last_visible  + tiling.MaxTextureOffsetPositive();

        for (int i = 0; i < 2; i++)
        {
            bool small_tiles = (i == 0);

            for (int y = first.y; y <= last.y; y++)
            for (int x = first.x; x <= last.x; x++)
            {
                ivec2 pos = ivec2(x,y);

                tile_id_t id = Get(pos, layer);
                if (id == no_tile)
                    continue;

                if (tiling.GetTile(id).HasFlag(flag_invisible))
                    continue;

                const auto &variant = tiling.GetVariant(id);

                if (variant.Small() != small_tiles)
                    continue;

                DrawTile(r, variant, pos * tile_size - cam_pos, pos, first_visible, last_visible, [](Renderers::Poly2D::Quad_t &){});
            }
        }
    }

    bool AnyTileExistsAt(ivec2 pos) const
    {
        auto tile = Get(pos);
        for (int layer = 0; layer < num_layers; layer++)
            if (tile.*layer_list[layer] != no_tile)
                return 1;
        return 0;
    }
    bool TileExistsAt(int index, ivec2 pos) const
    {
        layer_mem_ptr_t layer = layer_list[tiling.TileByIndex(index).layer];
        tile_id_t tile_id = Get(pos, layer);
        if (tile_id == no_tile)
            return 0;
        return tiling.GetTileIndex(tile_id) == index;
    }
    bool TileFromGroupExistsAt(int group_index, ivec2 pos) const
    {
        const auto &group = tiling.GroupByIndex(group_index);
        auto tile = Get(pos);
        for (int layer = 0; layer < num_layers; layer++)
        {
            tile_id_t tile_id = tile.*layer_list[layer];
            if (tile_id != no_tile && group.Contains(tiling.GetTileIndex(tile_id)))
                return 1;
        }
        return 0;
    }
    bool TileWithFlagExistsAt(int flag_index, ivec2 pos) const
    {
        auto tile = Get(pos);
        for (int layer = 0; layer < num_layers; layer++)
        {
            tile_id_t tile_id = tile.*layer_list[layer];
            if (tile_id != no_tile && tiling.GetTile(tile_id).HasFlag(flag_index))
                return 1;
        }
        return 0;
    }

    bool CheckTileRequirement(const Tiling::TileRule::Requirement &req, ivec2 pos) // Offset mentioned in the requirement is added to `pos`.
    {
        pos += req.offset;
        if (req.is_group)
        {
            return TileFromGroupExistsAt(req.index, pos);
        }
        else
        {
            if (req.index == -1)
                return AnyTileExistsAt(pos);
            else
                return TileExistsAt(req.index, pos);
        }
    }

    void RunAutotilerForOneTile(ivec2 pos)
    {
        for (int layer_index = 0; layer_index < num_layers; layer_index++)
        {
            layer_mem_ptr_t mem_ptr = layer_list[layer_index];
            tile_id_t tile_id = Get(pos, mem_ptr);
            if (tile_id == no_tile)
                continue;
            int tile_index = tiling.GetTileIndex(tile_id);
            const auto &tile = tiling.TileByIndex(tile_index);

            int new_variant_index = tile.va_default_index;

            auto rules = tiling.GetTileRules(tile_index);
            for (const auto &rule : rules)
            {
                if (!rule.CanBeAppliedToVariant(new_variant_index))
                    continue;

                if (rule.modulo_pos.apply)
                {
                    if (rule.modulo_pos.offsets.count(pos % rule.modulo_pos.size) == 0) // We don't need `mod_ex` here, position will never be negative anyway.
                        continue;
                }

                bool ok = 1;
                for (const auto &req : rule.requires)
                {
                    if (!CheckTileRequirement(req, pos))
                    {
                        ok = 0;
                        break;
                    }
                }
                if (!ok)
                    continue;
                for (const auto &req_not : rule.requires_not)
                {
                    if (CheckTileRequirement(req_not, pos))
                    {
                        ok = 0;
                        break;
                    }
                }
                if (!ok)
                    continue;

                const auto &results = rule.results;

                if (results.size() == 1)
                {
                    new_variant_index = results[0].index;
                }
                else
                {
                    float r = random_real(1);

                    bool selected = 0;

                    for (std::size_t i = 0; i < results.size() - 1; i++) // Sic! We don't iterate over the last element to avoid potential precision errors if `r == 1`.
                    {
                        const auto &result = results[i];
                        if (r <= result.chance)
                        {
                            new_variant_index = result.index;
                            selected = 1;
                            break;
                        }
                        else
                        {
                            r -= result.chance;
                        }
                    }
                    if (!selected)
                        new_variant_index = results.back().index;
                }
            }

            Set(pos, mem_ptr, tile.variants[new_variant_index].global_index);
        }
    }
    void RunAutotiler(ivec2 pos, ivec2 size = ivec2(1)) // Runs autotiler for each tile in the specified rectange, expanded in every direction by `tiling.AutotilingRange()`.
    {
        ivec2 range = tiling.AutotilingRange();
        for (int y = -range.y; y < size.y + range.y; y++)
        for (int x = -range.x; x < size.x + range.x; x++)
            RunAutotilerForOneTile(pos + ivec2(x,y));
    }
    void RunAutotilerForEntireMap()
    {
        for (int y = 0; y <= data.size.y; y++)
        for (int x = 0; x <= data.size.x; x++)
            RunAutotilerForOneTile(ivec2(x,y));
    }

    const std::string &FileName() const
    {
        return file_name;
    }
    void SetFileName(std::string new_file_name)
    {
        file_name = new_file_name;
    }

    bool SaveToFile(bool forward_compat = 0, std::string suffix = "") const
    {
        ReflectedData refl;
        refl.size = data.size;
        for (tile_id_t tile_id = 0; tile_id < tiling.IndexCount(); tile_id++)
        {
            refl.tile_names.push_back(tiling.GetTile(tile_id).name);
            refl.variant_names.push_back(tiling.GetVariant(tile_id).name);
        }
        for (int la = 0; la < layer_count; la++)
        {
            auto &layer = refl.layers[la];
            layer.reserve(data.size.product());
            for (int y = 0; y < data.size.y; y++)
            for (int x = 0; x < data.size.x; x++)
                layer.push_back(data.tiles[x + data.size.x * y].*layer_list[la]);
        }

        if (forward_compat)
        {
            std::string str = Reflection::to_string(refl);
            return Utils::WriteToFile(file_name + ".fwdcompat" + suffix, (uint8_t *)str.data(), str.size());
        }
        else
        {
            auto len = sizeof(uint32_t) + Reflection::byte_buffer_size(refl);
            auto buf = std::make_unique<uint8_t[]>(len);
            uint8_t *ptr = Reflection::to_bytes<uint32_t>(version_magic, buf.get());
            ptr = Reflection::to_bytes(refl, ptr);
            if (ptr != buf.get() + len)
                return 0;
            return Utils::WriteToFile(file_name + suffix, buf.get(), len, Utils::compressed);
        }
    }
    bool LoadFromFile(bool forward_compat = 0)
    {
        Utils::MemoryFile file;
        try
        {
            if (forward_compat)
                file.Create(file_name + ".fwdcompat");
            else
                file.Create(file_name, Utils::compressed);
        }
        catch(decltype(Utils::file_input_error("","")) &e)
        {
            return 0;
        }

        ReflectedData refl;

        bool ok;
        if (forward_compat)
        {
            ok = Reflection::from_string(refl, (char *)file.Data()); // `MemoryFile::Data()` is null-terminated, so we're fine.
        }
        else
        {
            ok = 0;
            const uint8_t *begin = file.Data(), *end = file.Data() + file.Size();
            uint32_t magic;
            begin = Reflection::from_bytes<uint32_t>(magic, begin, end);
            if (begin && magic == version_magic)
            {
                begin = Reflection::from_bytes(refl, begin, end);
                if (begin == end)
                    ok = 1;
            }
        }

        if (!ok)
            return 0;

        if (refl.tile_names.size() != refl.variant_names.size())
            return 0;

        int indices_in_file = refl.tile_names.size();

        Data new_data; // There is no real need in operating on a copy for now, but we still do it.
        new_data.size = refl.size;
        new_data.tiles.resize(new_data.size.product());
        std::vector<tile_id_t> mapping;
        mapping.reserve(indices_in_file);
        for (int index = 0; index < indices_in_file; index++)
            mapping.push_back(tiling.IndexByName(refl.tile_names[index], refl.variant_names[index]));

        for (int la = 0; la < layer_count; la++)
        for (int y = 0; y < new_data.size.y; y++)
        for (int x = 0; x < new_data.size.x; x++)
        {
            int flat_xy = x + new_data.size.x * y;
            int old_index = refl.layers[la][flat_xy], new_index;
            if (old_index < 0 || old_index >= int(mapping.size()))
                new_index = -1;
            else
                new_index = mapping[old_index];
            new_data.tiles[flat_xy].*layer_list[la] = new_index;
        }

        data = std::move(new_data);

        return 1;
    }

    std::vector<ivec2> FindTiles(std::string tile_name)
    {
        int index = tiling.TileIndex(tile_name);
        std::vector<ivec2> ret;

        for (int y = 0; y < data.size.y; y++)
        for (int x = 0; x < data.size.x; x++)
        {
            ivec2 pos(x,y);
            if (TileExistsAt(index, pos))
                ret.push_back(pos);
        }
        return ret;
    }

    ivec2 FindSingleTile(std::string tile_name) // Fails with a error if there is 0 or more than 1 such tiles.
    {
        auto list = FindTiles(tile_name);
        if (list.size() != 1)
            Program::Error(Str("Expected a single tile `", tile_name, "` in map `", file_name, "`, but found ", list.size(), "."));
        return list[0];
    }

    ivec2 FindSingleTileOpt(std::string tile_name) // Fails with a error if there is more than 1 such tile.
    {
        auto list = FindTiles(tile_name);
        if (list.size() > 1)
            Program::Error(Str("Expected an optional single tile `", tile_name, "` in map `", file_name, "`, but found ", list.size(), "."));
        if (list.size() == 0)
            return ivec2(-1);
        return list[0];
    }
};

#endif
