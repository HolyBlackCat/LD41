#include "map.h"

void Map::Tiling::Group::Finalize()
{
    // Check for empty name
    if (name.empty())
        throw std::runtime_error("Attempt to create a tile group with an empty name.");

    // Sort
    std::sort(tiles.begin(), tiles.end());

    // Check for duplicates
    if (auto it = std::adjacent_find(tiles.begin(), tiles.end()); it != tiles.end())
        throw std::runtime_error(Str("Duplicate tile named `", *it, "` in group `", name, "`."));
}

bool Map::Tiling::Group::Contains(std::string name) const
{
    return std::binary_search(tiles.begin(), tiles.end(), name);
}
bool Map::Tiling::Group::Contains(int index) const
{
    return std::binary_search(indices.begin(), indices.end(), index);
}


void Map::Tiling::TileVariant::Finalize(std::string tile_name) // This is not const to prevent calling it from outside.
{
    if (name.empty())
        throw std::runtime_error(Str("Variant `", name, "` of tile `", tile_name, "` has empty name."));

    if ((size < 1).any())
        throw std::runtime_error(Str("Variant `", name, "` of tile `", tile_name, "` has non-positive size."));
    if ((texture < 0).any() || (texture + size > sheet_size).any())
        throw std::runtime_error(Str("Texture coordinates for variant `", name, "` of tile `", tile_name, "` are out of range."));

    small = (size == ivec2(1));
    effective_texture_pixel_pos    = sheet_tex_pos + (texture + offset) * tile_size;
    effective_texture_pixel_size   = size * tile_size;
    effective_texture_pixel_offset = offset * tile_size;
}
ivec2 Map::Tiling::TileVariant::TexturePos() const
{
    return effective_texture_pixel_pos;
}
ivec2 Map::Tiling::TileVariant::TextureSize() const
{
    return effective_texture_pixel_size;
}
ivec2 Map::Tiling::TileVariant::TextureOffset() const
{
    return effective_texture_pixel_offset;
}
bool Map::Tiling::TileVariant::Small() const
{
    return small;
}


bool Map::Tiling::TileRule::CanBeAppliedToVariant(int variant_index) const
{
    if (req_variant_indices.empty())
        return 1;
    else
        return std::binary_search(req_variant_indices.begin(), req_variant_indices.end(), variant_index);
}
void Map::Tiling::TileRule::Finalize(std::string tile_name)
{
    { // Check that result vector is not empty
        if (results.empty())
            throw std::runtime_error(Str("Result vector of rule ", original_index, " for tile `", tile_name, "` is empty."));
    }

    { // Fix chances
        float sum = 0;
        int need_init = 0;
        for (const auto &it : results)
        {
            if (it.chance >= 0)
                sum += it.chance;
            else
                need_init++;
        }
        if (sum > 1)
            throw std::runtime_error(Str("Results of the rule ", original_index, " for tile `", tile_name, "` have total probability greater than 1."));
        if (need_init > 0)
        {
            sum = (1 - sum) / need_init;
            for (auto &it : results)
                if (it.chance < 0)
                    it.chance = sum;
        }
    }

    { // Check modulo position settings
        if ((modulo_pos.size < 1).any())
            throw std::runtime_error(Str("Rectangle size for modulo position for the rule ", original_index, " for tile `", tile_name, "` is smaller than 1 in at least one dimension."));

        modulo_pos.apply = (modulo_pos.size != ivec2(1));

        if (modulo_pos.apply && modulo_pos.offsets.empty())
            throw std::runtime_error(Str("List of modulo offsets for the rule ", original_index, " for tile `", tile_name, "` is empty."));

        for (const auto &offset : modulo_pos.offsets)
            if ((offset < 0).any() || (offset >= modulo_pos.size).any())
                throw std::runtime_error(Str("Modulo offset ", offset," for the rule ", original_index, " for tile `", tile_name, "` is invalid."));
    }

    { // Copy requirements according to matrices
        for (auto mem_ptr : {&TileRule::requires, &TileRule::requires_not})
        {
            auto &vec = this->*mem_ptr;
            auto copy = vec;

            for (const auto &matrix : matrices)
            {
                auto tmp = copy; // Sic! We can't operate directly on the copy.
                for (auto &req : tmp)
                    req.offset = matrix /mul/ req.offset;
                vec.insert(vec.end(), tmp.begin(), tmp.end());
            }
        }
    }
}


bool Map::Tiling::Tile::HasFlag(int flag_index) const
{
    return flag_array[flag_index];
}
int Map::Tiling::Tile::VariantIndex(std::string variant_name) // This is not const to prevent calling it from outside.
{
    auto it = std::lower_bound(variants.begin(), variants.end(), variant_name);
    if (it == variants.end() || it->name != variant_name)
        return -1;
    return it - variants.begin();
}
const Map::Tiling::TileVariant &Map::Tiling::Tile::Variant(std::string variant_name) const
{
    auto it = std::lower_bound(variants.begin(), variants.end(), variant_name);
    if (it == variants.end() || it->name != variant_name)
        throw std::runtime_error(Str("Tile `", name, "` has no variant `", variant_name, "`."));
    return *it;
}
const Map::Tiling::TileVariant &Map::Tiling::Tile::DefaultVariant() const
{
    return variants[va_default_index];
}
const Map::Tiling::TileVariant &Map::Tiling::Tile::DisplayVariant() const
{
    return variants[va_display_index];
}
void Map::Tiling::Tile::Finalize(int index)
{
    // Assign index
    original_index = index;

    // Check for empty name
    if (name.empty())
        throw std::runtime_error("Attempt to create a tile with an empty name.");

    // Validate layer enum
    if (layer < 0 || layer >= num_layers)
        throw std::runtime_error(Str("Invalid layer enum value for tile `", name, "`."));

    { // Variants
        // Validate textures
        for (auto &it : variants)
            it.Finalize(name);
        // Sort
        std::sort(variants.begin(), variants.end());
        // Check for duplicates
        if (auto it = std::adjacent_find(variants.begin(), variants.end()); it != variants.end())
            throw std::runtime_error(Str("Duplicate variant `", it->name, "` for tile `", name, "`."));
        // Get indices for default/display variants
        va_default_index = VariantIndex(va_default);
        if (va_default_index == -1)
            throw std::runtime_error(Str("Default variant `", va_default, "` for tile `", name, "` doesn't exist."));
        va_display_index = VariantIndex(va_display);
        if (va_display_index == -1)
            throw std::runtime_error(Str("Display variant `", va_display, "` for tile `", name, "` doesn't exist."));
    }

    { // Rules
        { // Assign indices
            int index = 0;
            for (auto &it : rules)
                it.original_index = index++;
        }

        // Make duplicates (we do it before finalizing, because finalizing applies requirement matrices and checks result vectors)
        for (auto rule_it = rules.begin(); rule_it != rules.end();)
        {
            auto &original_rule = *rule_it++; // Sic! We want to increment it now to insert at the right place later.

            for (const auto &dupe : original_rule.duplicate)
            {
                rule_it = rules.insert(rule_it, original_rule);

                for (auto mem_ptr : {&TileRule::requires, &TileRule::requires_not})
                for (auto &req : (*rule_it).*mem_ptr)
                    req.offset = dupe.matrix /mul/ req.offset;

                if (dupe.results.size() > 0)
                    rule_it->results = dupe.results;

                rule_it++;
            }
        }

        // Finalize
        for (auto &it : rules)
            it.Finalize(name);

        // Get variant indices for results and required variants
        for (auto &rule : rules)
        {
            for (auto &result : rule.results)
            {
                result.index = VariantIndex(result.name);
                if (result.index == -1)
                    throw std::runtime_error(Str("A tiling rule result for tile `", name, "` references non-existent variant named `", result.name, "`."));
            }

            for (const auto &va_name : rule.req_variants)
            {
                int index = VariantIndex(va_name);
                if (index == -1)
                    throw std::runtime_error(Str("A tiling rule for tile `", name, "` references non-existent variant named `", va_name, "`."));
                rule.req_variant_indices.push_back(index);
            }
            std::sort(rule.req_variant_indices.begin(), rule.req_variant_indices.end());
        }
    }
}


bool Map::Tiling::Data::GroupExists(std::string name) const
{
    return std::binary_search(groups.begin(), groups.end(), name);
}
bool Map::Tiling::Data::TileExists(std::string name) const
{
    return std::binary_search(tiles.begin(), tiles.end(), name);
}

// Those return -1 if there is no such object.
int Map::Tiling::Data::TileFlagIndex(std::string name) const
{
    auto it = std::lower_bound(flags.begin(), flags.end(), name);
    if (it == flags.end() || *it != name)
        return -1;
    return it - flags.begin();
}
int Map::Tiling::Data::GroupIndex(std::string name) const
{
    auto it = std::lower_bound(groups.begin(), groups.end(), name);
    if (it == groups.end() || it->name != name)
        return -1;
    return it - groups.begin();
}
int Map::Tiling::Data::TileIndex(std::string name) const
{
    auto it = std::lower_bound(tiles.begin(), tiles.end(), name);
    if (it == tiles.end() || it->name != name)
        return -1;
    return it - tiles.begin();
}

void Map::Tiling::Data::Finalize()
{
    { // Tile flags
        // Sort
        std::sort(flags.begin(), flags.end());
        // Check for duplicates
        if (auto it = std::adjacent_find(flags.begin(), flags.end()); it != flags.end())
            throw std::runtime_error(Str("A duplicate tile flag name `", *it, "`."));
    }

    { // Groups
        // Finalize
        for (auto &it : groups)
            it.Finalize();
        // Sort
        std::sort(groups.begin(), groups.end());
        // Check for duplicates
        if (auto it = std::adjacent_find(groups.begin(), groups.end()); it != groups.end())
            throw std::runtime_error(Str("A duplicate tile group named `", it->name, "`."));
    }

    { // Tiles
        // Finalize
        int index = 0;
        for (auto &it : tiles)
            it.Finalize(index++);
        // Sort
        std::sort(tiles.begin(), tiles.end());
        // Check for duplicates
        if (auto it = std::adjacent_find(tiles.begin(), tiles.end()); it != tiles.end())
            throw std::runtime_error(Str("A duplicate tile named `", it->name, "`."));
        // Check for collision with group names
        for (const auto &it : tiles)
            if (GroupExists(it.name))
                throw std::runtime_error(Str("A name collision between a tile named `", it.name, "` and a group with the same name."));

        // Handle flags
        for (auto &it : tiles)
        {
            it.flag_array.resize(flags.size()); // This fills the vector with zeroes.

            for (const auto &flag : it.flags)
            {
                int flag_index = TileFlagIndex(flag);
                if (flag_index == 1)
                    throw std::runtime_error(Str("An invalid flag named `", flag, "` was specified for tile `", it.name, "`."));
                it.flag_array[flag_index] = 1;
            }
        }
    }

    { // Get tile indices for groups
        for (auto &group : groups)
        {
            for (const auto &name : group.tiles)
            {
                int index = TileIndex(name);
                if (index == -1)
                    throw std::runtime_error(Str("Tile named `", name, "` referenced in group `", group.name, "` doesn't exist."));
                group.indices.push_back(index);
            }

            std::sort(group.indices.begin(), group.indices.end());
        }
    }

    { // Generate tile IDs
        tile_id_t index = 0;

        for (std::size_t tile_index = 0; tile_index < tiles.size(); tile_index++)
        {
            auto &tile = tiles[tile_index];

            for (std::size_t variant_index = 0; variant_index < tile.variants.size(); variant_index++)
            {
                auto &variant = tile.variants[variant_index];
                variant.global_index = index++;
                indices_by_name[tile.name][variant.name] = variant.global_index;

                TileInfo info;
                info.tile_index = tile_index;
                info.variant_index = variant_index;
                info.tile_name = tile.name;
                info.variant_name = variant.name;
                tile_info.insert(std::make_pair(variant.global_index, info));
            }
        }

        global_index_count = index;
    }

    { // Get max texture offsets
        for (const auto &tile : tiles)
        {
            for (const auto &variant : tile.variants)
            {
                ivec2 a = variant.offset, b = variant.offset + variant.size - 1;
                if (a.x < max_texture_offset_negative.x) max_texture_offset_negative.x = a.x;
                if (a.y < max_texture_offset_negative.y) max_texture_offset_negative.y = a.y;
                if (b.x > max_texture_offset_positive.x) max_texture_offset_positive.x = b.x;
                if (b.y > max_texture_offset_positive.y) max_texture_offset_positive.y = b.y;
            }
        }
    }

    { // Obtain tile lists for specific layers
        for (int i = 0; i < num_layers; i++)
        {
            auto &vec = layer_tile_indices[i];
            int tile_index = 0;
            for (const auto &it : tiles)
            {
                if (it.layer == i)
                    vec.push_back(tile_index);
                tile_index++;
            }
            // Sort them back into the order they appeared in the file.
            std::sort(vec.begin(), vec.end(), [&](int a, int b){return tiles[a].original_index < tiles[b].original_index;});
        }
    }

    { // Get max autotiling range
        autotiling_range = ivec2(0);
        for (const auto &tile : tiles)
        for (const auto &rule : tile.rules)
        for (const auto *req_list : {&rule.requires, &rule.requires_not})
        for (const auto &req : *req_list)
        {
            ivec2 range = abs(req.offset);
            if (autotiling_range.x < range.x) autotiling_range.x = range.x;
            if (autotiling_range.y < range.y) autotiling_range.y = range.y;
        }
    }

    { // Get indices for autotiling requirements
        for (auto &tile : tiles)
        for (auto &rule : tile.rules)
        for (auto *req_list : {&rule.requires, &rule.requires_not})
        for (auto &req : *req_list)
        {
            req.index = TileIndex(req.name);
            req.is_group = 0;

            if (req.index == -1)
            {
                if (req.name == "")
                    continue;

                req.index = GroupIndex(req.name);
                req.is_group = 1;

                if (req.index == -1)
                    throw std::runtime_error(Str("Autotiling rule for tile `", tile.name, "` references `", req.name, "`, which is neither a tile nor a tile group."));
            }
        }
    }
}


Map::Tiling::Tiling(std::string file_name) : file_name(file_name)
{
    Reload(1);
}
void Map::Tiling::Reload(bool fatal_errors)
{
    Data data_copy = data;
    data = {};

    Utils::MemoryFile file(file_name);

    try
    {
        std::string error_message;
        if (auto ptr = Reflection::from_string(data, (char *)file.Data(), &error_message); ptr != (char *)file.Data() + file.Size())
            throw std::runtime_error(Str("Unable to parse tiling settings:\n", (ptr == 0 ? error_message : "Extra data at the end of input.")));

        data.Finalize();
    }
    catch (std::runtime_error &e)
    {
        if (fatal_errors)
            Program::Error(e.what());

        UI::MessageBox("Error!", e.what(), UI::warning);
        data = data_copy;
    }
}
int Map::Tiling::FlagIndex(std::string name) const // This fails with a error if such flag doesn't exist.
{
    auto it = std::lower_bound(data.flags.begin(), data.flags.end(), name);
    if (it == data.flags.end() || *it != name)
        Program::Error(Str("Attempt to access non-existent tile flag `", name, "`."));
    return it - data.flags.begin();
}
int Map::Tiling::IndexCount() const // Valid tile_id_t values are: 0 <= x < IndexCount().
{
    return data.global_index_count;
}
Map::tile_id_t Map::Tiling::IndexByName(std::string tile_name, std::string variant_name) const // Returns -1 if no such tile or variant.
{
    auto tile_it = data.indices_by_name.find(tile_name);
    if (tile_it == data.indices_by_name.end())
        return -1;
    auto variant_it = tile_it->second.find(variant_name);
    if (variant_it == tile_it->second.end())
        return -1;
    return variant_it->second;
}
int Map::Tiling::GetTileIndex(tile_id_t id) const
{
    auto it = data.tile_info.find(id);
    if (it == data.tile_info.end())
        Program::Error(Str("Attempt to get tile index for id ", ivec2(id), " which doesn't exist."));
    return it->second.tile_index;
}
int Map::Tiling::GetVariantIndex(tile_id_t id) const
{
    auto it = data.tile_info.find(id);
    if (it == data.tile_info.end())
        Program::Error(Str("Attempt to get tile variant index for id ", ivec2(id), " which doesn't exist."));
    return it->second.variant_index;
}
const Map::Tiling::Tile &Map::Tiling::GetTile(tile_id_t id) const
{
    auto it = data.tile_info.find(id);
    if (it == data.tile_info.end())
        Program::Error(Str("Attempt to get tile for id ", ivec2(id), " which doesn't exist."));
    return data.tiles[it->second.tile_index];
}
const Map::Tiling::TileVariant &Map::Tiling::GetVariant(tile_id_t id) const
{
    auto it = data.tile_info.find(id);
    if (it == data.tile_info.end())
        Program::Error(Str("Attempt to get tile variant for id ", ivec2(id), " which doesn't exist."));
    return data.tiles[it->second.tile_index].variants[it->second.variant_index];
}
const std::vector<Map::Tiling::TileRule> &Map::Tiling::GetTileRules(int tile_index) const
{
    return data.tiles[tile_index].rules;
}

// To make sure all the large tile textures get into your camera, increase rendered tile range by those values (add them to top-left and bottom-right corners respectively).
ivec2 Map::Tiling::MaxTextureOffsetNegative() const
{
    return data.max_texture_offset_negative;
}
ivec2 Map::Tiling::MaxTextureOffsetPositive() const
{
    return data.max_texture_offset_positive;
}

int Map::Tiling::TileCount() const
{
    return data.tiles.size();
}
const Map::Tiling::Tile &Map::Tiling::TileByIndex(int index) const
{
    return data.tiles[index];
}
const Map::Tiling::Group &Map::Tiling::GroupByIndex(int index) const
{
    return data.groups[index];
}
const Map::Tiling::TileVariant &Map::Tiling::DefaultVariant(int tile_index) const
{
    return Map::tiling.TileByIndex(tile_index).DefaultVariant();
}
const std::vector<int> &Map::Tiling::TileIndicesForLayer(LayerEnum layer) const
{
    return data.layer_tile_indices[layer];
}
ivec2 Map::Tiling::AutotilingRange() const
{
    return data.autotiling_range;
}
