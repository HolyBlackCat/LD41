#ifndef GRAPHICS_H_INCLUDED
#define GRAPHICS_H_INCLUDED

#include <algorithm>
#include <bitset>
#include <cstddef>
#include <ios>
#include <string>
#include <type_traits>
#include <vector>
#include <utility>

#include <GLFL/glfl.h>
#include <stb_image.h>
#include <stb_image_write.h>
#include <ft2build.h>
#include FT_FREETYPE_H // Ugh.

#include "exceptions.h"
#include "platform.h"
#include "program.h"
#include "reflection.h"
#include "template_utils.h"
#include "utils.h"

/* GLSL version chart:
    1.10.59		2.0		April 2004		#version 110
    1.20.8		2.1		September 2006	#version 120
    1.30.10		3.0		August 2008		#version 130
    1.40.08		3.1		March 2009		#version 140
    1.50.11		3.2		August 2009		#version 150
    3.30.6		3.3		February 2010	#version 330
    4.00.9		4.0		March 2010		#version 400
    4.10.6		4.1		July 2010		#version 410
    4.20.11		4.2		August 2011		#version 420
    4.30.8		4.3		August 2012		#version 430
    4.40		4.4		July 2013		#version 440
    4.50		4.5		August 2014		#version 450
    1.00        ES 2                    #version 100

    GLSL ES can be tested with `#ifdef GL_ES`.
    GLSL ES lacks default precision for float inside of fragment shaders.
*/

namespace Graphics
{
    inline namespace Exceptions
    {
        DefineExceptionBase(exception)

        DefineExceptionInline(cant_create_gl_resource, :exception, "Can't create an OpenGL resource.",
            (std::string,type,"Type")
        )
        DefineExceptionInline(gl_error, :exception, "OpenGL error.",
            (std::string,message,"Message")
        )
        DefineExceptionInline(no_free_texture_slots, :exception, "Out of free texture slots.",)

        DefineExceptionInline(cant_init_freetype, :exception, "Can't initialize Freetype.",)

        DefineExceptionInline(cant_parse_font, :exception, "Unable to parse a font.",
            (std::string,name,"Name")
        )
        DefineExceptionInline(bad_font_size, :exception, "Invalid font size.",
            (ivec2,size,"Specified size")
            (std::string,available,"Available sizes")
        )
        DefineExceptionInline(cant_render_glyph, :exception, "Unable to render a glyph.",
            (unsigned int,code,"Code")
            (std::string,reason,"Reason")
        )

        DefineExceptionInline(not_enough_texture_atlas_space, :exception, "Out of space for a font atlas.",
            (ivec2,pos,"Position")
            (ivec2,size,"Size")
        )

        DefineExceptionInline(cant_load_image, :exception, "Can't load image.",
            (std::string,name,"Name")
        )

        DefineExceptionInline(shader_compilation_error, :exception, "Shader compilation error.",
            (std::string,vertex_status,"Vertex status")
            (std::string,fragment_status,"Fragment status")
            (std::string,vertex_log,"Vertex log")
            (std::string,fragment_log,"Fragment log")
        )
        DefineExceptionInline(shader_linking_error, :exception, "Shader linking error.",
            (std::string,fragment_log,"Log")
        )
    }

    inline void CheckErrors()
    {
        GLenum err = glGetError();
        if (err)
        {
            std::string msg;
            do
            {
                if (msg.size()) msg += ", ";
                switch (err)
                {
                    case GL_INVALID_ENUM:                  msg += "Invalid enum";                  break;
                    case GL_INVALID_VALUE:                 msg += "Invalid value";                 break;
                    case GL_INVALID_OPERATION:             msg += "Invalid operation";             break;
                    case GL_INVALID_FRAMEBUFFER_OPERATION: msg += "Invalid framebuffer operation"; break;
                    case GL_OUT_OF_MEMORY:                 msg += "Out of memory";                 break;
                    #ifdef GL_STACK_UNDERFLOW
                    case GL_STACK_UNDERFLOW:               msg += "Stack underflow";               break;
                    #endif
                    #ifdef GL_STACK_OVERFLOW
                    case GL_STACK_OVERFLOW:                msg += "Stack overflow";                break;
                    #endif
                    default:                               msg += "Unknown error";                 break;
                }
            }
            while ((err = glGetError()));
            throw gl_error(msg + '.');
        }
    }

    inline namespace Misc
    {
        enum ClearBits
        {
            color   = GL_COLOR_BUFFER_BIT,
            depth   = GL_DEPTH_BUFFER_BIT,
            stencil = GL_STENCIL_BUFFER_BIT,
        };
        inline ClearBits operator|(ClearBits a, ClearBits b) {return ClearBits(a | b);}

        inline void Clear(ClearBits bits)
        {
            glClear(bits);
        }


        namespace Blending
        {
            enum Factors
            {
                zero                 = GL_ZERO,
                one                  = GL_ONE,
                src                  = GL_SRC_COLOR,
                one_minus_src        = GL_ONE_MINUS_SRC_COLOR,
                dst                  = GL_DST_COLOR,
                one_minus_dst        = GL_ONE_MINUS_DST_COLOR,
                src_a                = GL_SRC_ALPHA,
                one_minus_src_a      = GL_ONE_MINUS_SRC_ALPHA,
                dst_a                = GL_DST_ALPHA,
                one_minus_dst_a      = GL_ONE_MINUS_DST_ALPHA,
                constant             = GL_CONSTANT_COLOR,
                one_minus_constant   = GL_ONE_MINUS_CONSTANT_COLOR,
                constant_a           = GL_CONSTANT_ALPHA,
                one_minus_constant_a = GL_ONE_MINUS_CONSTANT_ALPHA,
                src_a_saturate       = GL_SRC_ALPHA_SATURATE,
                OnPC
                (
                    src1             = GL_SRC1_COLOR,
                    one_minus_src1   = GL_ONE_MINUS_SRC1_COLOR,
                    src1_a           = GL_SRC1_ALPHA,
                    one_minus_src1_a = GL_ONE_MINUS_SRC1_ALPHA,
                )
            };
            enum Equations
            {
                eq_add              = GL_FUNC_ADD,
                eq_subtract         = GL_FUNC_SUBTRACT,
                eq_reverse_subtract = GL_FUNC_REVERSE_SUBTRACT,
                OnPC
                (
                    eq_min          = GL_MIN,
                    eq_max          = GL_MAX,
                )
            };

            // Func(a,b) and Equation(a) set same parameters for both color and alpha blending.
            // Func(a,b,c,d) and Equation(a,b) set same parameters for color and alpha blending separately.
            inline void Enable() {glEnable(GL_BLEND);}
            inline void Disable() {glDisable(GL_BLEND);}
            inline void Func(Factors src, Factors dst)                             {glBlendFunc(src, dst);}
            inline void Func(Factors src, Factors dst, Factors srca, Factors dsta) {glBlendFuncSeparate(src, dst, srca, dsta);}
            inline void Equation(Equations eq) {glBlendEquation(eq);}
            inline void Equation(Equations eq, Equations eqa) {glBlendEquationSeparate(eq, eqa);}

            inline void FuncOverwrite     () {Func(one, zero);}
            inline void FuncNormalSimple  () {Func(src_a, one_minus_src_a);} // Resulting alpha is incorrect.
            inline void FuncNormalRawToPre() {Func(src_a, one_minus_src_a, one, one_minus_src_a);} // Output is premultiplied.
            inline void FuncNormalPre     () {Func(one, one_minus_src_a);} // Source and and output are premultiplited
        }
    }

    class CharMap
    {
      public:
        struct Char
        {
            ivec2 tex_pos = {0,0}, size = {0,0}, offset = {0,0};
            int advance = 0;
        };
      private:
        static constexpr int pack_size = 256;
        static_assert(0x10000 % pack_size == 0);

        struct CharPack
        {
            std::bitset<pack_size> available; // Filled with zeroes by default.
            std::vector<Char> glyphs;
        };

        int height = 0, ascent = 0, line_skip = 0;
        bool enable_line_gap = 1;
        std::vector<CharPack> data{0x10000 / pack_size};
      public:
        CharMap() {Set(0xffff, {});}
        void Set(uint16_t index, const Char &glyph)
        {
            auto &sub_vec = data[index / pack_size];
            sub_vec.available[index % pack_size] = 1;
            if (sub_vec.glyphs.empty())
                sub_vec.glyphs.resize(pack_size);
            sub_vec.glyphs[index % pack_size] = glyph;
        }
        bool Available(uint16_t index) const
        {
            return data[index / pack_size].available[index % pack_size];
        }
        const Char &Get(uint16_t index) const // If no glyph with such index is found, returns 0xffff'th glyph.
        {
            if (!Available(index))
                return GetDefault();
            return data[index / pack_size].glyphs[index % pack_size];
        }
        const Char &GetDefault() const // Returns 0xffff'th glyph.
        {
            return Get(0xffff);
        }

        void SetMetrics(int new_height, int new_ascent, int new_line_skip)
        {
            height = new_height;
            ascent = new_ascent;
            line_skip = new_line_skip;
        }
        void EnableLineGap(bool new_enable_line_gap) // If enabled (default), LineSkip() returns height instead.
        {
            enable_line_gap = new_enable_line_gap;
        }
        int Height() const {return height;}
        int Ascent() const {return ascent;}
        int Descent() const {return height-ascent;}
        int LineSkip() const {return enable_line_gap ? line_skip : height;}
    };

    class Font
    {
        struct FreetypeFontFuncs
        {
            template <typename> friend class ::Utils::Handle;
            static FT_Face Create(const std::string &/*display_name*/, const void *data, std::size_t data_size, int font_index)
            {
                static FT_Library lib = 0;
                if (!lib)
                {
                    if (FT_Init_FreeType(&lib))
                        throw cant_init_freetype();
                }

                FT_Open_Args args{};
                args.flags = FT_OPEN_MEMORY;
                args.memory_base = (decltype(args.memory_base))data;
                args.memory_size = data_size;

                FT_Face ret;
                if (FT_Open_Face(lib, &args, font_index, &ret))
                    return 0;

                return ret;
            }
            static void Destroy(FT_Face value)
            {
                FT_Done_Face(value);
            }
            static void Error(const std::string &display_name, const void *, std::size_t, int)
            {
                throw cant_parse_font(display_name);
            }
        };
        using FreetypeFont = Utils::Handle<FreetypeFontFuncs>;

        Utils::MemoryFile file;
        FreetypeFont ft_font;

      public:
        Font() {}
        Font(std::string fname, int index = 0)
        {
            Create(fname, index);
        }
        void Create(std::string fname, int index = 0)
        {
            Utils::MemoryFile new_file(fname);
            FreetypeFont new_tt_font({fname, new_file.Data(), new_file.Size(), index});
            file    = std::move(new_file);
            ft_font = std::move(new_tt_font);
        }
        void Destroy()
        {
            file.Destroy();
            ft_font.destroy();
        }
        bool Exists() const
        {
            return bool(ft_font);
        }

        void SetSize(int pixel_height, int pixel_width = 0)
        {
            DebugAssert("Attempt to set size for a null font.", ft_font);
            if (FT_Set_Pixel_Sizes(*ft_font, pixel_width, pixel_height))
            {
                std::string available_sizes;
                if (ft_font.value()->num_fixed_sizes)
                {
                    for (int i = 0; i < ft_font.value()->num_fixed_sizes; i++)
                    {
                        if (i != 0)
                            available_sizes += ',';
                        available_sizes += Reflection::Interface::to_string(ivec2(ft_font.value()->available_sizes[i].width,
                                                                                  ft_font.value()->available_sizes[i].height));
                    }
                }
                else
                    available_sizes = "?";
                throw bad_font_size(ivec2(pixel_width, pixel_height), available_sizes);
            }
        }

        int Ascent() const
        {
            return (*ft_font)->size->metrics.ascender >> 6; // It's stored in fixed point format (supposed to be already rounded) and we truncate it.
        }
        int Descent() const
        {
            return -((*ft_font)->size->metrics.descender >> 6); // Sic, we negate the result.
        }
        int Height() const
        {
            return Ascent() + Descent();
        }
        int LineSkip() const
        {
            return (*ft_font)->size->metrics.height >> 6; // Ha, freetype calls it 'height'.
        }

        bool HasChar(unsigned int ch) const
        {
            return (bool)FT_Get_Char_Index(*ft_font, ch);
        }

        enum RenderMode
        {
            normal     = FT_LOAD_TARGET_NORMAL,
            light      = FT_LOAD_TARGET_LIGHT,
            monochrome = FT_LOAD_TARGET_MONO,
        };

        struct CharInfo
        {
            unsigned char *data;
            int pitch; // Difference between nearest row addresses.
            bool monochrome; // If this is not set, the image uses 1 byte per pixel, otherwise it is 1 bit per pixel (most significant bits represent leftmost pixels).
            ivec2 size, offset;
            int advance;

            unsigned char GetPixel(ivec2 pos)
            {
                if (monochrome)
                    return ((data[pos.y * pitch + pos.x / 8] >> (7 - pos.x % 8)) & 1) ? 255 : 0;
                else
                    return data[pos.y * pitch + pos.x];
            }
        };

        CharInfo Render(unsigned int ch, RenderMode mode) // Freetype caches the last rendered glyph for each font. After you render another one, the returned image reference is no longer valid.
        {
            if (FT_Load_Char(*ft_font, ch, FT_LOAD_RENDER | mode))
                throw cant_render_glyph(ch, "Unknown.");
            auto glyph = (*ft_font)->glyph;
            auto bitmap = glyph->bitmap;
            if (mode == monochrome ? bitmap.pixel_mode != FT_PIXEL_MODE_MONO : bitmap.pixel_mode != FT_PIXEL_MODE_GRAY || bitmap.num_grays != 256)
                throw cant_render_glyph(ch, "Unexpected pixel format.");
            CharInfo ret;
            ret.data = bitmap.buffer;
            ret.pitch = bitmap.pitch;
            ret.monochrome = (mode == monochrome);
            ret.size = ivec2(bitmap.width, bitmap.rows);
            ret.offset = ivec2(glyph->bitmap_left, -glyph->bitmap_top);
            ret.advance = (glyph->advance.x + (1 << 5)) >> 6; // The last 6 bits of `bitmap.advance` represent fractional part, here we perform poor man's rounding.
            return ret;
        }
    };

    class Image
    {
        std::vector<u8vec4> data;
        int width = 0;
      public:
        enum Format {png, tga};

        Image() {}
        Image(ivec2 size, const char *ptr = 0)
        {
            FromMemory(size, ptr);
        }
        Image(std::string fname, bool flip_y = 0)
        {
            FromFile(fname, flip_y);
        }
        ivec2 Size() const {return {width, int(data.size()) / width};}
        const u8vec4 *Data() const {return data.data();}
        u8vec4 At(ivec2 pos) const
        {
            if ((pos < 0).any() || (pos >= Size()).any())
                return {0,0,0,0};
            return data[pos.x + pos.y * width];
        }
        u8vec4 &At(ivec2 pos)
        {
            if ((pos < 0).any() || (pos >= Size()).any())
            {
                static u8vec4 ret;
                ret = {0,0,0,0};
                return ret;
            }
            return data[pos.x + pos.y * width];
        }
        void Fill(ivec2 pos, ivec2 size, u8vec4 color)
        {
            pos = clamp(pos, 0, Size() - 1);
            size = min(pos + size, Size());
            for (int y = pos.y; y < size.y; y++)
            for (int x = pos.y; x < size.x; x++)
                data[x + y * width] = color;
        }
        void Fill(u8vec4 color)
        {
            for (auto &it : data)
                it = color;
        }

        void FromMemory(ivec2 size, const char *ptr = 0)
        {
            width = size.x;
            data.resize(size.product());
            if (ptr)
                std::copy(ptr, ptr + size.product() * sizeof(u8vec4), (char *)data.data());
        }
        void FromFile(std::string fname, bool flip_y = 0)
        {
            stbi_set_flip_vertically_on_load(flip_y); // This just sets an internal flag, shouldn't be slow.
            ivec2 size;
            [[maybe_unused]] int components;
            char *ptr = (char *)stbi_load(fname.c_str(), &size.x, &size.y, &components, 4);
            if (!ptr)
            {
                width = 0;
                throw cant_load_image(fname);
            }
            FromMemory(size, ptr);
            stbi_image_free(ptr);
        }
        void Destroy()
        {
            data = {};
            width = 0;
        }
        bool Exists() const
        {
            return data.size() != 0;
        }

        void SaveToFile(Format format, std::string fname)
        {
            switch (format)
            {
              case png:
                stbi_write_png(fname.c_str(), Size().x, Size().y, 4, data.data(), width * sizeof(u8vec4));
                return;
              case tga:
                stbi_write_tga(fname.c_str(), Size().x, Size().y, 4, data.data());
                return;
            }
        }

        /*
        struct AtlasFont
        {
            const TruetypeFont &ttfont;
            Font &font;
            int size; // If non-negative, represents full height measured in pixels. Otherwise represents 'point size' (supposedly height of letter 'M' in pixels).
            int index;
            Utils::Range<const int, Utils::contiguous> range; // Chars must fit in [0,0xffff], otherwise bad stuff will happen.

            AtlasFont(const TruetypeFont &ttfont, Font &font, int size, int index, decltype(range) range)
                : ttfont(ttfont), font(font), size(size), index(index), range(range) {}
        };

        void CreateFontAtlas(ivec2 pos, ivec2 size, Utils::Range<const AtlasFont> fonts, int alpha_threshold = -1) // Set `alpha_threshold` to -1 to do antialiasing. Otherwise any alpha smaller than this becomes 0, and 255 otherwise.
        {
            if ((pos < 0).any() || (pos + size >= Size()).any())
                throw cant_init_font_atlas("Specified region doesn't fit into the image.", pos, size);

            std::vector<unsigned char> bitmap(size.product());

            stbtt_pack_context pack_con;
            if (!stbtt_PackBegin(&pack_con, bitmap.data(), size.x, size.y, size.x * sizeof(char), 1, nullptr))
                throw cant_init_font_atlas("Unable to init packer.", pos, size);

            for (const auto &it : fonts)
            {
                std::size_t char_count = it.range.size();
                const int *char_ptr = &*it.range.begin();

                stbtt_pack_range range{};
                range.font_size = it.size;
                range.array_of_unicode_codepoints = const_cast<int *>(char_ptr); // No idea why the stbtt doesn't use a const pointer.
                range.num_chars = char_count;

                std::vector<stbtt_packedchar> results(char_count);
                range.chardata_for_range = results.data();

                if (!stbtt_PackFontRanges(&pack_con, (unsigned char *)it.ttfont.Data(), it.index, &range, 1))
                {
                    stbtt_PackEnd(&pack_con);
                    throw not_enough_texture_atlas_space(pos, size);
                }

                for (std::size_t i = 0; i < char_count; i++)
                {
                    const auto &ref = results[i];
                    Font::Glyph glyph;
                    glyph.tex_pos = ivec2(ref.x0, ref.y0) + pos;
                    glyph.size = decltype(glyph.size)(ref.x1 - ref.x0, ref.y1 - ref.y0);
                    glyph.offset = iround(fvec2(ref.xoff, ref.yoff));
                    glyph.advance = iround(ref.xadvance);
                    it.font.Set(char_ptr[i], glyph);
                }
            }

            stbtt_PackEnd(&pack_con);

            for (int y = 0; y < size.y; y++)
            for (int x = 0; x < size.x; x++)
            {
                if (alpha_threshold == -1)
                    data[pos.x + x + (pos.y + y) * width] = u8vec4(255, 255, 255, bitmap[x + y * size.x]);
                else
                    data[pos.x + x + (pos.y + y) * width] = u8vec4(255, 255, 255, (bitmap[x + y * size.x] < alpha_threshold ? 0 : 255));
            }
        }*/
    };

    class Texture
    {
        class HandleFuncs
        {
            template <typename> friend class ::Utils::Handle;
            static GLuint Create() {GLuint value; glGenTextures(1, &value); return value;}
            static void Destroy(GLuint value) {glDeleteTextures(1, &value);}
            static void Error() {throw cant_create_gl_resource("Texture");}
        };
        using Handle = Utils::Handle<HandleFuncs>;

        using SlotAllocator = Utils::ResourceAllocator<int, int>;
        inline static SlotAllocator slot_allocator{64};

        Handle handle;
        int slot = SlotAllocator::not_allocated;

        ivec2 size{};

      public:
        static void SetActiveSlot(int slot) // You don't usually need to call this manually.
        {
            static int old_slot = 0;
            if (slot == old_slot)
                return;
            old_slot = slot;
            glActiveTexture(GL_TEXTURE0 + slot);
        }

        enum InterpMode
        {
            nearest,
            linear,
            min_nearest_mag_linear,
            min_linear_mag_nearest,
        };
        enum WrapMode
        {
            clamp  = GL_CLAMP_TO_EDGE,
            mirror = GL_MIRRORED_REPEAT,
            repeat = GL_REPEAT,
            fill   = OnPC(GL_CLAMP_TO_BORDER) OnMobile(GL_CLAMP_TO_EDGE),
        };

        Texture() {}
        Texture(Texture &&) = default;
        Texture &operator=(Texture &&) = default;

        Texture(InterpMode mode, ivec2 img_size, const u8vec4 *data = 0) // Creates and attaches the texture.
        {
            Create();
            SetData(img_size, data);
            Interpolation(mode);
        }
        Texture(const Image &img, InterpMode mode) // Creates and attaches the texture.
        {
            Create();
            SetData(img);
            Interpolation(mode);
        }

        void Create()
        {
            handle.create({});
        }
        void Destroy()
        {
            Detach();
            handle.destroy();
        }
        bool Exists() const
        {
            return bool(handle);
        }

        void Attach() // The texture must be created first. Consecutive calls just activate a slot to which the texture was attached.
        {
            DebugAssert("Attempt to attach a null texture.", *handle);
            if (slot != SlotAllocator::not_allocated) // Already attached.
            {
                SetActiveSlot(slot);
                return;
            }
            slot = slot_allocator.alloc();
            if (slot == SlotAllocator::not_allocated)
                throw no_free_texture_slots();
            SetActiveSlot(slot);
            glBindTexture(GL_TEXTURE_2D, *handle);
        }
        void Detach()
        {
            if (slot == SlotAllocator::not_allocated)
                return; // Not attached.
            SetActiveSlot(slot);
            glBindTexture(GL_TEXTURE_2D, 0);
            slot_allocator.free(slot);
            slot = SlotAllocator::not_allocated;
        }
        bool Attached() const
        {
            return slot != SlotAllocator::not_allocated;
        }

        int Slot() const
        {
            return slot;
        }

        void SetData(ivec2 img_size, const u8vec4 *data = 0) // Attaches the texture.
        {
            Attach();
            size = img_size;
            glTexImage2D(GL_TEXTURE_2D, 0, OnPC(GL_RGBA8) OnMobile(GL_RGBA), size.x, size.y, 0, GL_RGBA, GL_UNSIGNED_BYTE, data);
        }
        void SetData(const Image &img) // Attaches the texture.
        {
            SetData(img.Size(), img.Data());
        }

        void SetDataPart(ivec2 pos, ivec2 size, const u8vec4 *data) // Attaches the texture.
        {
            Attach();
            glTexSubImage2D(GL_TEXTURE_2D, 0, pos.x, pos.y, size.x, size.y, GL_RGBA, GL_UNSIGNED_BYTE, data);
        }
        void SetDataPart(ivec2 pos, const Image &img) // Attaches the texture.
        {
            SetDataPart(pos, img.Size(), img.Data());
        }

        void Interpolation(InterpMode mode) // Attaches the texture.
        {
            Attach();
            GLenum min_mode, mag_mode;
            if (mode == nearest || mode == min_nearest_mag_linear)
                min_mode = GL_NEAREST;
            else
                min_mode = GL_LINEAR;
            if (mode == nearest || mode == min_linear_mag_nearest)
                mag_mode = GL_NEAREST;
            else
                mag_mode = GL_LINEAR;

            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, min_mode);
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, mag_mode);
        }

        void WrapX(WrapMode mode) // Attaches the texture.
        {
            Attach();
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, (GLuint)mode);
        }
        void WrapY(WrapMode mode) // Attaches the texture.
        {
            Attach();
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, (GLuint)mode);
        }
        void Wrap(WrapMode mode) // Attaches the texture.
        {
            WrapX(mode);
            WrapY(mode);
        }

        ivec2 Size() const
        {
            return size;
        }

        static void SetMaxTextureCount(int count) // If you have any attached textures when calling this, it does nothing.
        {
            if (slot_allocator.current_size() != 0)
                return;
            slot_allocator.resize(count);
        }

        ~Texture()
        {
            if (slot != SlotAllocator::not_allocated)
                slot_allocator.free(slot); // OpenGL will unbind the deleted texture automatically.
        }
    };

    template <typename T> const char *GlslTypeName()
    {
        using namespace TemplateUtils::CexprStr;
             if constexpr (std::is_same_v<T, bool        >) return "bool";
        else if constexpr (std::is_same_v<T, float       >) return "float";
        else if constexpr (std::is_same_v<T, double      >) return "double";
        else if constexpr (std::is_same_v<T, int         >) return "int";
        else if constexpr (std::is_same_v<T, unsigned int>) return "uint";
        else if constexpr (Math::type_category<T>::vec_or_mat)
        {
            using Base = typename T::type;
            using Prefix = std::conditional_t<std::is_same_v<Base, float       >, str_lit<>,
                           std::conditional_t<std::is_same_v<Base, double      >, str_lit<'d'>,
                           std::conditional_t<std::is_same_v<Base, bool        >, str_lit<'b'>,
                           std::conditional_t<std::is_same_v<Base, int         >, str_lit<'i'>,
                           std::conditional_t<std::is_same_v<Base, unsigned int>, str_lit<'u'>,
                           void
                           >>>>>;
            static_assert(!std::is_void_v<Prefix>, "No name for vectors of this base type.");
            static_assert(!Math::type_category<T>::mat || std::is_same_v<Base, float> || std::is_same_v<Base, double>, "Matrices aren't allowed to have this base type.");
            using Body = std::conditional_t<Math::type_category<T>::vec, str_lit<'v','e','c'>, str_lit<'m','a','t'>>;
            if constexpr (Math::type_category<T>::vec)
                return str_lit_cat<Prefix, Body, str_lit<T::size + '0'>>::value;
            else if constexpr (T::width == T::height)
                return str_lit_cat<Prefix, Body, str_lit<T::width + '0'>>::value;
            else
                return str_lit_cat<Prefix, Body, str_lit<T::width + '0', 'x', T::height + '0'>>::value;
        }
        else if constexpr (std::is_same_v<T, Texture>)
        {
            return "sampler2D";
        }
        else
        {
            static_assert(std::is_void_v<T>, "No name for this type.");
            return "void";
        }
    }

    class Buffer
    {
        template <typename> friend class ::Utils::Handle;
        static GLuint Create() {GLuint value; glGenBuffers(1, &value); return value;}
        static void Destroy(GLuint value) {glDeleteBuffers(1, &value);} \
        static void Error() {throw cant_create_gl_resource("Buffer");}
        using Handle = Utils::Handle<Buffer>;
        Handle handle;

      public:
        Buffer(decltype(nullptr)) : handle(Handle::params_t{}) {}
        Buffer() {}
        void create() {handle.create({});}
        void destroy() {handle.destroy();}
        GLuint operator*() const {return *handle;}
    };

    enum Usage
    {
        static_draw  = GL_STATIC_DRAW,
        dynamic_draw = GL_DYNAMIC_DRAW,
        stream_draw  = GL_STREAM_DRAW,
    };

    enum Primitive
    {
        points    = GL_POINTS,
        lines     = GL_LINES,
        triangles = GL_TRIANGLES,
    };

    template <typename T> class VertexBuffer
    {
        static_assert(Reflection::Interface::field_count<T>() || std::is_void_v<T>, "T must be reflected or be `void`.");

        inline static GLuint binding = 0, draw_binding = 0;
        inline static int active_attribute_count = 0;

        Buffer buffer;
        int size = 0;

      public:
        VertexBuffer() {}
        VertexBuffer(VertexBuffer &&) = default;
        VertexBuffer &operator=(VertexBuffer &&) = default;

        VertexBuffer(decltype(nullptr)) : buffer(nullptr) {}
        VertexBuffer(int count, const T *data = 0, Usage usage = static_draw) : buffer(nullptr)
        {
            if (count)
                SetData(count, data, usage);
        }

        void Create()
        {
            buffer.create();
            size = 0;
        }
        void Destroy()
        {
            if (*buffer)
            {
                if (binding == *buffer)
                    binding = 0; // GL unbinds a buffer when it's deleted.
                buffer.destroy();
            }
        }
        bool Exists() const
        {
            return *buffer != 0;
        }

        int Size() const {return size;}
        int ByteSize() const {return size * sizeof(T);}

        void BindStorage() const // Removes draw binding.
        {
            DebugAssert("Attempt to bind a null buffer.", *buffer);
            if (binding == *buffer)
                return;
            binding = *buffer;
            glBindBuffer(GL_ARRAY_BUFFER, binding);
            draw_binding = 0;
            return;
        }
        static void UnbindStorage() // Removes draw binding.
        {
            // I don't want to check old binding here.
            binding = 0;
            glBindBuffer(GL_ARRAY_BUFFER, 0);
            draw_binding = 0;
        }
        void BindDraw() const // Also does storage binding.
        {
            DebugAssert("Attempt to bind a null buffer.", *buffer);
            if (draw_binding == *buffer)
                return;
            BindStorage();
            draw_binding = *buffer;
            SetActiveAttributes(Reflection::Interface::field_count<T>());
            int offset = 0, pos = 0;
            TemplateUtils::for_each(std::make_index_sequence<Reflection::Interface::field_count<T>()>{}, [&](auto index)
            {
                using CurType = Reflection::Interface::field_type<T, index.value>;
                int components;
                if constexpr (Math::type_category<CurType>::vec)
                    components = CurType::size;
                else
                    components = 1;
                glVertexAttribPointer(pos++, components, GL_FLOAT, 0, sizeof(T), (void *)offset);
                offset += sizeof(CurType);
            });
        }
        static void UnbindDraw()
        {
            draw_binding = 0;
        }

        void Draw(Primitive p, int from, int count) // Binds for drawing.
        {
            BindDraw();
            glDrawArrays(p, from, count);
        }
        void Draw(Primitive p, int count) // Binds for drawing.
        {
            Draw(p, 0, count);
        }
        void Draw(Primitive p) // Binds for drawing.
        {
            Draw(p, 0, Size());
        }

        void SetData(int count, const T *data = 0, Usage usage = static_draw) // Binds storage.
        {
            BindStorage();
            size = count;
            glBufferData(GL_ARRAY_BUFFER, count * max(1u, sizeof(T)), data, usage);
        }
        void SetDataPart(int obj_offset, int count, const T *data) // Binds storage.
        {
            SetDataPartBytes(obj_offset * sizeof(T), count * max(1u, sizeof(T)), (const char *)data);
        }
        void SetDataPartBytes(int offset, int bytes, const char *data) // Binds storage.
        {
            BindStorage();
            glBufferSubData(GL_ARRAY_BUFFER, offset, bytes, data);
        }

        static void SetActiveAttributes(int count) // Makes sure attributes 0..count-1 are active.
        {
            if (count == active_attribute_count)
                return;
            if (active_attribute_count < count)
                do glEnableVertexAttribArray(active_attribute_count++); while (active_attribute_count < count);
            else if (active_attribute_count > count)
                do glDisableVertexAttribArray(--active_attribute_count); while (active_attribute_count > count);
        }

        ~VertexBuffer()
        {
            Destroy(); // We need to call this to unbind if necessary.
        }
    };

    template <typename T, Primitive P> class RenderQueue
    {
        static_assert(Reflection::Interface::field_count<T>(), "T must be reflected.");
        std::vector<T> data;
        int pos = 0;
        VertexBuffer<T> buffer;
      public:
        RenderQueue() {}
        RenderQueue(int prim_count)
        {
            Create(prim_count);
        }
        void Create(int prim_count)
        {
            if (prim_count < 1)
                prim_count = 1;
            if constexpr (P == lines)
                prim_count *= 2;
            else if constexpr (P == triangles)
                prim_count *= 3;
            std::vector<T> new_data(prim_count); // Extra exception safety.
            buffer.Create();
            buffer.SetData(prim_count);
            data = std::move(new_data);
            pos = 0;
        }
        void Destroy()
        {
            data = {};
            buffer.Destroy();
        }
        explicit operator bool() const
        {
            return bool(buffer);
        }

        void Flush()
        {
            DebugAssert("Attempt to flush a null render queue.", buffer.Exists());
            buffer.SetDataPart(0, pos, data.data());
            buffer.Draw(P, pos);
            pos = 0;
        }
        void Point(const T &a)
        {
            static_assert(P == points, "This function for point queues only.");
            if (pos + 1 > int(data.size()))
                Flush();
            data[pos++] = a;
        }
        void Line(const T &a, const T &b)
        {
            static_assert(P == lines, "This function for line queues only.");
            if (pos + 2 > int(data.size()))
                Flush();
            data[pos++] = a;
            data[pos++] = b;
        }
        void Triangle(const T &a, const T &b, const T &c)
        {
            static_assert(P == triangles, "This function for triangle queues only.");
            if (pos + 3 > int(data.size()))
                Flush();
            data[pos++] = a;
            data[pos++] = b;
            data[pos++] = c;
        }
        void Quad(const T &a, const T &b, const T &c, const T &d) // Not really a quad, but rather two triangles.
        {
            Triangle(a, b, d);
            Triangle(d, b, c);
        }
    };

    class Shader
    {
        enum class ShaderType
        {
            vertex   = GL_VERTEX_SHADER,
            fragment = GL_FRAGMENT_SHADER,
        };

        class SeparateShader
        {
            template <typename> friend class ::Utils::Handle;
            static GLuint Create(ShaderType type) {return glCreateShader((GLenum)type);}
            static void Destroy(GLuint value) {glDeleteShader(value);}
            static void Error(ShaderType) {throw cant_create_gl_resource("Shader");}
            using Handle = Utils::Handle<SeparateShader>;
            Handle object;
          public:
            SeparateShader() {}
            SeparateShader(ShaderType type) : object({type}) {}
            void create(ShaderType type) {object.create({type});}
            void destroy() {object.destroy();}
            GLuint handle() const {return *object;}
            SeparateShader &&move() {return (SeparateShader&&)*this;}
            bool compile(const std::string &src) const
            {
                const char *ptr = src.c_str();
                glShaderSource(*object, 1, &ptr, 0);
                glCompileShader(*object);
                GLint status;
                glGetShaderiv(*object, GL_COMPILE_STATUS, &status);
                return status;
            }
            std::string get_log() const
            {
                GLint len;
                glGetShaderiv(*object, GL_INFO_LOG_LENGTH, &len);
                if (len == 0)
                    return "";
                std::string ret;
                ret.resize(len-1); // std::string adds a null-terminator automatically
                glGetShaderInfoLog(*object, len, 0, ret.data());
                return Strings::Escape(Strings::Trim(ret));
            }
        };

        class Program
        {
            template <typename> friend class ::Utils::Handle;
            static GLuint Create() {return glCreateProgram();}
            static void Destroy(GLuint value) {glDeleteProgram(value);}
            static void Error() {throw cant_create_gl_resource("Shader program");}
            using Handle = Utils::Handle<Program>;
            Handle object;
          public:
            Program() {}
            Program(decltype(nullptr)) : object(Handle::params_t{}) {}
            void destroy() {object.destroy();}
            GLuint handle() const {return *object;}
            Program &&move() {return (Program&&)*this;}
            void attach(const SeparateShader &sh) const
            {
                glAttachShader(*object, sh.handle());
            }
            void set_attribute_location(int loc, const std::string &name)
            {
                glBindAttribLocation(*object, loc, name.c_str());
            }
            bool link()
            {
                glLinkProgram(*object);
                GLint status;
                glGetProgramiv(*object, GL_LINK_STATUS, &status);
                return status;
            }
            std::string get_log()
            {
                GLint len;
                glGetProgramiv(*object, GL_INFO_LOG_LENGTH, &len);
                if (len == 0)
                    return "";
                std::string ret;
                ret.resize(len-1); // std::string adds a null-terminator automatically
                glGetProgramInfoLog(*object, len, 0, ret.data());
                return Strings::Escape(Strings::Trim(ret));
            }
        };

        Program program;
        SeparateShader vertex, fragment;
        inline static GLuint binding = 0;
      public:
        Shader() {}
        Shader(Shader &&) = default;
        Shader &operator=(Shader &&) = default;

        struct Config
        {
            std::string version = "330 compatibility";
            std::string vertex_header, fragment_header;
            std::string uniform = "uniform";
            std::string uniform_prefix = "u_";
            std::string attribute = "attribute";
            std::string attribute_prefix = "a_";
            std::string varying_vertex = "varying";
            std::string varying_fragment = "varying";
            std::string varying_prefix = "v_";
        };
        struct Attribute
        {
            int location;
            std::string name;
        };
        void CreateRaw(const std::string &v_src, const std::string &f_src, Utils::Range<const Attribute> attributes = {})
        {
            Program p(nullptr);
            SeparateShader v(ShaderType::vertex), f(ShaderType::fragment);
            p.attach(v);
            p.attach(f);
            bool v_status = v.compile(v_src);
            bool f_status = f.compile(f_src);
            if (!v_status || !f_status)
                throw shader_compilation_error(v_status ? "OK" : "NOT OK",
                                               f_status ? "OK" : "NOT OK",
                                               '\n' + v.get_log(),
                                               '\n' + f.get_log());
            for (const auto &it : attributes)
                p.set_attribute_location(it.location, it.name);
            if (!p.link())
                throw shader_linking_error(p.get_log());
            vertex   = v.move();
            fragment = f.move();
            program  = p.move();
        }

        template <typename T> class Uniform
        {
            GLuint sh = 0;
            int loc = -1;
          public:
            using type = T;
            Uniform() {}
            Uniform(GLuint sh, int loc) : sh(sh), loc(loc) {}
            const T &operator=(const T &object) const // Binds the shader.
            {
                DebugAssert("Attempt to bind a null shader.", sh);
                if (sh != binding)
                {
                    binding = sh;
                    glUseProgram(sh);
                }
                if constexpr (std::is_same_v<T, Texture>)
                {
                    glUniform1i(loc, object.Slot());
                }
                else if constexpr (std::is_same_v<type, float       >) glUniform1f (loc, object);
                else if constexpr (std::is_same_v<type, fvec2       >) glUniform2f (loc, object.x, object.y);
                else if constexpr (std::is_same_v<type, fvec3       >) glUniform3f (loc, object.x, object.y, object.z);
                else if constexpr (std::is_same_v<type, fvec4       >) glUniform4f (loc, object.x, object.y, object.z, object.w);
                else if constexpr (std::is_same_v<type, int         >) glUniform1i (loc, object);
                else if constexpr (std::is_same_v<type, ivec2       >) glUniform2i (loc, object.x, object.y);
                else if constexpr (std::is_same_v<type, ivec3       >) glUniform3i (loc, object.x, object.y, object.z);
                else if constexpr (std::is_same_v<type, ivec4       >) glUniform4i (loc, object.x, object.y, object.z, object.w);
                else if constexpr (std::is_same_v<type, unsigned int>) glUniform1ui(loc, object);
                else if constexpr (std::is_same_v<type, uvec2       >) glUniform2ui(loc, object.x, object.y);
                else if constexpr (std::is_same_v<type, uvec3       >) glUniform3ui(loc, object.x, object.y, object.z);
                else if constexpr (std::is_same_v<type, uvec4       >) glUniform4ui(loc, object.x, object.y, object.z, object.w);
                else if constexpr (std::is_same_v<type, fmat2       >) glUniformMatrix2fv(loc, 1, 0, object.as_array());
                else if constexpr (std::is_same_v<type, fmat3       >) glUniformMatrix3fv(loc, 1, 0, object.as_array());
                else if constexpr (std::is_same_v<type, fmat4       >) glUniformMatrix4fv(loc, 1, 0, object.as_array());
                else if constexpr (std::is_same_v<type, fmat3x2     >) glUniformMatrix3x2fv(loc, 1, 0, object.as_array());
                else if constexpr (std::is_same_v<type, fmat4x2     >) glUniformMatrix4x2fv(loc, 1, 0, object.as_array());
                else if constexpr (std::is_same_v<type, fmat2x3     >) glUniformMatrix2x3fv(loc, 1, 0, object.as_array());
                else if constexpr (std::is_same_v<type, fmat4x3     >) glUniformMatrix4x3fv(loc, 1, 0, object.as_array());
                else if constexpr (std::is_same_v<type, fmat2x4     >) glUniformMatrix2x4fv(loc, 1, 0, object.as_array());
                else if constexpr (std::is_same_v<type, fmat3x4     >) glUniformMatrix3x4fv(loc, 1, 0, object.as_array());
                else static_assert(std::is_void_v<type>, "Uniforms of this type are not supported.");
                return object;
            }
        };
        template <typename T> class VertexUniform   : public Uniform<T> {public: using Uniform<T>::Uniform; using Uniform<T>::operator=;};
        template <typename T> class FragmentUniform : public Uniform<T> {public: using Uniform<T>::Uniform; using Uniform<T>::operator=;};

        template <typename ReflAttributes = void, // Has to be reflected. Regardless of reflected types, shader will get them as floats.
                  typename ReflUniforms   = void> // Has to be reflected and contain only [Vertex|Fragment]Uniform structs.
        void Create(const std::string &v_src, const std::string &f_src, ReflUniforms *uniforms = 0, const Config &cfg = {})
        {
            std::string v, f;
            v = "#version " + cfg.version + '\n' + cfg.vertex_header + '\n';
            f = "#version " + cfg.version + '\n' + cfg.fragment_header + '\n';
            std::vector<Attribute> attribute_vector;
            v += "#define VARYING(type,name) " + cfg.varying_vertex   + " type " + (cfg.varying_prefix.size() ? cfg.varying_prefix + "##name;\n" : "name;\n");
            f += "#define VARYING(type,name) " + cfg.varying_fragment + " type " + (cfg.varying_prefix.size() ? cfg.varying_prefix + "##name;\n" : "name;\n");
            if constexpr (!std::is_void_v<ReflUniforms>)
            {
                constexpr int field_count = Reflection::Interface::field_count<ReflUniforms>();
                static_assert(field_count > 0, "Unable to reflect common uniforms. Pass `void` as ReflUniforms if you have none.");
                TemplateUtils::for_each(std::make_index_sequence<field_count>{}, [&](auto index)
                {
                    const char *field_name = Reflection::Interface::field_name<ReflUniforms, index.value>();
                    using field_type_raw = Reflection::Interface::field_type<ReflUniforms, index.value>;
                    using field_type = typename field_type_raw::type;
                    if constexpr (!std::is_same_v<VertexUniform<field_type>, field_type_raw>)
                        f += cfg.uniform + ' ' + GlslTypeName<field_type>() + ' ' + cfg.uniform_prefix + field_name + ";\n";
                    if constexpr (!std::is_same_v<FragmentUniform<field_type>, field_type_raw>)
                        v += cfg.uniform + ' ' + GlslTypeName<field_type>() + ' ' + cfg.uniform_prefix + field_name + ";\n";
                });
            }
            if constexpr (!std::is_void_v<ReflAttributes>)
            {
                constexpr int field_count = Reflection::Interface::field_count<ReflAttributes>();
                static_assert(field_count > 0, "Unable to reflect attributes. Pass `void` as ReflAttributes if you have none.");
                attribute_vector.reserve(field_count);
                TemplateUtils::for_each(std::make_index_sequence<field_count>{}, [&](auto index)
                {
                    const char *field_name = Reflection::Interface::field_name<ReflAttributes, index.value>();
                    using field_type = Math::change_base_type_t<Reflection::Interface::field_type<ReflAttributes, index.value>, float>;
                    v += cfg.attribute + ' ' + GlslTypeName<field_type>() + ' ' + cfg.attribute_prefix + field_name + ";\n";
                    attribute_vector.push_back({int(attribute_vector.size()), cfg.attribute_prefix + field_name});
                });
            }
            CreateRaw(v + v_src, f + f_src, attribute_vector);
            if constexpr (!std::is_void_v<ReflUniforms>)
            {
                if (uniforms)
                {
                    constexpr int field_count = Reflection::Interface::field_count<ReflUniforms>();
                    static_assert(field_count > 0, "Unable to reflect common uniforms. Pass `void` as ReflUniforms if you have none.");
                    TemplateUtils::for_each(std::make_index_sequence<field_count>{}, [&](auto index)
                    {
                        const char *field_name = Reflection::Interface::field_name<ReflUniforms, index.value>();
                        auto &ref = Reflection::Interface::field<index.value>(*uniforms);
                        ref = std::remove_reference_t<decltype(ref)>(program.handle(), GetUniformLocation(cfg.uniform_prefix + field_name));
                    });
                }
            }
        }
        void Destroy()
        {
            if (program.handle() && binding == program.handle())
                Unbind(); // GL doesn't delete shaders if they're bound.
            program.destroy();
            vertex.destroy();
            fragment.destroy();
        }
        bool Exists() const
        {
            return program.handle() != 0;
        }

        void Bind() const
        {
            DebugAssert("Attempt to bind a null shader.", program.handle());
            if (program.handle() == binding)
                return;
            binding = program.handle();
            glUseProgram(program.handle());
        }
        static void Unbind()
        {
            // I don't want to check old binding here.
            binding = 0;
            glUseProgram(0);
        }
        GLint GetUniformLocation(const std::string &name) const
        {
            return glGetUniformLocation(program.handle(), name.c_str());
        }
        ~Shader()
        {
            Destroy(); // We need to call this to unbind if necessary.
        }
    };
}

#endif
