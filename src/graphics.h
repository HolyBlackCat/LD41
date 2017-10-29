#ifndef GRAPHICS_H_INCLUDED
#define GRAPHICS_H_INCLUDED

#include <algorithm>
#include <string>
#include <type_traits>
#include <vector>
#include <utility>

#include <GLFL/glfl.h>
#include <stb_image.h>
#include <stb_image_write.h>

#include "exceptions.h"
#include "platform.h"
#include "program.h"
#include "reflection.h"
#include "template_utils.h"
#include "utils.h"

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
        DefineExceptionInline(cant_load_image, :exception, "Can't load an image.",
            (std::string,name,"Name")
        )
        DefineExceptionInline(no_free_texture_slots, :exception, "Out of free texture slots.",)

        DefineExceptionBase(shader_exception, :exception)
        //{
            DefineExceptionInline(shader_compilation_error, :shader_exception, "Shader compilation error.",
                (std::string,vertex_status,"Vertex status")
                (std::string,fragment_status,"Fragment status")
                (std::string,vertex_log,"Vertex log")
                (std::string,fragment_log,"Fragment log")
            )
            DefineExceptionInline(shader_linking_error, :shader_exception, "Shader linking error.",
                (std::string,fragment_log,"Log")
            )
        //}
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
        Image(std::string fname, bool flip_y = 1)
        {
            FromFile(fname, flip_y);
        }
        ivec2 Size() const {return {width, int(data.size()) / width};}
        const u8vec4 *Data() const {return data.data();}
        u8vec4 Get(ivec2 pos) const
        {
            pos = clamp(pos, 0, Size());
            return data[pos.x + pos.y * width];
        }
        void Set(ivec2 pos, u8vec4 v)
        {
            if ((pos < 0).any() || (pos >= Size()).any())
                return;
            data[pos.x + pos.y * width] = v;
        }

        void FromMemory(ivec2 size, const char *ptr = 0)
        {
            width = size.x;
            data.resize(size.product());
            if (ptr)
                std::copy(ptr, ptr + size.product() * sizeof(u8vec4), (char *)data.data());
        }
        void FromFile(std::string fname, bool flip_y = 1)
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
        Texture(InterpMode mode, ivec2 size, const u8vec4 *data = 0) // Creates and attaches the texture.
        {
            Create();
            SetData(size, data);
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

        int Slot() const
        {
            return slot;
        }

        void SetData(ivec2 size, const u8vec4 *data = 0) // Attaches the texture.
        {
            Attach();
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
        VertexBuffer(decltype(nullptr)) : buffer(nullptr) {}
        VertexBuffer(int count, const T *data = 0, Usage usage = static_draw) : buffer(nullptr)
        {
            if (count)
                SetData(count, data, usage);
        }

        void Create()
        {
            buffer.create();
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
        explicit operator bool() const
        {
            return *buffer;
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
            DebugAssert("Attempt to flush a null render queue.", buffer);
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
        Shader() {}
        void CreateRaw(const std::string &v_src, const std::string &f_src, const std::vector<Attribute> &attributes = {})
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
        void Create(const std::vector<std::string> &varyings, const std::string &v_src, const std::string &f_src, ReflUniforms *uniforms = 0, const Config &cfg = {})
        {
            std::string v, f;
            v = "#version " + cfg.version + '\n' + cfg.vertex_header + '\n';
            f = "#version " + cfg.version + '\n' + cfg.fragment_header + '\n';
            std::vector<Attribute> attribute_vector;
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
            for (const auto &it : varyings)
            {
                std::string cur = it;
                auto space_pos = cur.find_last_of(' ');
                if (space_pos != std::string::npos)
                    cur.insert(space_pos+1, cfg.varying_prefix);
                v += cfg.varying_vertex   + ' ' + cur + ";\n";
                f += cfg.varying_fragment + ' ' + cur + ";\n";
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
