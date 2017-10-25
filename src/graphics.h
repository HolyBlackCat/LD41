#ifndef GRAPHICS_H_INCLUDED
#define GRAPHICS_H_INCLUDED

#include <string>
#include <type_traits>
#include <vector>

#include <GLFL/glfl.h>

#include "exceptions.h"
#include "program.h"
#include "reflection.h"
#include "template_utils.h"
#include "wrappers.h"

namespace Graphics
{
    inline namespace Exceptions
    {
        DefineExceptionBase(exception)
        DefineExceptionInline(cant_create_gl_resource, :exception, "Can't create an OpenGL resource.",
            (std::string,type,"Type")
        )
        DefineExceptionBase(shader_exception, :exception)
        DefineExceptionInline(shader_compilation_error, :shader_exception, "Shader compilation error.",
            (std::string,vertex_status,"Vertex status")
            (std::string,fragment_status,"Fragment status")
            (std::string,vertex_log,"Vertex log")
            (std::string,fragment_log,"Fragment log")
        )
        DefineExceptionInline(shader_linking_error, :shader_exception, "Shader linking error.",
            (std::string,fragment_log,"Log")
        )
    }

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
        else
        {
            static_assert(std::is_void_v<T>, "No name for this type.");
            return "void";
        }
    }


    class Buffer
    {
        template <typename> friend class ::Wrappers::Handle;
        static GLuint Create() {GLuint value; glGenBuffers(1, &value); return value;}
        static void Destroy(GLuint value) {glDeleteBuffers(1, &value);} \
        static void Error() {throw cant_create_gl_resource("Buffer");}
        using Handle = Wrappers::Handle<Buffer>;
        Handle handle;

      public:
        Buffer(decltype(nullptr)) : handle(Handle::params_t{}) {}
        Buffer() {}
        void create() {handle.create({});}
        void destroy() {handle.destroy();}
        GLuint operator*() const {return *handle;}
    };

    enum class Usage
    {
        static_draw  = GL_STATIC_DRAW,
        dynamic_draw = GL_DYNAMIC_DRAW,
        stream_draw  = GL_STREAM_DRAW,
    };

    class VertexByteBuffer
    {
        inline static GLuint binding = 0;

        Buffer buffer;
        int size = 0;
      public:
        VertexByteBuffer() {}
        VertexByteBuffer(int bytes, const char *ptr = 0, Usage usage = Usage::static_draw) : buffer(nullptr)
        {
            if (bytes)
                SetData(bytes, ptr, usage);
        }
        ~VertexByteBuffer()
        {
            if (*buffer && binding == *buffer)
                Unbind();
        }

        void Bind() const
        {
            DebugAssert("Attempt to bind a null buffer.", *buffer);
            if (binding == *buffer)
                return;
            binding = *buffer;
            glBindBuffer(GL_ARRAY_BUFFER , binding);
        }
        static void Unbind()
        {
            // I don't want to check old binding here.
            binding = 0;
            glBindBuffer(GL_ARRAY_BUFFER , 0);
        }
        void SetData(int bytes, const char *ptr = 0, Usage usage = Usage::static_draw)
        {
            Bind();
            size = bytes;
            glBufferData(GL_ARRAY_BUFFER , bytes, ptr, (GLenum)usage);
        }
        void SetDataPart(int offset, int bytes, const char *ptr)
        {
            Bind();
            glBufferSubData(GL_ARRAY_BUFFER , offset, bytes, ptr);
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
            template <typename> friend class ::Wrappers::Handle;
            static GLuint Create(ShaderType type) {return glCreateShader((GLenum)type);}
            static void Destroy(GLuint value) {glDeleteShader(value);}
            static void Error(ShaderType) {throw cant_create_gl_resource("Shader");}
            using Handle = Wrappers::Handle<SeparateShader>;
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
            template <typename> friend class ::Wrappers::Handle;
            static GLuint Create() {return glCreateProgram();}
            static void Destroy(GLuint value) {glDeleteProgram(value);}
            static void Error() {throw cant_create_gl_resource("Shader program");}
            using Handle = Wrappers::Handle<Program>;
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
            ""; std::cout << "VERTEX:\n" << v_src << "\nFRAGMENT:\n" << f_src << "\nEND\n";
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

        template <typename T> struct Uniform
        {
            using type = T;
            int location = -1;
        };
        template <typename T> struct VertexUniform
        {
            using type = T;
            int location = -1;
        };
        template <typename T> struct FragmentUniform
        {
            using type = T;
            int location = -1;
        };

        template <typename ReflAttributes = void, // Has to be reflected.
                  typename ReflVaryings   = void, // Has to be reflected too.
                  typename ReflUniforms   = void> // Has to be reflected and contain only [Vertex|Fragment]Uniform structs.
        void Create(const std::string &v_src, const std::string &f_src, ReflUniforms *uniforms = 0, const Config &cfg = {})
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
                    if (uniforms)
                        Reflection::Interface::field<index.value>(*uniforms).location = GetUniformLocation(cfg.uniform_prefix + field_name);
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
            if constexpr (!std::is_void_v<ReflVaryings>)
            {
                constexpr int field_count = Reflection::Interface::field_count<ReflVaryings>();
                static_assert(field_count > 0, "Unable to reflect varyings. Pass `void` as ReflVaryings if you have none.");
                TemplateUtils::for_each(std::make_index_sequence<field_count>{}, [&](auto index)
                {
                    const char *field_name = Reflection::Interface::field_name<ReflVaryings, index.value>();
                    using field_type = Reflection::Interface::field_type<ReflVaryings, index.value>;
                    v += cfg.varying_vertex   + ' ' + GlslTypeName<field_type>() + ' ' + cfg.varying_prefix + field_name + ";\n";
                    f += cfg.varying_fragment + ' ' + GlslTypeName<field_type>() + ' ' + cfg.varying_prefix + field_name + ";\n";
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
                        Reflection::Interface::field<index.value>(*uniforms).location = GetUniformLocation(cfg.uniform_prefix + field_name);
                    });
                }
            }
        }
        void Destroy()
        {
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
            if (program.handle() && binding == program.handle())
                Unbind();
        }
    };
}

#endif
