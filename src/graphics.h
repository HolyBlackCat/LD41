#ifndef GRAPHICS_H_INCLUDED
#define GRAPHICS_H_INCLUDED

#include <string>
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

    class Buffer
    {
        template <typename> friend class ::Wrappers::Handle;
        static GLuint Create() {GLuint value; glGenBuffers(1, &value); return value;}
        static void Destroy(GLuint value) {glDeleteBuffers(1, &value);} \
        static void Error() {throw cant_create_gl_resource("Buffer");}
        using Handle = Wrappers::Handle<Buffer>;
        Handle handle;

      public:
        Buffer(decltype(nullptr)) : handle({}) {}
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
        VertexByteBuffer(int bytes, char *ptr = 0, Usage usage = Usage::static_draw) : buffer(nullptr)
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
            glBindBuffer(GL_VERTEX_ARRAY, binding);
        }
        static void Unbind()
        {
            // I don't want to check old binding here.
            binding = 0;
            glBindBuffer(GL_VERTEX_ARRAY, 0);
        }
        void SetData(int bytes, char *ptr = 0, Usage usage = Usage::static_draw)
        {
            Bind();
            size = bytes;
            glBufferData(GL_VERTEX_ARRAY, bytes, ptr, (GLenum)usage);
        }
        void SetDataPart(int offset, int bytes, char *ptr)
        {
            Bind();
            glBufferSubData(GL_VERTEX_ARRAY, offset, bytes, ptr);
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
            Program(decltype(nullptr)) : object({}) {}
            void destroy() {object.destroy();}
            GLuint handle() const {return *object;}
            Program &&move() {return (Program&&)*this;}
            void attach(const SeparateShader &sh) const
            {
                glAttachShader(*object, sh.handle());
            }
            void set_attribute_location(int loc, const char *name)
            {
                glBindAttribLocation(*object, loc, name);
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
        std::vector<GLint> uniform_locations;
        inline static GLuint binding = 0;
      public:
        struct Attribute
        {
            const char *name;
            int location;
        };
        Shader() {}
        void Create(const std::string &v_src, const std::string &f_src, const std::vector<Attribute> &attributes = {})
        {
            Program p({});
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
            {
                p.set_attribute_location(it.location, it.name);
            }
            if (!p.link())
                throw shader_linking_error(p.get_log());
            vertex   = v.move();
            fragment = f.move();
            program  = p.move();
        }
        void Destroy()
        {
            uniform_locations = {};
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
        GLint UniformLocation(int index) // GetUniformLocations must be called before this can be used.
        {
            return uniform_locations[index];
        }
        void GetUniformLocations(const std::vector<const char *> &uniforms)
        {
            uniform_locations = {};
            for (const char *it : uniforms)
            {
                uniform_locations.push_back(glGetUniformLocation(program.handle(), it));
            }
        }
        ~Shader()
        {
            if (program.handle() && binding == program.handle())
                Unbind();
        }
    };
}

#endif
