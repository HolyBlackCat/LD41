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
        DefineExceptionInline(shader_compilation_error, :exception, "Shader compilation error.",
            (std::string,vertex_status,"Vertex status")
            (std::string,fragment_status,"Fragment status")
            (std::string,vertex_log,"Vertex log")
            (std::string,fragment_log,"Fragment log")
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
        Buffer() : handle({}) {}
        Buffer(decltype(nullptr)) {}
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
        VertexByteBuffer() : buffer(nullptr) {}
        VertexByteBuffer(int bytes, char *ptr = 0, Usage usage = Usage::static_draw)
        {
            if (bytes)
                SetData(bytes, ptr, usage);
        }
        ~VertexByteBuffer()
        {
            if (binding == *buffer)
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
            GLuint handle() const {return *object;}
        };
    };
}

#endif
