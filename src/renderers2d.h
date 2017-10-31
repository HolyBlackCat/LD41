#ifndef RENDERERS2D_H_INCLUDED
#define RENDERERS2D_H_INCLUDED

#include <utility>

#include "graphics.h"
#include "mat.h"
#include "reflection.h"

namespace Renderers
{
    ReflectStruct(Attributes, (
        (fvec2)(pos),
        (fvec4)(color),
        (fvec2)(texture_pos),
        (fvec3)(factors),
    ))

    ReflectStruct(Uniforms, (
        (Graphics::Shader::VertexUniform<fmat4>)(matrix),
        (Graphics::Shader::VertexUniform<fvec2>)(texture_size),
        (Graphics::Shader::FragmentUniform<Graphics::Texture>)(texture),
        (Graphics::Shader::FragmentUniform<fmat4>)(color_matrix),
    ))

    class Poly2D
    {
        Graphics::Shader shader;
        Graphics::RenderQueue<Attributes, Graphics::triangles> queue;
      public:
        Uniforms uni;
        Poly2D() {}
        Poly2D(int size, const Graphics::Shader::Config &cfg = {})
        {
            Create(size, cfg);
        }
        Poly2D(int size, const std::string &v_src, const std::string &f_src, const Graphics::Shader::Config &cfg = {})
        {
            Create(size, v_src, f_src, cfg);
        }
        void Create(int size, const Graphics::Shader::Config &cfg = {})
        {
            constexpr const char *v = R"(
VARYING( vec4 , color       )
VARYING( vec2 , texture_pos )
VARYING( vec3 , factors     )
void main()
{
    gl_Position = u_matrix * vec4(a_pos, 0, 1);
    v_color       = a_color;
    v_texture_pos = a_texture_pos / u_texture_size;
    v_factors     = a_factors;
})";
            constexpr const char *f = R"(
VARYING( vec4 , color       )
VARYING( vec2 , texture_pos )
VARYING( vec3 , factors     )
void main()
{
    vec4 tex_color = texture2D(u_texture, v_texture_pos);
    gl_FragColor = vec4(v_color.rgb * (1. - v_factors.x) + tex_color.rgb * v_factors.x,
                        v_color.a   * (1. - v_factors.y) + tex_color.a   * v_factors.y);
    vec4 result = u_color_matrix * vec4(gl_FragColor.rgb, 1);
    gl_FragColor.a *= result.a;
    gl_FragColor.rgb = result.rgb * gl_FragColor.a;
    gl_FragColor.a *= v_factors.z;
})";
            Create(size, v, f, cfg);
        }
        void Create(int size, const std::string &v_src, const std::string &f_src, const Graphics::Shader::Config &cfg = {}) // With custom shader.
        {
            decltype(shader) new_shader;
            new_shader.Create<Attributes>(v_src, f_src, &uni, cfg);
            decltype(queue) new_queue(size);
            shader = std::move(new_shader);
            queue  = std::move(new_queue);
        }
        void Destroy()
        {
            shader.Destroy();
            queue.Destroy();
        }

        void Finish()
        {
            queue.Flush();
        }
    };
}

#endif
