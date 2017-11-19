#include "everything.h"

#include <iostream>
#include <sstream>

Input::Mouse mouse;

ReflectStruct(Attributes, (
    (fvec3)(pos),
    (fvec3)(normal),
))

ReflectStruct(Uniforms, (
    (Graphics::Shader::VertexUniform<fmat4>)(projection,view,model),
    (Graphics::Shader::VertexUniform<fmat3>)(normal),
    (Graphics::Shader::FragmentUniform<fvec3>)(camera),
))

Graphics::VertexBuffer<Attributes> LoadModel(Utils::MemoryFile file)
{
    std::string_view view((const char *)file.Data(), file.Size());
    std::istringstream ss;

    std::vector<fvec3> vertices, normals;

    struct Index
    {
        int vertex, normal;
    };
    std::vector<Index> indices;

    int line = 0;
    std::size_t next_line_start;

    do
    {
        line++;
        ss.clear();
        auto line_end = view.find_first_of("\n\r");
        ss.str(std::string(view.substr(0, line_end)));
        next_line_start = view.find_first_not_of("\n\r", line_end);
        if (next_line_start != view.npos)
            view.remove_prefix(next_line_start);
        ss.seekg(0); // Not sure if this is needed after `.str()`.
        std::string word;
        ss >> word;
        if (!ss)
            continue;
        if (word.empty() || word[0] == '#')
            continue;
        if (word == "s"       // Smooth lighting
         || word == "o"       // Object
         || word == "g"       // Group
         || word == "mtllib"  // Material library
         || word == "usemtl") // Use material
            continue;

        fvec3 v;
        if (word == "v") // Vertex
        {
            ss >> v;
            if (!ss)
                Program::Error(Str("Parsing failure at ", file.Name(), ":", line, "."));
            vertices.push_back(v);
        }
        else if (word == "vn") // Normal
        {
            ss >> v;
            if (!ss)
                Program::Error(Str("Parsing failure at ", file.Name(), ":", line, "."));
            normals.push_back(v);
        }
        else if (word == "f") // Face (aka triangle)
        {
            int v, n;
            char ch;
            int v_count = 0;
            while (1)
            {
                ss >> v;
                if (!ss)
                {
                    if (v_count < 3)
                        Program::Error(Str("Parsing failure at ", file.Name(), ":", line, ", not enough vertices."));
                    break;
                }
                if (v_count >= 4)
                    Program::Error(Str("Parsing failure at ", file.Name(), ":", line, ", too many vertices."));
                for (int i = 0; i < 2; i++)
                {
                    ss >> ch;
                    if (ch != '/')
                        Program::Error(Str("Parsing failure at ", file.Name(), ":", line, "."));
                }
                ss >> n;
                v_count++;
                indices.push_back({v-1, n-1});
            }
            if (v_count == 4)
            {
                indices.push_back(indices[indices.size()-4]);
                indices.push_back(indices[indices.size()-3]);
            }
        }
        else
            Program::Error(Str("Unknown instruction at ", file.Name(), ":", line, "."));
    }
    while (next_line_start != view.npos);

    std::vector<Attributes> attribs;
    attribs.reserve(indices.size());
    for (const auto &it : indices)
    {
        Attributes a;
        a.pos = vertices[it.vertex];
        a.normal = normals[it.normal];
        attribs.push_back(a);
    }

    return Graphics::VertexBuffer<Attributes>(attribs.size(), attribs.data());
}

Graphics::Shader sh;
Uniforms uniforms;

int main(int, char **)
{
    Window win("Woah", {800,600});


    sh.Create<Attributes>(
R"(
VARYING(vec3, pos)
VARYING(vec3, normal)
void main()
{
    vec4 pos = u_projection * u_view * u_model * vec4(a_pos, 1);
    gl_Position = pos;
    v_pos = a_pos;
    v_normal = u_normal * a_normal;
})",
R"(
VARYING(vec3, pos)
VARYING(vec3, normal)
void main()
{
    float c = dot(vec3(0,0,1), normalize(v_normal));
    c = c * 0.9 + 0.1;
    gl_FragColor = vec4(c,c,c,1);
}
)", &uniforms);

    uniforms.projection = fmat4::perspective(to_rad(85), win.Size().ratio(), 0.1, 100);
    fmat4 view = fmat4::look_at({0,0,5},{0,0,0},{0,1,0});
    uniforms.view = view;
    uniforms.model = fmat4::identity();
    uniforms.normal = fmat3::identity();
    uniforms.camera = {0,0,5};

    Graphics::Blending::Enable();
    Graphics::Blending::FuncNormalPre();

    Graphics::VertexBuffer<Attributes> buf = LoadModel("untitled.obj");

    Graphics::Depth(1);

    //fquat q = fquat::around_axis({1,0,0}, f_pi / 6);
    fmat4 m = fmat4::rotate({1,0,0}, f_pi / 6);

    while (1)
    {
        Events::Process();

        if (Input::Keys::f11.pressed())
            win.ToggleFullscreen();

        Graphics::CheckErrors();
        Graphics::Clear(Graphics::color | Graphics::depth);

        //q = q /mul/ fquat::around_axis({0,1,0}, 0.1);
        //q.normalize();
        //fmat4 m = q.make_mat4();
        //m = m /mul/ fmat4::rotate({0,1,0}, 0.02);
        m = fquat::around_axis({0,1,0}, mouse.pos().x / 100.).make_mat4();
        uniforms.model = m;
        uniforms.normal = (view /mul/ m).to_mat3().inverse().transpose();

        buf.Draw(Graphics::triangles);

        win.Swap();
    }

    return 0;
}
