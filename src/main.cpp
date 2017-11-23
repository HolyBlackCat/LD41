#include "everything.h"

#include <iostream>
#include <sstream>

Input::Mouse mouse;

ReflectStruct(AttributesMain, (
    (fvec3)(pos),
    (fvec3)(normal),
    (float)(material),
))

ReflectStruct(UniformsMain, (
    (Graphics::Shader::VertexUniform<fmat4>)(projection,view,model),
    (Graphics::Shader::VertexUniform<fmat3>)(normal),
    (Graphics::Shader::VertexUniform<float>)(material_count),
    (Graphics::Shader::FragmentUniform<fvec3>)(camera),
    (Graphics::Shader::FragmentUniform<Graphics::Texture>)(materials),
    (Graphics::Shader::FragmentUniform<Graphics::Texture>)(lookup),
    (Graphics::Shader::FragmentUniform<fvec3>)(background),
))

ReflectStruct(AttributesTex, (
    (fvec2)(pos),
))

ReflectStruct(UniformsExtractBright, (
    (Graphics::Shader::FragmentUniform<Graphics::Texture>)(texture),
    (Graphics::Shader::FragmentUniform<float>)(exposure),
    (Graphics::Shader::FragmentUniform<float>)(threshold),
))

ReflectStruct(UniformsBlur, (
    (Graphics::Shader::FragmentUniform<Graphics::Texture>)(texture),
    (Graphics::Shader::FragmentUniform<fvec2>)(step),
    (Graphics::Shader::FragmentUniform<bool>)(vertical),
))

ReflectStruct(UniformsIdentity, (
    (Graphics::Shader::FragmentUniform<Graphics::Texture>)(texture),
    (Graphics::Shader::FragmentUniform<float>)(factor),
))

ReflectStruct(UniformsFinal, (
    (Graphics::Shader::FragmentUniform<Graphics::Texture>)(texture),
    (Graphics::Shader::FragmentUniform<float>)(exposure),
))

Graphics::VertexBuffer<AttributesMain> LoadModel(Utils::MemoryFile file)
{
    std::string_view view((const char *)file.Data(), file.Size());
    std::istringstream ss;

    std::vector<fvec3> vertices, normals;

    struct Index
    {
        int vertex, normal, material;
    };
    std::vector<Index> indices;

    int line = 0;
    std::size_t next_line_start;

    int material_index = 0;

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
         || word == "mtllib") // Material library
         // Use material
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
                indices.push_back({v-1, n-1, material_index});
            }
            if (v_count == 4)
            {
                indices.push_back(indices[indices.size()-4]);
                indices.push_back(indices[indices.size()-3]);
            }
        }
        else if (word == "usemtl")
        {
            ss >> material_index;
            if (!ss)
                Program::Error(Str("Parsing failure at ", file.Name(), ":", line, ", material name must be a number."));
        }
        else
            Program::Error(Str("Unknown instruction at ", file.Name(), ":", line, "."));
    }
    while (next_line_start != view.npos);

    std::vector<AttributesMain> attribs;
    attribs.reserve(indices.size());
    for (const auto &it : indices)
    {
        AttributesMain a;
        a.pos = vertices[it.vertex];
        a.normal = normals[it.normal];
        a.material = it.material;
        attribs.push_back(a);
    }

    return Graphics::VertexBuffer<AttributesMain>(attribs.size(), attribs.data());
}

Window win;
Graphics::Shader shader_main;
Graphics::Shader shader_extract_bright;
Graphics::Shader shader_blur;
Graphics::Shader shader_identity;
Graphics::Shader shader_final;
Graphics::Texture tex_materials;
Graphics::Texture tex_lookup;
Graphics::Texture tex_framebuffer_hdr;
Graphics::Texture tex_framebuffer_hdr_depth;
Graphics::FrameBuffer framebuffer_hdr;
Graphics::Texture tex_framebuffer_bloom1;
Graphics::Texture tex_framebuffer_bloom2;
Graphics::FrameBuffer framebuffer_bloom1;
Graphics::FrameBuffer framebuffer_bloom2;

UniformsMain uniforms_main;
UniformsExtractBright uniforms_extract_bright;
UniformsBlur uniforms_blur;
UniformsIdentity uniforms_identity;
UniformsFinal uniforms_final;

namespace ShaderSource
{
    inline namespace Main
    {
        constexpr const char *main_v = R"(
VARYING(vec3, pos)
VARYING(vec3, normal)
VARYING(float, material)
void main()
{
    vec4 v = u_model * vec4(a_pos, 1);
    v_pos = v.xyz;
    gl_Position = u_projection * u_view * v;
    v_normal = u_normal * a_normal;
    v_material = (a_material + 0.5) / u_material_count;
})";
        constexpr const char *main_f = R"(
VARYING(vec3, pos)
VARYING(vec3, normal)
VARYING(float, material)

const float PI = 3.14159265359;

float DistributionGGX(vec3 N, vec3 H, float roughness)
{
    float a      = roughness*roughness;
    float a2     = a*a;
    float NdotH  = max(dot(N, H), 0.0);
    float NdotH2 = NdotH*NdotH;

    float nom   = a2;
    float denom = (NdotH2 * (a2 - 1.0) + 1.0);
    denom = PI * denom * denom;

    return nom / denom;
}
float GeometrySchlickGGX(float NdotV, float roughness)
{
    float r = (roughness + 1.0);
    float k = (r*r) / 8.0;

    float nom   = NdotV;
    float denom = NdotV * (1.0 - k) + k;

    return nom / denom;
}
float GeometrySmith(vec3 N, vec3 V, vec3 L, float roughness)
{
    float NdotV = max(dot(N, V), 0.0);
    float NdotL = max(dot(N, L), 0.0);
    float ggx2  = GeometrySchlickGGX(NdotV, roughness);
    float ggx1  = GeometrySchlickGGX(NdotL, roughness);

    return ggx1 * ggx2;
}
vec3 fresnelSchlick(float cosTheta, vec3 F0)
{
    return F0 + (1.0 - F0) * pow(1.0 - cosTheta, 5.0);
}
vec3 fresnelSchlickRoughness(float cosTheta, vec3 F0, float roughness)
{
    return F0 + (max(vec3(1.0 - roughness), F0) - F0) * pow(1.0 - cosTheta, 5.0);
}

void main()
{
    vec3 albedo = texture2D(u_materials, vec2(v_material, 0.25)).xyz;
    vec3 stats = texture2D(u_materials, vec2(v_material, 0.75)).xyz;
    float metallic  = stats.x;
    float roughness = stats.y;
    float ao        = stats.z;

    vec3 N = normalize(v_normal);
    vec3 V = normalize(u_camera - v_pos);

    vec3 F0 = vec3(0.04);
    F0 = mix(F0, albedo, metallic);

    // reflectance equation
    vec3 Lo = vec3(0.0);

    if (1==1)
    {
            vec3 light_pos = vec3(5,0,5);
            vec3 light_color = vec3(1,0.7,0)*500.0*2;

        // calculate per-light radiance
        vec3 L = normalize(light_pos - v_pos);
        vec3 H = normalize(V + L);
        float distance    = length(light_pos - v_pos);
        float attenuation = 1.0 / (distance * distance);
        vec3 radiance     = light_color * attenuation;

        // cook-torrance brdf
        float NDF = DistributionGGX(N, H, roughness);
        float G   = GeometrySmith(N, V, L, roughness);
        vec3 F    = fresnelSchlick(max(dot(H, V), 0.0), F0);

        vec3 kS = F;
        vec3 kD = vec3(1.0) - kS;
        kD *= 1.0 - metallic;

        vec3 nominator    = NDF * G * F;
        float denominator = 4 * max(dot(N, V), 0.0) * max(dot(N, L), 0.0) + 0.001;
        vec3 specular     = nominator / denominator;

        // add to outgoing radiance Lo
        float NdotL = max(dot(N, L), 0.0);
        Lo += (kD * albedo / PI + specular) * radiance * NdotL;
    }

    // ambient lighting (we now use IBL as the ambient term)
    vec3 F = fresnelSchlickRoughness(max(dot(N, V), 0.0), F0, roughness);

    vec3 kS = F;
    vec3 kD = 1.0 - kS;
    kD *= 1.0 - metallic;

    vec3 irradiance = u_background;
    vec3 diffuse      = irradiance * albedo;

    // sample both the pre-filter map and the BRDF lut and combine them together as per the Split-Sum approximation to get the IBL specular part.
    vec3 prefilteredColor = u_background; // for reflections
    vec2 brdf = texture2D(u_lookup, vec2(max(dot(N, V), 0.0), roughness)).rg;
    vec3 specular = prefilteredColor * (F * brdf.x + brdf.y);

    vec3 ambient = (kD * diffuse + specular) * ao;

    vec3 color = ambient + Lo;

    gl_FragColor = vec4(color, 1.0);
})";
    }
    inline namespace ExtractBright
    {
        constexpr const char *extract_bright_v = R"(
VARYING(vec2, pos)
void main()
{
    v_pos = a_pos;
    gl_Position = vec4(a_pos * 2. - 1., 0, 1);
})";
        constexpr const char *extract_bright_f = R"(
VARYING(vec2, pos)
void main()
{
    vec3 color = texture2D(u_texture, v_pos).rgb;
    color = vec3(1.0) - exp(-color * exp(u_exposure));
    float b = dot(color, vec3(0.2126, 0.7152, 0.0722));
    b -= u_threshold;
    gl_FragColor = vec4(color * b, 0);
})";
    }
    inline namespace Blur
    {
        constexpr const char *blur_v = R"(
VARYING(vec2, pos)
void main()
{
    v_pos = a_pos;
    gl_Position = vec4(a_pos * 2. - 1., 0, 1);
})";
        constexpr const char *blur_f = R"(
VARYING(vec2, pos)
#define R 7
void main()
{
    const float c[R] = float[](0.149446, 0.139483, 0.11333, 0.0799976, 0.0488874, 0.0257302, 0.0115786);
    vec3 color = texture2D(u_texture, v_pos).rgb * c[0];
    if (u_vertical)
    {
        for(int i = 1; i < R; ++i)
        {
            float s = u_step.y * i;
            s *= 3;
            color += texture(u_texture, v_pos + vec2(0.0, s)).rgb * c[i];
            color += texture(u_texture, v_pos - vec2(0.0, s)).rgb * c[i];
        }
    }
    else
    {
        for(int i = 1; i < R; ++i)
        {
            float s = u_step.x * i;
            s *= 3;
            color += texture(u_texture, v_pos + vec2(s, 0.0)).rgb * c[i];
            color += texture(u_texture, v_pos - vec2(s, 0.0)).rgb * c[i];
        }
    }
    gl_FragColor = vec4(color,1);
})";
    }
    inline namespace Identity
    {
        constexpr const char *identity_v = R"(
VARYING(vec2, pos)
void main()
{
    v_pos = a_pos;
    gl_Position = vec4(a_pos * 2. - 1., 0, 1);
})";
        constexpr const char *identity_f = R"(
VARYING(vec2, pos)
void main()
{
    vec4 c = texture2D(u_texture, v_pos);
    gl_FragColor = vec4(c.rgb * u_factor, c.a);
})";
    }
    inline namespace Final
    {
        constexpr const char *final_v = R"(
VARYING(vec2, pos)
void main()
{
    v_pos = a_pos;
    gl_Position = vec4(a_pos * 2. - 1., 0, 1);
})";
        constexpr const char *final_f = R"(
VARYING(vec2, pos)
void main()
{
    vec4 tex_color = texture2D(u_texture, v_pos);
    vec3 color = tex_color.rgb;
    color = vec3(1.0) - exp(-color * exp(u_exposure));
    color = pow(color, vec3(1.0/2.2));
    gl_FragColor = vec4(color, tex_color.a);
})";
    }
}

void FullscreenQuad()
{
    static Graphics::VertexBuffer<AttributesTex> vbuf;
    static bool first = 1;
    if (first)
    {
        first = 0;
        vbuf.Create();
        AttributesTex arr[]{{{-1,-1}},{{-1,10}},{{10,-1}}};
        vbuf.SetData(3, arr);
    }
    vbuf.Draw(Graphics::triangles);
}

void Init()
{
    win.Create("Woah", {800,600});

    shader_main.Create<AttributesMain>(ShaderSource::main_v, ShaderSource::main_f, &uniforms_main);
    shader_extract_bright.Create<AttributesTex>(ShaderSource::extract_bright_v, ShaderSource::extract_bright_f, &uniforms_extract_bright);
    shader_blur.Create<AttributesTex>(ShaderSource::blur_v, ShaderSource::blur_f, &uniforms_blur);
    shader_identity.Create<AttributesTex>(ShaderSource::identity_v, ShaderSource::identity_f, &uniforms_identity);
    shader_final.Create<AttributesTex>(ShaderSource::final_v, ShaderSource::final_f, &uniforms_final);

    uniforms_main.projection = fmat4::perspective(to_rad(85), win.Size().ratio(), 0.1, 100);
    fmat4 view = fmat4::look_at({0,0,5},{0,0,0},{0,1,0});
    uniforms_main.view = view;
    uniforms_main.model = fmat4::identity();
    uniforms_main.normal = fmat3::identity();
    uniforms_main.camera = {0,0,5};

    uniforms_main.background = (fvec3(127,209,255)/255).apply([](float x){return std::pow(x,2.2);});

    { // Load materials
        auto mat_img_raw = Graphics::Image::File("assets/materials.png");
        if (mat_img_raw.Size().y != 4)
            Program::Error("Bad material texture: H must be 4.");
        int mat_count = mat_img_raw.Size().x;
        auto mat_img = Graphics::Image::Memory({mat_count, 2});
        for (int x = 0; x < mat_count; x++)
        {
            mat_img.FastSet({x,0}, mat_img_raw.FastGet({x,0}));
            auto metallic          = mat_img_raw.FastGet({x,1});
            auto roughness         = mat_img_raw.FastGet({x,2});
            auto ambient_occlusion = mat_img_raw.FastGet({x,3});
            for (const auto &it : {metallic, roughness, ambient_occlusion})
                if (it.x != it.y || it.y != it.z || it.w != 255)
                    Program::Error(Str("Bad material texture: Properties are not greyscale for index ", x, "."));
            mat_img.FastSet({x,1}, u8vec4(metallic.x, roughness.x, ambient_occlusion.x, 0));
        }

        tex_materials.Create();
        tex_materials.Interpolation(Graphics::Texture::nearest);
        tex_materials.SetData(mat_img);

        uniforms_main.materials = tex_materials;
        uniforms_main.material_count = mat_img.Size().x;
    }

    tex_lookup.Create();
    tex_lookup.Interpolation(Graphics::Texture::linear);
    tex_lookup.Wrap(Graphics::Texture::clamp);
    tex_lookup.SetData(Graphics::Image::File("assets/lookup.png"));
    uniforms_main.lookup = tex_lookup;

    tex_framebuffer_hdr.Create();
    tex_framebuffer_hdr.SetData(GL_RGB16F, GL_RGB, GL_UNSIGNED_BYTE, win.Size());
    tex_framebuffer_hdr.Interpolation(Graphics::Texture::linear);
    tex_framebuffer_hdr_depth.Create();
    tex_framebuffer_hdr_depth.SetData(GL_DEPTH_COMPONENT, GL_DEPTH_COMPONENT, GL_UNSIGNED_BYTE, win.Size());
    tex_framebuffer_hdr_depth.Interpolation(Graphics::Texture::linear);
    framebuffer_hdr.Create();
    framebuffer_hdr.Attach(tex_framebuffer_hdr);
    framebuffer_hdr.Attach(tex_framebuffer_hdr_depth,-1);
    framebuffer_hdr.Unbind();

    for (auto it : {&tex_framebuffer_bloom1, &tex_framebuffer_bloom2})
    {
        it->Create();
        it->SetData(GL_RGB8, GL_RGB, GL_UNSIGNED_BYTE, win.Size());
        it->Interpolation(Graphics::Texture::linear);
        it->Wrap(Graphics::Texture::clamp);
    }
    framebuffer_bloom1.Create();
    framebuffer_bloom2.Create();
    framebuffer_bloom1.Attach(tex_framebuffer_bloom1);
    framebuffer_bloom2.Attach(tex_framebuffer_bloom2);

    uniforms_extract_bright.texture = tex_framebuffer_hdr;
    uniforms_extract_bright.exposure = 0;
    uniforms_extract_bright.threshold = 0.4;

    uniforms_blur.step = 1. / win.Size();

    uniforms_identity.texture = tex_framebuffer_bloom1;
    uniforms_identity.factor = 1.75;

    uniforms_final.texture = tex_framebuffer_hdr;
    uniforms_final.exposure = 0;
}

[[deprecated("Use this once and then use the prerendered texture.")]]
void PrecomputeLookupTexture(int size, float x_offset, std::string fname) // `size` probably should be 256. `x_offset` should be 1/size, or smaller if you're careful. If it's 0, the first column becomes black.
{
    auto RadicalInverse_VdC = [&](uint32_t bits) -> float
    {
        bits = (bits << 16u) | (bits >> 16u);
        bits = ((bits & 0x55555555u) << 1u) | ((bits & 0xAAAAAAAAu) >> 1u);
        bits = ((bits & 0x33333333u) << 2u) | ((bits & 0xCCCCCCCCu) >> 2u);
        bits = ((bits & 0x0F0F0F0Fu) << 4u) | ((bits & 0xF0F0F0F0u) >> 4u);
        bits = ((bits & 0x00FF00FFu) << 8u) | ((bits & 0xFF00FF00u) >> 8u);
        return float(bits) * 2.3283064365386963e-10; // / 0x100000000
    };
    auto Hammersley = [&](uint32_t i, uint32_t N) -> fvec2
    {
        return fvec2(float(i)/float(N), RadicalInverse_VdC(i));
    };

    auto ImportanceSampleGGX = [&](fvec2 Xi, fvec3 N, float roughness) -> fvec3
    {
        float a = roughness*roughness;

        float phi = 2.0 * f_pi * Xi.x;
        float cosTheta = std::sqrt((1.0 - Xi.y) / (1.0 + (a*a - 1.0) * Xi.y));
        float sinTheta = std::sqrt(1.0 - cosTheta*cosTheta);

        // from spherical coordinates to cartesian coordinates
        fvec3 H;
        H.x = std::cos(phi) * sinTheta;
        H.y = std::sin(phi) * sinTheta;
        H.z = cosTheta;

        // from tangent-space vector to world-space sample vector
        fvec3 up        = abs(N.z) < 0.999 ? fvec3(0.0, 0.0, 1.0) : fvec3(1.0, 0.0, 0.0);
        fvec3 tangent   = (up /cross/ N).norm();
        fvec3 bitangent = N /cross/ tangent;

        fvec3 sampleVec = tangent * H.x + bitangent * H.y + N * H.z;
        return sampleVec.norm();
    };

    auto G_SchlickGGX = [&](float dot, float roughness) -> float
    {
        float a = roughness;
        float k = (a * a) / 2.0;

        float nom   = dot;
        float denom = dot * (1.0 - k) + k;

        return nom / denom;
    };

    auto G_Smith = [&](float roughness, float NoV, float NoL) -> float
    {
        float ggx2 = G_SchlickGGX(NoV, roughness);
        float ggx1 = G_SchlickGGX(NoL, roughness);

        return ggx1 * ggx2;
    };

    auto IntegrateBRDF = [&](float Roughness, float NoV) -> fvec2
    {
        fvec3 V;
        V.x = std::sqrt(1.0f - NoV * NoV); // sin
        V.y = 0;
        V.z = NoV; // cos
        float A = 0;
        float B = 0;
        fvec3 N = fvec3(0,0,1);
        const int NumSamples = 1024;
        for (int i = 0; i < NumSamples; i++)
        {
            fvec2 Xi = Hammersley(i, NumSamples);
            fvec3 H = ImportanceSampleGGX(Xi, N, Roughness);
            fvec3 L = 2 * (V /dot/ H) * H - V;
            float NoL = clamp(L.z);
            float NoH = clamp(H.z);
            float VoH = clamp(V /dot/ H);
            if( NoL > 0 )
            {
                float G = G_Smith(Roughness, NoV, NoL);
                float G_Vis = G * VoH / (NoH * NoV);
                float Fc = ipow(1 - VoH, 5);
                A += (1 - Fc) * G_Vis;
                B += Fc * G_Vis;
            }
        }
        return fvec2(A, B) / NumSamples;
    };

    auto img = Graphics::Image::Memory(ivec2(size));
    for (int y = 0; y < size; y++)
    {
        for (int x = 0; x < size; x++)
            img.FastSet(ivec2(x,y), iround(IntegrateBRDF(y / float(size-1), x_offset + x / float(size-1) * (1 - x_offset)) * 255).to<uint8_t>().to_vec4(0,255));
        std::cout << (y+1) << "/" << size << '\n';;
    }
    img.SaveToFile(Graphics::Image::png, fname);
}

int main(int, char **)
{
    Init();

    Graphics::VertexBuffer<AttributesMain> buf = LoadModel("assets/untitled.obj");

    fquat q = fquat::around_axis({1,0,0}, f_pi / 6);

    Graphics::SetClearColor((fvec3(127,209,255)/255).apply([](float x){return std::pow(x,2.2);}));

    while (1)
    {
        Events::Process();

        if (Input::Keys::f11.pressed())
            win.ToggleFullscreen();

        Graphics::CheckErrors();

        Graphics::Blending::Enable();
        Graphics::Blending::FuncNormalPre();
        Graphics::Depth(1);
        framebuffer_hdr.Bind();
        shader_main.Bind();

        Graphics::Clear(Graphics::color | Graphics::depth);

        q = q /mul/ fquat::around_axis({0,1,0}, 0.005);
        q.normalize();
        fmat4 m = q.make_mat4();

        uniforms_main.model = m;
        uniforms_main.normal = m.to_mat3().inverse().transpose();
        buf.Draw(Graphics::triangles);

        Graphics::Blending::Disable();
        Graphics::Blending::FuncAdd();
        Graphics::Depth(0);

        framebuffer_bloom1.Bind();
        shader_extract_bright.Bind();
        FullscreenQuad();

        shader_blur.Bind();

        #if 0 // Render bloom only
        framebuffer_hdr.Bind();
        Graphics::SetClearColor(fvec3(0));
        Graphics::Clear(Graphics::color);
        Graphics::SetClearColor((fvec3(127,209,255)/255).apply([](float x){return std::pow(x,2.2);}));
        #endif


        for (int i = 0; i < 4; i++)
        {
            framebuffer_bloom2.Bind();
            uniforms_blur.vertical = 0;
            uniforms_blur.texture = tex_framebuffer_bloom1;
            FullscreenQuad();

            framebuffer_bloom1.Bind();
            uniforms_blur.vertical = 1;
            uniforms_blur.texture = tex_framebuffer_bloom2;
            FullscreenQuad();

            Graphics::Blending::Enable();
            framebuffer_hdr.Bind();
            shader_identity.Bind();
            FullscreenQuad();
            Graphics::Blending::Disable();
        }

        Graphics::FrameBuffer::Unbind();

        shader_final.Bind();

        FullscreenQuad();

        win.Swap();
    }

    return 0;
}
