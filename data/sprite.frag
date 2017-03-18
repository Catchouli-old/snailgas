#version 330 core

#extension GL_ARB_explicit_uniform_location : enable

in vec2 var_uv;

out vec3 out_color;

layout (location = 0) uniform sampler2D sampler_tex;

void main(){
  out_color = vec3(texture(sampler_tex, var_uv));
}

