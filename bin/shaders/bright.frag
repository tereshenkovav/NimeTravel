uniform sampler2D texture;
uniform float bright ;

void main()
{
    vec4 pixel = texture2D(texture, gl_TexCoord[0].xy)*bright ;
    gl_FragColor = pixel * gl_Color;
}
