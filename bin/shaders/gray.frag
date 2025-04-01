uniform sampler2D texture;

void main()
{
    vec4 pixel = texture2D(texture, gl_TexCoord[0].xy) ;
    pixel.r = (pixel.r + pixel.g + pixel.b) / 3.0 ;
    pixel.g = pixel.r ;
    pixel.b = pixel.r ;
    gl_FragColor = pixel * gl_Color;
}
