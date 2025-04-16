unit AuraMaker ;

interface
uses SfmlSystem, SfmlGraphics,
  Helpers ;

type
  TAuraMaker = class
  private
    img:TSfmlImage ;
  public
    constructor Create(Aimg:TSfmlImage) ;
    function GenAura():TUniList<TSfmlSprite> ;
  end ;

implementation

constructor TAuraMaker.Create(Aimg:TSfmlImage) ;
begin
  img:=Aimg ;
end ;

function TAuraMaker.GenAura():TUniList<TSfmlSprite> ;
var tex:TSfmlTexture ;
    spr:TSfmlSprite ;
    i,j:Integer ;
begin
  Result:=TUniList<TSfmlSprite>.Create() ;

  for i := 0 to img.Size.X-1 do
    for j := 0 to img.Size.Y-1 do
     img.Pixel[i,j]:=SfmlColorFromRGBA(100,0,0,128) ;

  tex:=TSfmlTexture.Create(img.Handle) ;
  spr:=TSfmlSprite.Create(tex) ;
  spr.Origin:=SfmlVector2f(img.Size.X/2,img.Size.Y/2) ;
  Result.Add(spr) ;

  for i := 0 to img.Size.X-1 do
    for j := 0 to img.Size.Y-1 do
     img.Pixel[i,j]:=SfmlColorFromRGBA(100,0,0,255) ;

  tex:=TSfmlTexture.Create(img.Handle) ;
  spr:=TSfmlSprite.Create(tex) ;
  spr.Origin:=SfmlVector2f(img.Size.X/2,img.Size.Y/2) ;
  Result.Add(spr) ;
end ;

end.
