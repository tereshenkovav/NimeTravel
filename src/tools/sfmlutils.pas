unit sfmlutils ;

interface
uses SfmlGraphics,SfmlSystem ;

type
  TSpriteLoaderOption = (sloCentered,sloNoSmooth,sloMipmap) ;
  TSpriteLoaderOptions = set of TSpriteLoaderOption ;

function loadSprite(filename:string):TSfmlSprite ; overload ;
function loadSprite(filename:string; options:TSpriteLoaderOptions):TSfmlSprite ; overload ;
function createSFMLColor(color:Cardinal):TSfmlColor ;
function createSFMLColorAlpha(color:Cardinal; alpha:Byte):TSfmlColor ;
function SfmlVector2i(X, Y: Integer): TSfmlVector2i;

implementation
uses SysUtils ;

function loadSprite(filename:string):TSfmlSprite ;
begin
  Result:=loadSprite(filename,[]) ;
end ;

function loadSprite(filename:string; options:TSpriteLoaderOptions):TSfmlSprite ;
var tex:TSfmlTexture ;
begin
  Result:=TSfmlSprite.Create;
  tex:=TSfmlTexture.Create(filename) ;
  if not(sloNoSmooth in options) then tex.Smooth:=True ;
  if (sloMipmap in options) then tex.GenerateMipmap() ;
  Result.SetTexture(tex,True) ;
  if (sloCentered in options) then
    Result.Origin:=SfmlVector2f(tex.Size.X/2,tex.Size.Y/2) ;
end ;

function createSFMLColor(color:Cardinal):TSfmlColor ;
begin
  Result.A:=$FF ;
  Result.B:=color and $FF ;
  Result.G:=(color shr 8) and $FF ;
  Result.R:=(color shr 16) and $FF ;
end;

function createSFMLColorAlpha(color:Cardinal; alpha:Byte):TSfmlColor ;
begin
  Result.A:=alpha ;
  Result.B:=color and $FF ;
  Result.G:=(color shr 8) and $FF ;
  Result.R:=(color shr 16) and $FF ;
end;

function SfmlVector2i(X, Y: Integer): TSfmlVector2i;
begin
  Result.X := X;
  Result.Y := Y;
end;

end.