unit AuraMaker ;

interface
uses SfmlSystem, SfmlGraphics,
  Helpers ;

type
  TAuraMaker = class
  private
    type
      TPixel = record
        x:Integer ;
        y:Integer ;
        c:TSfmlColor ;
      end ;
  private
    img:TSfmlImage ;
    function Pixel(x,y:Integer; c:TSfmlColor):TPixel ;
  public
    constructor Create(Aimg:TSfmlImage) ;
    function GenAura():TUniList<TSfmlSprite> ;
  end ;

implementation
uses Math ;

constructor TAuraMaker.Create(Aimg:TSfmlImage) ;
begin
  img:=Aimg ;
end ;

function TAuraMaker.GenAura():TUniList<TSfmlSprite> ;
var tex:TSfmlTexture ;
    spr:TSfmlSprite ;
    i,j:Integer ;
    rcount,dcount:Integer ;
    rads:array of Integer ;
    r,rlast:Integer ;
    aura:TSfmlImage ;
    x,y,cx,cy:Integer ;
    a,q,k:Single ;
    pixels:TUniList<TPixel> ;
    p:TPixel ;
    minx,miny,maxx,maxy,dx,dy,neww,newh:Integer ;
    frame,fcount:Integer ;
    freqanim,freqr:Integer ;
begin
  Result:=TUniList<TSfmlSprite>.Create() ;

  fcount:=12 ; // Число фреймов анимации
  rcount:=48 ; // Число лучей для трассировки
  dcount:=6 ;  // Число отрезков между лучами
  freqanim:=6 ; // Скорость сдвига между фреймами
  freqr:=3 ; // Половина размера колебаний радиуса луча

  SetLength(rads,rcount) ;
  cx:=img.Size.X div 2 ;
  cy:=img.Size.Y div 2 ;

  for i := 0 to rcount-1 do begin
    rlast:=-1 ;
    r:=Min(img.Size.X,img.Size.Y) div 4 ;
    while True do begin
    a:=2*PI*i/rcount ;
    x:=cx+Round(r*Cos(a)) ;
    y:=cy+Round(r*Sin(a)) ;
    if x<0 then break ;
    if x>=img.Size.X then break ;
    if y<0 then break ;
    if y>=img.Size.Y then break ;
    if img.Pixel[x,y].A=0 then begin
      if rlast=-1 then rlast:=r ;
    end
    else begin
      rlast:=-1 ;
    end;
    r:=r+1 ;
    end;
    if rlast=-1 then rlast:=r ;
    rads[i]:=rlast ;
  end;

  for frame := 0 to fcount-1 do begin

  pixels:=TUniList<TPixel>.Create ;

  q:=2*PI*(frame/fcount) ;
  for i := 0 to rcount-1 do begin
    for j := 0 to dcount-1 do begin
    r:=Round((rads[i]+(rads[(i+1) mod rcount]-rads[i])*(j/(dcount-1)))) ;
    r:=r+Round(freqr+freqr*Sin(q)) ;
    q:=q+freqanim*2*PI/(rcount*dcount) ;
    a:=2*PI*i/rcount+ (2*PI/rcount)*(j/dcount) ;
    x:=cx+Round(r*Cos(a)) ;
    y:=cy+Round(r*Sin(a)) ;
    pixels.Add(Pixel(x,y,SfmlColorFromRGBA(255,255,255,255))) ;
    end;
  end;

  minx:=pixels[0].x ;
  maxx:=pixels[0].x ;
  miny:=pixels[0].y ;
  maxy:=pixels[0].y ;
  for i:=1 to pixels.Count-1 do begin
    if minx>pixels[i].x then minx:=pixels[i].x ;
    if maxx<pixels[i].x then maxx:=pixels[i].x ;
    if miny>pixels[i].y then miny:=pixels[i].y ;
    if maxy<pixels[i].y then maxy:=pixels[i].y ;
  end;

  if minx<0 then dx:=-minx else dx:=0 ;
  if miny<0 then dy:=-miny else dy:=0 ;

  neww:=Max(img.Size.X,maxx+1)+dx ;
  newh:=Max(img.Size.Y,maxy+1)+dy ;

  aura:=TSfmlImage.Create(neww,newh,SfmlColorFromRGBA(0,0,0,0)) ;
  for p in pixels do
    aura.Pixel[p.x+dx,p.y+dy]:=p.c ;
  pixels.Free ;

  tex:=TSfmlTexture.Create(aura.Handle) ;
  aura.Free ;

  spr:=TSfmlSprite.Create(tex) ;
  spr.Origin:=SfmlVector2f(img.Size.X/2+dx,img.Size.Y/2+dy) ;
  Result.Add(spr) ;
  end;
end ;

function TAuraMaker.Pixel(x, y: Integer; c: TSfmlColor): TPixel;
begin
  Result.x:=x ;
  Result.y:=y ;
  Result.c:=c ;
end;

end.
