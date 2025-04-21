unit AuraMaker ;

interface
uses SfmlSystem, SfmlGraphics,
  Helpers ;

type
  TImageShifted = record
    originx:Integer ;
    originy:Integer ;
    img:TSfmlImage ;
  end;

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
    function ImageShifted(originx,originy:Integer; img:TSfmlImage):TImageShifted ;
    procedure DrawLine(pixels:TUniList<TPixel>;x1,y1,x2,y2:Integer; c:TSfmlColor) ;
    class procedure CopyImageToImage(dst,src:TSfmlImage; x,y:Integer) ;
  public
    constructor Create(Aimg:TSfmlImage) ;
    function GenAuraImages(freqanim,magicborder:Integer):TUniList<TImageShifted> ;
    class function ImagesToSprites(images:TUniList<TImageShifted>):TUniList<TSfmlSprite> ;
    class function ImagesToSolidImage(images:TUniList<TImageShifted>):TImageShifted ;
  end ;

implementation
uses Math ;

class procedure TAuraMaker.CopyImageToImage(dst, src: TSfmlImage; x,
  y: Integer);
var i,j:Integer ;
begin
  for i := 0 to src.Size.X-1 do
   for j := 0 to src.Size.Y-1 do
     dst.Pixel[x+i,y+j]:=src.Pixel[i,j] ;
end;

constructor TAuraMaker.Create(Aimg:TSfmlImage) ;
begin
  img:=Aimg ;
end ;

class function TAuraMaker.ImagesToSprites(images:TUniList<TImageShifted>):TUniList<TSfmlSprite> ;
var img:TImageShifted ;
    tex:TSfmlTexture ;
    spr:TSfmlSprite ;
begin
  Result:=TUniList<TSfmlSprite>.Create() ;
  for img in images do begin
    tex:=TSfmlTexture.Create(img.img.Handle) ;
    spr:=TSfmlSprite.Create(tex) ;
    spr.Origin:=SfmlVector2f(img.originx,img.originy) ;
    Result.Add(spr) ;
  end;
end;

function TAuraMaker.ImageShifted(originx, originy: Integer;
  img: TSfmlImage): TImageShifted;
begin
  Result.originx:=originx ;
  Result.originy:=originy ;
  Result.img:=img ;
end;

class function TAuraMaker.ImagesToSolidImage(images:TUniList<TImageShifted>):TImageShifted ;
var img:TImageShifted ;
    maxl,maxr,maxt,maxb,maxw,maxh:Integer ;
    i:Integer;
begin

  maxl:=0 ;
  maxr:=0 ;
  maxt:=0 ;
  maxb:=0 ;
  for img in images do begin
    if maxl<img.originx then maxl:=img.originx ;

    if maxr<img.img.Size.X-img.originx then maxr:=img.img.Size.X-img.originx ;
    if maxt<img.originy then maxt:=img.originy ;

    if maxb<img.img.Size.Y-img.originy then maxb:=img.img.Size.Y-img.originy ;
  end;

  maxw:=maxl+maxr+2 ;

  maxh:=maxt+maxb+2 ;

  Result.img:=TSfmlImage.Create(images.Count*maxw,maxh,SfmlColorFromRGBA(0,0,0,0)) ;

  Result.originx:=maxl+1 ;

  Result.originy:=maxt+1 ;

  for i := 0 to images.Count-1 do

    CopyImageToImage(Result.img,images[i].img,
      i*maxw+maxl-images[i].originx+1,maxt-images[i].originy+1) ;
end;

procedure TAuraMaker.DrawLine(pixels: TUniList<TPixel>; x1, y1, x2, y2: Integer;
  c:TSfmlColor);
var delta:Single ;
    x,y:Single ;
begin
  if (x1=x2)and(y1=y2) then begin
    pixels.Add(Pixel(x1,y1,c)) ;
    Exit ;
  end;

  x:=x1 ;
  y:=y1 ;
  if Abs(x2-x1)>Abs(y2-y1) then begin
    delta:=(y2-y1)/Abs(x2-x1) ;
    repeat
      pixels.Add(Pixel(Round(x),Round(y),c)) ;
      x:=x+Sign(x2-x1) ;
      y:=y+delta ;
    until x=x2;
  end
  else begin
    delta:=(x2-x1)/Abs(y2-y1) ;
    repeat
      pixels.Add(Pixel(Round(x),Round(y),c)) ;
      x:=x+delta ;
      y:=y+Sign(y2-y1) ;
    until y=y2;
  end;
  pixels.Add(Pixel(x2,y2,c)) ;
end;

function TAuraMaker.GenAuraImages(freqanim,magicborder:Integer):TUniList<TImageShifted> ;
var i,j:Integer ;
    rcount,dcount:Integer ;
    rads,newrads:array of Integer ;
    r,rlast:Integer ;
    aura:TSfmlImage ;
    x,y,cx,cy:Integer ;
    a,q,k:Single ;
    border,pixels,newpixels:TUniList<TPixel> ;
    p:TPixel ;
    minx,miny,maxx,maxy,dx,dy,neww,newh:Integer ;
    frame,fcount:Integer ;
    freqr:Integer ;
    i1,i2,maxr:Integer ;
    xold,yold,xfirst,yfirst:Integer ;
    cborder,cfill:TSfmlColor ;
    dist,mindist:Single ;
    sr,sg,sb,sa:Single ;
    x1,y1:Integer ;
    gaussmatrix:array of array of Single ;
    v,sum:Single ;

function isEqual(c1,c2:TSfmlColor):Boolean ;
begin
  Result:=(c1.r=c2.r)and(c1.g=c2.g)and(c1.b=c2.b)and(c1.a=c2.a) ;
end;

begin
  Result:=TUniList<TImageShifted>.Create() ;
  cborder:=SfmlColorFromRGBA(255,255,255,255) ;
  cfill:=SfmlColorFromRGBA(176,0,96,128) ;

  // Заполнение матрицы Гаусса
  SetLength(gaussmatrix,3) ;
  sum:=0 ;
  for i:= 0 to 2 do begin
    SetLength(gaussmatrix[i],3) ;
    for j:= 0 to 2 do begin
      v := Exp(-((i-1)*(i-1)+(j-1)*(j-1))/(2.0*1.5*1.5));
      gaussmatrix[i,j]:=v ;
			sum:=sum+v ;
    end ;
  end ;
  for i:= 0 to 2 do
    for j:= 0 to 2 do
      gaussmatrix[i,j]:=gaussmatrix[i,j]/sum ;

  fcount:=12 ; // Число фреймов анимации
  rcount:=48 ; // Число лучей для трассировки
  dcount:=12 ;  // Число отрезков между лучами
  freqr:=2 ; // Половина размера колебаний радиуса луча

  SetLength(rads,rcount) ;
  SetLength(newrads,rcount) ;
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

  for i := 0 to rcount-1 do
    newrads[i]:=rads[i] ;

  for j := 0 to 3 do begin

  for i := 0 to rcount-1 do begin
    i1:=i-1 ;
    if i1<0 then i1:=rcount-1 ;
    i2:=i+1 ;
    if i2>=rcount then i2:=0 ;
    maxr:=Max(rads[i1],rads[i2]) ;
    if maxr>rads[i] then newrads[i]:=rads[i]+Round(0.5*(maxr-rads[i])) ;
  end;

  for i := 0 to rcount-1 do
    rads[i]:=newrads[i] ;

  end;


  for frame := 0 to fcount-1 do begin

  border:=TUniList<TPixel>.Create ;

  q:=2*PI*(frame/fcount) ;
  for i := 0 to rcount-1 do begin
    for j := 0 to dcount-1 do begin
    r:=Round((rads[i]+(rads[(i+1) mod rcount]-rads[i])*(j/(dcount-1)))) ;
    r:=r+Round(freqr+freqr*Sin(q)) ;
    q:=q-freqanim*2*PI/(rcount*dcount) ;
    a:=2*PI*i/rcount+ (2*PI/rcount)*(j/dcount) ;
    x:=cx+Round(r*Cos(a)) ;
    y:=cy+Round(r*Sin(a)) ;
    if not((i=0)and(j=0)) then
      DrawLine(border,xold,yold,x,y,cborder)
    else begin
      xfirst:=x ;
      yfirst:=y ;
    end ;
    xold:=x ;
    yold:=y ;
    end;
  end;
  DrawLine(border,xold,yold,xfirst,yfirst,cborder) ;

  minx:=border[0].x ;
  maxx:=border[0].x ;
  miny:=border[0].y ;
  maxy:=border[0].y ;
  for i:=1 to border.Count-1 do begin
    if minx>border[i].x then minx:=border[i].x ;
    if maxx<border[i].x then maxx:=border[i].x ;
    if miny>border[i].y then miny:=border[i].y ;
    if maxy<border[i].y then maxy:=border[i].y ;
  end;

  if minx<0 then dx:=-minx else dx:=0 ;
  if miny<0 then dy:=-miny else dy:=0 ;

  neww:=Max(img.Size.X,maxx+1)+dx ;
  newh:=Max(img.Size.Y,maxy+1)+dy ;

  aura:=TSfmlImage.Create(neww,newh,SfmlColorFromRGBA(0,0,0,0)) ;
  for p in border do
    aura.Pixel[p.x+dx,p.y+dy]:=p.c ;

  pixels:=TUniList<TPixel>.Create ;
  newpixels:=TUniList<TPixel>.Create ;
  pixels.Add(Pixel(cx,cy,cfill)) ;
  aura.Pixel[cx,cy]:=cfill ;
  while True do begin
    for p in pixels do begin
       if p.x<1 then continue ;
       if p.y<1 then continue ;
       if p.x>neww-2 then continue ;
       if p.y>newh-2 then continue ;
       if aura.Pixel[p.x-1,p.y].a=0 then begin
          newpixels.Add(Pixel(p.x-1,p.y,cfill)) ;
          aura.Pixel[p.x-1,p.y]:=cfill ;
       end ;
       if aura.Pixel[p.x+1,p.y].a=0 then begin
          newpixels.Add(Pixel(p.x+1,p.y,cfill)) ;
          aura.Pixel[p.x+1,p.y]:=cfill ;
       end ;
       if aura.Pixel[p.x,p.y-1].a=0 then begin
          newpixels.Add(Pixel(p.x,p.y-1,cfill)) ;
          aura.Pixel[p.x,p.y-1]:=cfill ;
       end ;
       if aura.Pixel[p.x,p.y+1].a=0 then begin
          newpixels.Add(Pixel(p.x,p.y+1,cfill)) ;
          aura.Pixel[p.x,p.y+1]:=cfill ;
       end ;
    end ;
    if newpixels.Count=0 then Break ;
    pixels.Clear() ;
    for p in newpixels do
      pixels.Add(p) ;
    newpixels.Clear() ;
  end;
  pixels.Free ;
  newpixels.Free ;

  for x := 0 to neww-1 do
    for y := 0 to newh-1 do
      if isEqual(aura.Pixel[x,y],cborder) then
        aura.Pixel[x,y]:=SfmlColorFromRGBA(0,0,0,0) ;

  for x := 0 to neww-1 do
    for y := 0 to newh-1 do
      if isEqual(aura.Pixel[x,y],cfill) then begin
        mindist:=-1 ;
        for p in border do begin
          dist:=(p.x+dx-x)*(p.x+dx-x)+(p.y+dy-y)*(p.y+dy-y) ;
          if (dist<mindist)or(mindist=-1) then mindist:=dist ;
        end ;
        if (mindist<magicborder*magicborder) then
          aura.Pixel[x,y]:=SfmlColorFromRGBA(cfill.R,cfill.G,cfill.B,
            Round(mindist*(cfill.A-255)/(magicborder*magicborder)+255))
      end ;

  pixels:=TUniList<TPixel>.Create() ;
  for x := 0 to neww-1 do
    for y := 0 to newh-1 do begin
      mindist:=-1 ;
      for p in border do begin
        dist:=(p.x+dx-x)*(p.x+dx-x)+(p.y+dy-y)*(p.y+dy-y) ;
        if (dist<mindist)or(mindist=-1) then mindist:=dist ;
      end ;
      if mindist<=9 then begin
        sr:=0 ; sg:=0 ; sb:=0 ; sa:=0 ;
        for x1 := x-1 to x+1 do
          for y1 := y-1 to y+1 do begin
            if x1<0 then continue ;
            if x1>=neww then continue ;
            if y1<0 then continue ;
            if y1>=newh then continue ;
            sr:=sr+gaussmatrix[x1-x+1,y1-y+1]*aura.Pixel[x1,y1].r ;
            sg:=sg+gaussmatrix[x1-x+1,y1-y+1]*aura.Pixel[x1,y1].g ;
            sb:=sb+gaussmatrix[x1-x+1,y1-y+1]*aura.Pixel[x1,y1].b ;
            sa:=sa+gaussmatrix[x1-x+1,y1-y+1]*aura.Pixel[x1,y1].a ;
          end ;
        pixels.Add(Pixel(x,y,SfmlColorFromRGBA(Round(sr),Round(sg),Round(sb),Round(sa)))) ;
      end ;
    end ;

  for p in pixels do
    aura.Pixel[p.x,p.y]:=p.c ;

  pixels.Free ;

  border.Free ;

  Result.Add(ImageShifted(img.Size.X div 2+dx,img.Size.Y div 2+dy,aura)) ;
  end;
end ;

function TAuraMaker.Pixel(x, y: Integer; c: TSfmlColor): TPixel;
begin
  Result.x:=x ;
  Result.y:=y ;
  Result.c:=c ;
end;

end.
