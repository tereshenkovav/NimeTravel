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
    procedure DrawLine(pixels:TUniList<TPixel>;x1,y1,x2,y2:Integer) ;
  public
    constructor Create(Aimg:TSfmlImage) ;
    function GenAura(freqanim:Integer):TUniList<TSfmlSprite> ;
  end ;

implementation
uses Math ;

constructor TAuraMaker.Create(Aimg:TSfmlImage) ;
begin
  img:=Aimg ;
end ;

procedure TAuraMaker.DrawLine(pixels: TUniList<TPixel>; x1, y1, x2, y2: Integer);
var delta:Single ;
    x,y:Single ;
    c:TSfmlColor ;
begin
  c:=SfmlColorFromRGBA(255,255,255,255) ;
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

function TAuraMaker.GenAura(freqanim:Integer):TUniList<TSfmlSprite> ;
var tex:TSfmlTexture ;
    spr:TSfmlSprite ;
    i,j:Integer ;
    rcount,dcount:Integer ;
    rads,newrads:array of Integer ;
    r,rlast:Integer ;
    aura:TSfmlImage ;
    x,y,cx,cy:Integer ;
    a,q,k:Single ;
    pixels:TUniList<TPixel> ;
    p:TPixel ;
    minx,miny,maxx,maxy,dx,dy,neww,newh:Integer ;
    frame,fcount:Integer ;
    freqr:Integer ;
    i1,i2,maxr:Integer ;
    xold,yold:Integer ;
begin
  Result:=TUniList<TSfmlSprite>.Create() ;

  fcount:=12 ; // ����� ������� ��������
  rcount:=48 ; // ����� ����� ��� �����������
  dcount:=12 ;  // ����� �������� ����� ������
  freqr:=2 ; // �������� ������� ��������� ������� ����

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
    if j>0 then DrawLine(pixels,xold,yold,x,y) ;
    xold:=x ;
    yold:=y ;
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
