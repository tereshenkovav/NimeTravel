unit sfmlanimation ;

interface
uses SfmlGraphics ;

type
  TPlayState = (psPlayed,psPlayedOnce,psStopped) ;

  TSfmlAnimation = class(TSfmlSprite)
  private
    frames:array of TSfmlTexture ;
	  FrameCount:Integer ;
	  FPS:Single ;
	  tekt:Single ;
    playstate:TPlayState ;
  public
    // По кадру из файла
    constructor Create(path:string; filenames:array of string; AFPS:Single) ; overload ;
    // Из файла с заданными размерами кадра
    constructor Create(filename:string; w,h,AFrameCount:Integer; AFPS:Single) ; overload ;
    // Из файла с авторасчетом (только для горизонтальных анимаций)
    constructor Create(filename:string; AFrameCount:Integer; AFPS:Single) ; overload ;
    destructor Destroy; override ;
    procedure Update(dt:Single) ;
    function isPlayed():Boolean ;
    procedure Play() ;
    procedure Stop() ;
    procedure PlayOnce() ;
  end;

implementation
uses helpers ;

{ TSfmlAnimation }

constructor TSfmlAnimation.Create(path:string; filenames: array of string; AFPS:Single);
var i:Integer ;
begin
  inherited Create() ;

  playstate:=psStopped ;
  FrameCount:=Length(filenames) ;
  FPS:=AFPS ;
  SetLength(frames,FrameCount) ;
  for i:=0 to FrameCount-1 do begin
    frames[i]:=TSfmlTexture.Create(path+PATH_SEP+filenames[i]);
    frames[i].Smooth:=True ;
  end;
  tekt:=0 ;
  Update(0) ;
end;

constructor TSfmlAnimation.Create(filename: string; w,h,AFrameCount:Integer; AFPS: Single);
var i:Integer ;
    img:TSfmlImage ;
    area:TSfmlIntRect ;
begin
  inherited Create() ;

  playstate:=psStopped ;
  FrameCount:=AFrameCount ;
  FPS:=AFPS ;
  SetLength(frames,FrameCount) ;

  img:=TSfmlImage.Create(filename) ;
  area.Width:=w ;
  area.Height:=h ;
  area.Left:=0 ;
  area.Top:=0 ;
  for i:=0 to FrameCount-1 do begin
    frames[i]:=TSfmlTexture.Create(img.Handle,@area) ;
    frames[i].Smooth:=True ;
    Inc(area.Left,w) ;
    if area.Left>=img.Size.X then begin
      area.Left:=0 ;
      Inc(area.Top,h) ;
    end ;
  end;
  tekt:=0 ;
  Update(0) ;
end;

constructor TSfmlAnimation.Create(filename: string; AFrameCount: Integer;
  AFPS: Single);
var i:Integer ;
    img:TSfmlImage ;
    area:TSfmlIntRect ;
begin
  inherited Create() ;

  playstate:=psStopped ;
  FrameCount:=AFrameCount ;
  FPS:=AFPS ;
  SetLength(frames,FrameCount) ;

  img:=TSfmlImage.Create(filename) ;
  area.Width:=img.Size.X div FrameCount ;
  area.Height:=img.Size.Y ;
  area.Left:=0 ;
  area.Top:=0 ;
  for i:=0 to FrameCount-1 do begin
    frames[i]:=TSfmlTexture.Create(img.Handle,@area) ;
    frames[i].Smooth:=True ;
    Inc(area.Left,area.Width) ;
  end;
  tekt:=0 ;
  Update(0) ;
end;

destructor TSfmlAnimation.Destroy;
var i:Integer ;
begin
  for i:=0 to FrameCount-1 do
    frames[i].Free ;
  SetLength(frames,0) ;
  inherited Destroy;
end;

function TSfmlAnimation.isPlayed: Boolean;
begin
  Result:=playstate<>psStopped ;
end;

procedure TSfmlAnimation.Play;
begin
  playstate:=psPlayed ;
end;

procedure TSfmlAnimation.PlayOnce;
begin
  playstate:=psPlayedOnce ;
end;

procedure TSfmlAnimation.Stop;
begin
  playstate:=psStopped ;
end;

procedure TSfmlAnimation.Update(dt: Single);
var sec_per_frame:Single ;
    tekframe:Integer ;
begin
  if not isPlayed() then Exit ;

	sec_per_frame := 1.0/FPS ;

	tekt:=tekt+dt ;
	tekframe := Round(tekt/sec_per_frame) ;
	if (tekframe>=FrameCount) then begin
		tekframe := 0 ;
		tekt:=tekt-FrameCount*sec_per_frame ;
    if playstate=psPlayedOnce then playstate:=psStopped ;

	end ;
	setTexture(frames[tekframe]) ;
end;

end.