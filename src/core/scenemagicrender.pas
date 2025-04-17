unit SceneMagicRender;

interface

uses
  Classes, SysUtils,
  SfmlSystem,SfmlWindow,SfmlGraphics,
  Scene, Helpers ;

type

  { TSceneMagicRender }

  TSceneMagicRender = class(TScene)
  private
    sprites:TUniList<TSfmlSprite> ;
    auras:TUniList<TUniList<TSfmlSprite>> ;
    t:Single ;
    procedure LoadSpriteAndAura(const filename:string; x,y:Integer; freqanim,magicborder:Integer) ;
  public
    function Init():Boolean ; override ;
    function FrameFunc(dt:Single; events:TUniList<TSfmlEventEx>):TSceneResult ; override ;
    procedure RenderFunc() ; override ;
    procedure UnInit() ; override ;
  end;

implementation
uses SfmlUtils, AuraMaker ;

function TSceneMagicRender.Init():Boolean ;
begin
  sprites:=TUniList<TSfmlSprite>.Create() ;
  auras:=TUniList<TUniList<TSfmlSprite>>.Create() ;
  LoadSpriteAndAura('scenes\scene1\bush.png',150,100,12,7) ;
  LoadSpriteAndAura('scenes\scene1\grave_clover.png',350,100,6,5) ;
  LoadSpriteAndAura('scenes\scene4\book.png',500,100,6,5) ;
  LoadSpriteAndAura('scenes\scene6\fire_ico.png',600,100,3,5) ;
  LoadSpriteAndAura('scenes\scene5\colb.png',700,100,3,5) ;
  LoadSpriteAndAura('scenes\scene4\pony.png',210,400,24,10) ;
  LoadSpriteAndAura('scenes\scene5\pony.png',510,400,24,10) ;
  LoadSpriteAndAura('scenes\scene6\pony.png',700,400,24,10) ;
  t:=0 ;
end ;

procedure TSceneMagicRender.LoadSpriteAndAura(const filename: string; x,y:Integer;
  freqanim,magicborder:Integer);
var spr:TSfmlSprite ;
    img:TSfmlImage ;
    list:TUniList<TImageShifted> ;
begin
  spr:=loadSprite(filename,[sloCentered]) ;
  spr.Position:=SfmlVector2f(x,y) ;
  sprites.Add(spr) ;

  img:=TSfmlImage.Create(filename) ;
  with TAuraMaker.Create(img) do begin
    list:=GenAuraImages(freqanim,magicborder) ;
    auras.Add(TAuraMaker.ImagesToSprites(list)) ;
    list.Free ;
    Free ;
  end ;
  img.Free ;
end;

function TSceneMagicRender.FrameFunc(dt:Single; events:TUniList<TSfmlEventEx>):TSceneResult ;
var event:TSfmlEventEx ;
begin
  Result:=Normal ;
  for Event in events do begin
    if (Event.Event.EventType = sfEvtKeyPressed) then Result:=TSceneResult.Close ;
    if (Event.Event.EventType = sfEvtMouseButtonPressed) then Result:=TSceneResult.Close ;
  end;
  t:=t+dt ;
end ;

procedure TSceneMagicRender.RenderFunc() ;
var i,idx:Integer ;
begin
  for i:=0 to sprites.Count-1 do begin
    window.Draw(sprites[i]) ;
    idx:=Round(auras[i].Count*t) mod auras[i].Count ;
    DrawSprite(auras[i][idx],sprites[i].Position.X,sprites[i].Position.Y) ;
  end;
end ;

procedure TSceneMagicRender.UnInit() ;
begin
  sprites.Free ;
end ;

end.
