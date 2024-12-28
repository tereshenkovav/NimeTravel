unit SceneTextView;

interface

uses
  Classes, SysUtils,
  SfmlSystem,SfmlWindow,SfmlGraphics,SfmlAudio,
  Scene, Helpers ;

type

  { TSceneTextView }

  TSceneTextView = class(TScene)
  private
    texttitle:TSfmlText ;
    text:TSfmlText ;
    str_main:string ;
    str_title:string ;
  public
    constructor Create(const Astr_title:string; const Astr_main:string) ;
    function Init():Boolean ; override ;
    function FrameFunc(dt:Single; events:TUniList<TSfmlEventEx>):TSceneResult ; override ;
    procedure RenderFunc() ; override ;
    procedure UnInit() ; override ;
  end;

implementation
uses CommonData, SfmlUtils ;

function TSceneTextView.Init():Boolean ;
begin
  text:=createText(TCommonData.font,'',20,createSFMLColor($493100)) ;
  texttitle:=createText(TCommonData.font,str_title,28,createSFMLColor($493100)) ;
end ;

constructor TSceneTextView.Create(const Astr_title, Astr_main: string);
begin
  str_main:=Astr_main ;
  str_title:=Astr_title ;
end;

function TSceneTextView.FrameFunc(dt:Single; events:TUniList<TSfmlEventEx>):TSceneResult ;
var event:TSfmlEventEx ;
begin
  Result:=Normal ;
  for Event in events do begin
    if (Event.Event.EventType = sfEvtKeyPressed) then Result:=TSceneResult.ExitSubScene ;
    if (Event.Event.EventType = sfEvtMouseButtonPressed) then Result:=TSceneResult.ExitSubScene ;
  end;
end ;

procedure TSceneTextView.RenderFunc() ;
begin
  window.Draw(TCommonData.help_back) ;
  drawTextCentered(texttitle,wwidth/2,60) ;
  drawTextInBlockWidth(text,str_main,
    50,100,wwidth-100,3) ;
  DrawSprite(TCommonData.Cursor,mousex,mousey) ;
end ;

procedure TSceneTextView.UnInit() ;
begin
  texttitle.Free ;
  text.Free ;
end ;

end.
