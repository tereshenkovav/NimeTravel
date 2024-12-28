unit SceneCloseHandler;

interface

uses
  Classes, SysUtils,
  SfmlSystem,SfmlWindow,SfmlGraphics,SfmlAudio,
  Scene, Helpers ;

type

  { TSceneCloseHandler }

  TSceneCloseHandler = class(TScene)
  private
    menu_back: TSfmlSprite ;
    text:TSfmlText ;
    items:TStringList ;
    function getButtonX(i:Integer):Integer ;
    function getButtonY(i:Integer):Integer ;
    function isMouseOver(i:Integer):Boolean ;
  public
    function Init():Boolean ; override ;
    function FrameFunc(dt:Single; events:TUniList<TSfmlEventEx>):TSceneResult ; override ;
    procedure RenderFunc() ; override ;
    procedure UnInit() ; override ;
  end;

implementation
uses CommonData, SfmlUtils ;

function TSceneCloseHandler.Init():Boolean ;
begin
  menu_back:=LoadSprite('images'+PATH_SEP+'menu_back.png',[sloCentered]) ;
  menu_back.Position:=SfmlVector2f(wwidth/2,(wheight-100)/2) ;
  text:=createText(TCommonData.font,'',24,SfmlWhite) ;
  items:=TStringList.Create() ;
  items.Add('but_yes') ;
  items.Add('but_no') ;
end ;

function TSceneCloseHandler.FrameFunc(dt:Single; events:TUniList<TSfmlEventEx>):TSceneResult ;
var event:TSfmlEventEx ;
    i:Integer ;
begin
  Result:=Normal ;
  for event in events do
    if (Event.Event.EventType = sfEvtMouseButtonPressed) then
        if (event.Event.MouseButton.Button = sfMouseLeft) then
          for i :=0 to items.Count-1 do
            if isMouseOver(i) then begin
              if (items[i]='but_yes') then Exit(TSceneResult.Close) ;
              if (items[i]='but_no') then Exit(TSceneResult.Switch) ;
            end;
end ;

function TSceneCloseHandler.getButtonX(i: Integer): Integer;
begin
  Result:=wwidth div 2-50+(i)*100 ;
end;

function TSceneCloseHandler.getButtonY(i: Integer): Integer;
begin
  Result:=250 ;
end;

function TSceneCloseHandler.isMouseOver(i: Integer): Boolean;
begin
  text.UnicodeString:=UTF8Decode(TCommonData.texts.getText(items[i])) ;
  text.Position := SfmlVector2f(getButtonX(i)-text.LocalBounds.Width/2,getButtonY(i));
  Result:=(mousex>text.GlobalBounds.Left)and
    (mousex<text.GlobalBounds.Left+text.GlobalBounds.Width)and
    (mousey>text.GlobalBounds.Top)and
    (mousey<text.GlobalBounds.Top+text.GlobalBounds.Height) ;
end;

procedure TSceneCloseHandler.RenderFunc() ;
var i:Integer ;
begin
  DrawSprite(TCommonData.intro,0,0) ;
  window.Draw(menu_back) ;

  text.Color:=createSFMLColor($895722) ;
  text.Style:=0 ;
  text.UnicodeString:=UTF8Decode(TCommonData.texts.getText('text_close_quest')) ;
  DrawTextCentered(text,wwidth/2,170) ;

  for i := 0 to items.Count-1 do begin
    if isMouseOver(i) then begin
      text.Color:=createSFMLColor($996732) ;
      text.Style:=1 ;
    end
    else begin
      text.Color:=createSFMLColor($895722) ;
      text.Style:=0 ;
    end;
    text.UnicodeString:=UTF8Decode(TCommonData.texts.getText(items[i])) ;
    text.Position := SfmlVector2f(getButtonX(i)-text.LocalBounds.Width/2,getButtonY(i));
    window.Draw(text) ;
  end;

  DrawSprite(TCommonData.Cursor,mousex,mousey) ;
end ;

procedure TSceneCloseHandler.UnInit() ;
begin
  menu_back.Free ;
  text.Free ;
  items.Free ;
end ;

end.
