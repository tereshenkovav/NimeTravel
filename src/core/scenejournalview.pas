unit SceneJournalView;

interface

uses
  Classes, SysUtils,
  SfmlSystem,SfmlWindow,SfmlGraphics,SfmlAudio,
  Scene, Helpers, SfmlAnimation, Spell ;

type

  { TSceneJournalView }

  TSceneJournalView = class(TScene)
  private
    texttitle:TSfmlText ;
    spells:TUniList<TSpell> ;
    Marker:TSfmlAnimation ;
    SpellIcons:array of TSfmlSprite ;
    markerseed:Single ;
  public
    constructor Create(Aspells:TUniList<TSpell>) ;
    function Init():Boolean ; override ;
    function FrameFunc(dt:Single; events:TUniList<TSfmlEventEx>):TSceneResult ; override ;
    procedure RenderFunc() ; override ;
    procedure UnInit() ; override ;
  end;

implementation
uses CommonData, SfmlUtils ;

function TSceneJournalView.Init():Boolean ;
var i:Integer ;
begin
  texttitle:=createText(TCommonData.font,'',28,createSFMLColor($493100)) ;
  if spells.Count>0 then
    texttitle.UnicodeString:=UTF8Decode(TCommonData.texts.getText('journal_caption'))
  else
    texttitle.UnicodeString:=UTF8Decode(TCommonData.texts.getText('journal_caption_nospells')) ;

  Marker:=TSfmlAnimation.Create('images'+PATH_SEP+'marker.png',30,34,16,20) ;
  Marker.Origin:=SfmlVector2f(15,17) ;
  markerseed:=0 ;

  SetLength(spellicons,spells.Count) ;
  for i:=0 to Length(spellicons)-1 do
    spellicons[i]:=loadSprite(spells[i].iconfile,[sloCentered]);
end ;

constructor TSceneJournalView.Create(Aspells: TUniList<TSpell>);
begin
  spells:=Aspells ;
end;

function TSceneJournalView.FrameFunc(dt:Single; events:TUniList<TSfmlEventEx>):TSceneResult ;
var event:TSfmlEventEx ;
begin
  Result:=Normal ;
  markerseed:=markerseed+5*dt ;
  for Event in events do begin
    if (Event.Event.EventType = sfEvtKeyPressed) then Result:=TSceneResult.ExitSubScene ;
    if (Event.Event.EventType = sfEvtMouseButtonPressed) then Result:=TSceneResult.ExitSubScene ;
  end;
end ;

procedure TSceneJournalView.RenderFunc() ;
var i,j:Integer ;
begin
  window.Draw(TCommonData.help_back) ;
  drawTextCentered(texttitle,wwidth/2,60) ;
  for i := 0 to spells.Count-1 do begin
      DrawSprite(spellicons[i],100,135+i*70) ;
      for j := 0 to spells[i].len-1 do begin
        Marker.Color:=TCommonData.markercolors[spells[i].seq[j]] ;
        Marker.SetFrame(3+(Round(markerseed)+i*10+j) mod 9) ;
        DrawSprite(Marker,160+j*30,140+i*70) ;
      end;
  end;
  DrawSprite(TCommonData.Cursor,mousex,mousey) ;
end ;

procedure TSceneJournalView.UnInit() ;
var i:Integer ;
begin
  texttitle.Free ;
  for i:=0 to Length(spellicons)-1 do
    spellicons[i].Free ;
  SetLength(spellicons,0) ;
end ;

end.
