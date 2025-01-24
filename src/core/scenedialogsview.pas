unit SceneDialogsView;

interface

uses
  Classes, SysUtils,
  SfmlSystem,SfmlWindow,SfmlGraphics,SfmlAudio,
  Scene, Helpers, CommonClasses ;

type

  { TSceneDialogsView }

  TSceneDialogsView = class(TScene)
  private
    texttitle:TSfmlText ;
    str_title:string ;
    last_idx:Integer ;
    blocks:TUniList<TSfmlText> ;
    vertsize:Integer ;
    canscrollup:Boolean ;
    arrow:TSfmlSprite ;
    dialogjournal:TUniList<TDialogText> ;
    function genTextBlocks(str:string; width:Integer; redlinewidth:Integer; color:Cardinal):TUniList<TSfmlText>;
    procedure RebuildTexts() ;
  public
    constructor Create(Adialogjournal:TUniList<TDialogText>) ;
    function Init():Boolean ; override ;
    function FrameFunc(dt:Single; events:TUniList<TSfmlEventEx>):TSceneResult ; override ;
    procedure RenderFunc() ; override ;
    procedure UnInit() ; override ;
  end;

implementation
uses CommonData, SfmlUtils ;

const DIALOG_FONT_SIZE = 20 ;
      ARROW_UP_Y = 120 ;
      ARROW_DOWN_Y = 540 ;

function TSceneDialogsView.genTextBlocks
  (str:string; width:Integer; redlinewidth:Integer; color:Cardinal):TUniList<TSfmlText>;
var words:TArray<string> ;
    line:string ;
    i:Integer ;
    isnewline:Boolean ;
    text:TSfmlText ;
begin
  Result:=TUniList<TSfmlText>.Create() ;

  words:=str.Split([' '],TStringSplitOptions.ExcludeEmpty) ;

  if Length(words)=0 then Exit ;

  text:=createText(TCommonData.font,'',DIALOG_FONT_SIZE,createSFMLColor($493100)) ;

  line:=StringOfChar(' ',redlinewidth)+words[0] ;
  for i := 1 to Length(words)-1 do begin
    text.UnicodeString:=UTF8Decode(line+' '+words[i]) ;
    isnewline:=line.EndsWith(#10) ;
    if (text.LocalBounds.Width>=width)or isnewline then begin
      line:=line.Replace(#10,'') ;
      text.UnicodeString:=UTF8Decode(line) ;
      Result.Add(text) ;
      text:=createText(TCommonData.font,'',DIALOG_FONT_SIZE,createSFMLColor($493100)) ;

      line:=words[i] ;
      if isnewline then line:=StringOfChar(' ',redlinewidth)+line ;
    end
    else
      line:=line+' '+words[i] ;
  end;
  if line.Length>0 then begin
    line:=line.Replace(#10,'') ;
    text.UnicodeString:=UTF8Decode(line) ;
    Result.Add(text) ;
  end;
end;

function TSceneDialogsView.Init():Boolean ;
begin
  texttitle:=createText(TCommonData.font,str_title,28,CreateSfmlColor($493100)) ;
  arrow:=LoadSprite('images'+PATH_SEP+'arrow.png',[sloCentered]) ;
  vertsize:=Round(SfmlFontGetLineSpacing(TCommonData.font.Handle,DIALOG_FONT_SIZE)) ;
  RebuildTexts() ;
end ;

constructor TSceneDialogsView.Create(Adialogjournal:TUniList<TDialogText>);
begin
  str_title:=TCommonData.texts.getText('dialogs_caption') ;
  blocks:=TUniList<TSfmlText>.Create() ;
  dialogjournal:=Adialogjournal ;
  last_idx:=dialogjournal.Count-1 ;
end;

function TSceneDialogsView.FrameFunc(dt:Single; events:TUniList<TSfmlEventEx>):TSceneResult ;
var event:TSfmlEventEx ;

procedure TryRollUp() ;
begin
  if canscrollup then begin
    Dec(last_idx) ;
    RebuildTexts() ;
   end;
end;

procedure TryRollDown() ;
begin
  if last_idx<dialogjournal.Count-1 then begin
    Inc(last_idx) ;
    RebuildTexts() ;
  end;
end;

begin
  Result:=Normal ;
  for Event in events do begin
    if (Event.Event.EventType = sfEvtKeyPressed) then begin
      if (Event.Event.Key.Code = sfKeyUp) then TryRollUp() ;
      if (Event.Event.Key.Code = sfKeyDown) then TryRollDown() ;
      if (Event.Event.Key.Code in [sfKeyEscape,sfKeySpace]) then Result:=TSceneResult.ExitSubScene ;
    end;
    if (Event.Event.EventType = sfEvtMouseButtonPressed) then begin
      if isMouseOverSprite(arrow,wwidth/2,ARROW_UP_Y) then TryRollUp() ;
      if isMouseOverSprite(arrow,wwidth/2,ARROW_DOWN_Y) then TryRollDown() ;
    end;
  end;
end ;

procedure TSceneDialogsView.RebuildTexts;
var i,j:Integer ;
    subblocks:TUniList<TSfmlText> ;
begin
  blocks.Clear() ;
  i:=last_idx ;
  while i>=0 do begin
    subblocks:=genTextBlocks(dialogjournal[i].text,wwidth-100,3,dialogjournal[i].color) ;
    for j := subblocks.Count-1 downto 0 do
      blocks.Add(subblocks[j]) ;
    subblocks.Free ;
    if vertsize*blocks.Count>340 then break ;
    Dec(i) ;
  end;
  canscrollup:=i>0 ;
end;

procedure TSceneDialogsView.RenderFunc() ;
var i:Integer ;
begin
  window.Draw(TCommonData.help_back) ;
  drawTextCentered(texttitle,wwidth/2,60) ;

  for i := blocks.Count-1 downto 0 do
    drawText(blocks[blocks.Count-1-i],50,140+vertsize*i) ;

  if canscrollup then drawSpriteMirr(arrow,wwidth/2,ARROW_UP_Y,[MirrorVert]) ;
  if last_idx<dialogjournal.Count-1 then drawSpriteMirr(arrow,wwidth/2,ARROW_DOWN_Y,[]) ;

  DrawSprite(TCommonData.Cursor,mousex,mousey) ;
end ;

procedure TSceneDialogsView.UnInit() ;
begin
  texttitle.Free ;
end ;

end.
