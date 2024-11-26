unit mainmenu;

interface
uses Classes,
  SfmlGraphics, SfmlSystem,
  CommonClasses, helpers, Scene, SfmlAnimation, Spell ;

type
  TSubMode = (smNone,smHelp,smJournal) ;

  TMainMenu = class(TScene)
  private
    Cursor:TSfmlSprite ;
    menu_back: TSfmlSprite ;
    icons:TUniDictionary<string,TSfmlSprite> ;
    Marker:TSfmlAnimation ;
    SpellIcons:array of TSfmlSprite ;
    text:TSfmlText ;
    items:TStringList ;
    help_back:TSfmlSprite ;
    submode:TSubMode ;
    ismainmenu:Boolean ;
    shifty:Integer ;
    texthelptitle:TSfmlText ;
    textjournaltitle:TSfmlText ;
    texthelp:TSfmlText ;
    back:TSfmlSprite ;
    logo:TSfmlSprite ;
    markerseed:Single ;
    spells:TUniList<TSpell> ;
    function getButtonX(i:Integer):Integer ;
    function getButtonY(i:Integer):Integer ;
    function isMouseOver(i:Integer):Boolean ;
    procedure rebuildItems() ;
    procedure setHelpText() ;
    procedure loadLogo() ;
  public
    constructor CreateAsMainMenu() ;
    constructor CreateAsGameMenu(Aspells:TUniList<TSpell>) ;
    function Init():Boolean ; override ;
    procedure UnInit() ; override ;
    procedure RenderFunc() ; override ;
    function FrameFunc(dt:Single; events:TUniList<TSfmlEventEx>):TSceneResult ; override ;
  end;

implementation
uses sfmlutils, ViewStatic, CommonProc, CommonData,
  SfmlWindow, Math, SysUtils, StrUtils ;

const
  LANG_ITEM = 'lang' ;

{ TMainMenu }

constructor TMainMenu.CreateAsMainMenu() ;
begin
  ismainmenu:=True ;
  shifty:=110 ;
  spells:=TUniList<TSpell>.Create() ;

  back:=LoadSprite('images'+PATH_SEP+'intro.png') ;
  back.Position:=SfmlVector2f(0,0) ;
  loadLogo() ;
end;

constructor TMainMenu.CreateAsGameMenu(Aspells:TUniList<TSpell>) ;
begin
  ismainmenu:=False ;
  spells:=Aspells ;

  back:=LoadSprite('images'+PATH_SEP+'gray.png') ; ;
  back.Position:=SfmlVector2f(0,0) ;
  logo:=nil ;
end;

procedure TMainMenu.UnInit;
var i:Integer ;
begin
  if back<>nil then back.Free ;
  if logo<>nil then logo.Free ;
  for i:=0 to Length(spellicons)-1 do
    spellicons[i].Free ;
  SetLength(spellicons,0) ;
  menu_back.Free ;
  icons.Free ;
  Cursor.Free ;
end;

function TMainMenu.Init():Boolean;
var lang:string ;
    i:Integer ;
begin
  menu_back:=LoadSprite('images'+PATH_SEP+'menu_back.png',[sloCentered]) ;
  menu_back.Position:=SfmlVector2f(WINDOW_W/2,(WINDOW_H-100)/2+shifty) ;
  help_back:=LoadSprite('images'+PATH_SEP+'help_back.png') ;
  help_back.Position:=SfmlVector2f(0,0) ;
  text:=createText(TCommonData.font,'',24,SfmlWhite) ;
  texthelptitle:=createText(TCommonData.font,'',28,createSFMLColor($493100)) ;
  texthelp:=createText(TCommonData.font,'',20,createSFMLColor($493100)) ;
  textjournaltitle:=createText(TCommonData.font,'',28,createSFMLColor($493100)) ;
  setHelpText() ;
  items:=TStringList.Create ;
  submode:=smNone ;

  Cursor:=loadSprite('images'+PATH_SEP+'cursor.png');
  Cursor.Origin:=SfmlVector2f(0,10) ;

  Marker:=TSfmlAnimation.Create('images'+PATH_SEP+'marker.png',30,34,16,20) ;
  Marker.Origin:=SfmlVector2f(15,17) ;
  markerseed:=0 ;

  SetLength(spellicons,spells.Count) ;
  for i:=0 to Length(spellicons)-1 do
    spellicons[i]:=loadSprite(spells[i].iconfile,[sloCentered]);

  icons:=TUniDictionary<string,TSfmlSprite>.Create ;
  for lang in TCommonData.languages.getAll() do
    icons.Add(lang,LoadSprite('images'+PATH_SEP+'lang.'+lang+'.png')) ;

  if ismainmenu then TCommonData.LoadMusicIfNew(TCommonData.INTRO_MUSIC) ;

  rebuildItems() ;
end;

function TMainMenu.FrameFunc(dt:Single; events:TUniList<TSfmlEventEx>):TSceneResult ;
var Event:TSfmlEventEx;
    i:Integer ;
begin
  Result:=TSceneResult.Normal ;

  markerseed:=markerseed+5*dt ;

  if submode<>smNone then begin
      for Event in events do begin
        if (Event.Event.EventType = sfEvtKeyPressed) then submode:=smNone ;
        if (Event.Event.EventType = sfEvtMouseButtonPressed) then submode:=smNone ;
      end;
    Exit ;
  end;

  for Event in events do begin
      if (Event.Event.EventType = sfEvtKeyPressed) then begin
        if (event.Event.key.code = sfKeyEscape) then begin
              if ismainmenu then
                Exit(TSceneResult.Close)
              else
                Exit(TSceneResult.ExitSubScene) ;
        end ;
      end;
      if (Event.Event.EventType = sfEvtMouseButtonPressed) then
        if (event.Event.MouseButton.Button = sfMouseLeft) then begin
          for i :=0 to items.Count do
            if isMouseOver(i) then begin
              if (items[i]='continue') then begin
                Exit(TSceneResult.ExitSubScene) ;
              end;
              if (items[i]='newgame') then begin
                nextscene:=TViewStatic.Create() ;
                TViewStatic(nextscene).AddTask(TCommonData.texts.getText('intro1'),44) ;
                TViewStatic(nextscene).AddTask(TCommonData.texts.getText('intro2'),32) ;
                TViewStatic(nextscene).AddTask(TCommonData.texts.getText('intro3'),32) ;
                Exit(TSceneResult.Switch) ;
              end;
              if (items[i]='fullscr_on')or(items[i]='fullscr_off') then begin
                profile.switchFullScreen() ;
                nextscene:=TMainMenu.CreateAsMainMenu() ;
                Exit(TSceneResult.RebuildWindow) ;
              end;
              if (items[i]='sound_on')or(items[i]='sound_off') then begin
                profile.switchSoundOn() ;
                rebuildItems() ;
                break ;
              end;
              if (items[i]='music_on')or(items[i]='music_off') then begin
                profile.switchMusicOn() ;
                TCommonData.updateMusicVolume() ;
                rebuildItems() ;
                break ;
              end;
              if items[i]=LANG_ITEM then begin
                TCommonData.languages.switchCurrent() ;
                TCommonData.reloadTexts() ;
                setHelpText() ;
                newwindowtitle:=TCommonData.texts.getText('caption') ;
                if logo<>nil then loadLogo() ;
                Exit(TSceneResult.SetWindowTitle) ;
              end ;
              if items[i]='help' then submode:=smHelp ;
              if items[i]='journal' then submode:=smJournal ;
              if items[i]='exit' then Exit(TSceneResult.Close) ;
              if items[i]='mainmenu' then begin
                nextscene:=TMainMenu.CreateAsMainMenu() ;
                Exit(TSceneResult.Switch) ;
              end;
            end;
        end ;
    end ;
end;

function TMainMenu.getButtonX(i: Integer): Integer;
begin
  Result:=WINDOW_W div 2-200 div 2+(i-2)*3 ;
end;

function TMainMenu.getButtonY(i: Integer): Integer;
begin
  Result:=154+i*40-40 div 2 +shifty ;
end;

function TMainMenu.isMouseOver(i: Integer): Boolean;
begin
  Result:=(mousex>getButtonX(i))and(mousex<getButtonX(i)+200)and
    (mousey>getButtonY(i))and(mousey<getButtonY(i)+40) ;
end;

procedure TMainMenu.setHelpText;
begin
  texthelptitle.UnicodeString:=UTF8Decode(TCommonData.texts.getText('help_caption')) ;
  if spells.Count>0 then
    textjournaltitle.UnicodeString:=UTF8Decode(TCommonData.texts.getText('journal_caption'))
  else
    textjournaltitle.UnicodeString:=UTF8Decode(TCommonData.texts.getText('journal_caption_nospells')) ;
end;

procedure TMainMenu.loadLogo;
begin
  logo:=LoadSprite('images'+PATH_SEP+'logo.'+TCommonData.languages.getCurrent()+'.png') ;
  logo.Position:=SfmlVector2f(0,30) ;
end;

procedure TMainMenu.rebuildItems;
begin
  items.Clear ;
  if ismainmenu then begin
    items.Add('newgame') ;
    items.Add(LANG_ITEM) ;
    items.Add(IfThen(profile.isFullScreen,'fullscr_on','fullscr_off')) ;
    items.Add(IfThen(profile.isMusicOn,'music_on','music_off')) ;
    items.Add(IfThen(profile.isSoundOn,'sound_on','sound_off')) ;
    items.Add('help') ;
    items.Add('exit') ;
  end
  else begin
    items.Add('continue') ;
    items.Add(IfThen(profile.isMusicOn,'music_on','music_off')) ;
    items.Add(IfThen(profile.isSoundOn,'sound_on','sound_off')) ;
    items.Add('help') ;
    items.Add('journal') ;
    items.Add('mainmenu') ;
  end;
end;

procedure TMainMenu.RenderFunc();
var
  i: Integer;
  shiftlang:Integer ;
  j: Integer;
begin
  if back<>nil then window.Draw(back) ;
  if logo<>nil then window.Draw(logo) ;

  if submode=smHelp then begin
    window.Draw(help_back) ;
    texthelptitle.Position := SfmlVector2f(WINDOW_W/2-texthelptitle.LocalBounds.Width/2,60);
    window.Draw(texthelptitle) ;
    drawTextInBlockWidth(texthelp,TCommonData.texts.getText('help_text'),
      50,100,WINDOW_W-100,3) ;
    DrawSprite(Cursor,mousex,mousey) ;
    Exit ;
  end;

  if submode=smJournal then begin
    window.Draw(help_back) ;
    textjournaltitle.Position := SfmlVector2f(WINDOW_W/2-textjournaltitle.LocalBounds.Width/2,60);
    window.Draw(textjournaltitle) ;
    for i := 0 to spells.Count-1 do begin
      DrawSprite(spellicons[i],100,135+i*70) ;
      for j := 0 to spells[i].len-1 do begin
        Marker.Color:=TCommonData.markercolors[spells[i].seq[j]] ;
        Marker.SetFrame(3+(Round(markerseed)+i*10+j) mod 9) ;
        DrawSprite(Marker,160+j*30,140+i*70) ;
      end;
    end;
    DrawSprite(Cursor,mousex,mousey) ;
    Exit ;
  end;

  window.Draw(menu_back) ;

  for i := 0 to items.Count-1 do begin
    shiftlang:=0 ;
    if isMouseOver(i) then begin
      text.Color:=createSFMLColor($996732) ;
      text.Style:=1 ;
    end
    else begin
      text.Color:=createSFMLColor($895722) ;
      text.Style:=0 ;
    end;
    text.UnicodeString:=UTF8Decode(TCommonData.texts.getText('menu_'+items[i])) ;

    if items[i]=LANG_ITEM then shiftlang:=27+10 ;

    text.Position := SfmlVector2f(WINDOW_W/2-(text.LocalBounds.Width+shiftlang)/2,getButtonY(i));
    window.Draw(text) ;

    if items[i]=LANG_ITEM then begin
      icons[TCommonData.languages.getCurrent()].Position:=SfmlVector2f(
        text.Position.X+text.LocalBounds.Width+10,getButtonY(i)+5) ;
      window.Draw(icons[TCommonData.languages.getCurrent()]) ;
    end;
  end;

  DrawSprite(Cursor,mousex,mousey) ;
end;

end.
