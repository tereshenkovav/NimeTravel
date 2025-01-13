unit mainmenu;

interface
uses Classes,
  SfmlGraphics, SfmlSystem,
  CommonClasses, helpers, Scene, SfmlAnimation, Spell ;

type
  TCommonMenu = class(TScene)
  protected
    menu_back: TSfmlSprite ;
    text:TSfmlText ;
    items:TStringList ;
    icons:TUniDictionary<string,TSfmlSprite> ;
    shifty:Integer ;
    back:TSfmlTransformable ;
    logo:TSfmlSprite ;
    spells:TUniList<TSpell> ;
    localsubscene:TScene ;
    escaperesult:TSceneResult ;
    function getButtonX(i:Integer):Integer ;
    function getButtonY(i:Integer):Integer ;
    function isMouseOver(i:Integer):Boolean ;
    procedure rebuildItems() ; virtual ; abstract ;
    procedure InitMenu() ; virtual ;
    procedure loadLogo() ;
  public
    function Init():Boolean ; override ;
    procedure UnInit() ; override ;
    procedure RenderFunc() ; override ;
    function FrameFunc(dt:Single; events:TUniList<TSfmlEventEx>):TSceneResult ; override ;
  end;

  TMainMenu = class(TCommonMenu)
  protected
    procedure rebuildItems() ; override ;
    procedure InitMenu() ; override ;
  public
    constructor Create() ;
  end;

  TOptionsMenu = class(TCommonMenu)
  protected
    procedure rebuildItems() ; override ;
  public
    constructor Create() ;
  end;

  TGameMenu = class(TCommonMenu)
  protected
    procedure rebuildItems() ; override ;
  public
    constructor Create(Aspells:TUniList<TSpell>) ;
  end;

implementation
uses sfmlutils, ViewStatic, CommonProc, CommonData,
  SfmlWindow, Math, SysUtils, StrUtils, Logic, View,
  SceneConfirmNewGame, SceneTextView, SceneJournalView ;

const
  LANG_ITEM = 'lang' ;

{ TMainMenu }

constructor TMainMenu.Create() ;
begin
  shifty:=110 ;
  spells:=TUniList<TSpell>.Create() ;

  back:=TCommonData.intro ;
  loadLogo() ;
  escaperesult:=TSceneResult.Close ;
end;

procedure TMainMenu.InitMenu;
begin
  TCommonData.LoadMusicIfNew(TCommonData.INTRO_MUSIC) ;
end;

procedure TMainMenu.rebuildItems;
begin
  items.Clear ;
  if TLogic.isSaveGameExist() then
    items.Add('resumegame') ;
  items.Add('newgame') ;
  items.Add(LANG_ITEM) ;
  items.Add('options') ;
  items.Add('help') ;
  items.Add('credits') ;
  items.Add('exit') ;
end;

{ TGameMenu }

constructor TGameMenu.Create(Aspells:TUniList<TSpell>) ;
begin
  spells:=Aspells ;

  back:=TCommonData.grayrect ;
  logo:=nil ;
  escaperesult:=TSceneResult.ExitSubScene ;
end;

procedure TGameMenu.rebuildItems;
begin
  items.Clear() ;
  items.Add('continue') ;
  items.Add('music') ;
  items.Add('sound') ;
  items.Add('help') ;
  items.Add('journal') ;
  items.Add('mainmenu') ;
end;

{ TOptionsMenu }

constructor TOptionsMenu.Create;
begin
  shifty:=110 ;
  spells:=TUniList<TSpell>.Create() ;

  back:=TCommonData.intro ;
  loadLogo() ;
  escaperesult:=TSceneResult.Close ;
end;

procedure TOptionsMenu.rebuildItems;
begin
  items.Clear ;
  items.Add('music') ;
  items.Add('sound') ;
  items.Add('fullscr') ;
  items.Add('back') ;
end;

{ TCommonMenu }

procedure TCommonMenu.UnInit;
begin
  if logo<>nil then logo.Free ;
  menu_back.Free ;
  icons.Free ;
end;

function TCommonMenu.Init():Boolean;
var lang:string ;
begin
  menu_back:=LoadSprite('images'+PATH_SEP+'menu_back.png',[sloCentered]) ;
  menu_back.Position:=SfmlVector2f(wwidth/2,(wheight-100)/2+shifty) ;
  text:=createText(TCommonData.font,'',22,SfmlWhite) ;
  items:=TStringList.Create ;

  icons:=TUniDictionary<string,TSfmlSprite>.Create ;
  for lang in TCommonData.languages.getAll() do
    icons.Add(lang,LoadSprite('images'+PATH_SEP+'lang.'+lang+'.png')) ;

  rebuildItems() ;
  localsubscene:=nil ;

  InitMenu() ;
end;

function TCommonMenu.FrameFunc(dt:Single; events:TUniList<TSfmlEventEx>):TSceneResult ;
var Event:TSfmlEventEx;
    i:Integer ;
    lobj:TLogic ;
begin
  Result:=TSceneResult.Normal ;

  if localsubscene<>nil then begin
    localsubscene.setMousePos(mousex,mousey) ;
    if localsubscene.FrameFunc(dt,events)=TSceneResult.ExitSubScene then begin
      localsubscene.UnInit() ;
      FreeAndNil(localsubscene) ;
    end;
    Exit ;
  end;

  for Event in events do begin
      if (Event.Event.EventType = sfEvtKeyPressed) then
        if (event.Event.key.code = sfKeyEscape) then Exit(escaperesult) ;
      if (Event.Event.EventType = sfEvtMouseButtonPressed) then
        if (event.Event.MouseButton.Button = sfMouseLeft) then begin
          for i :=0 to items.Count-1 do
            if isMouseOver(i) then begin
              if (items[i]='continue') then begin
                Exit(TSceneResult.ExitSubScene) ;
              end;
              if (items[i]='resumegame') then begin
                lobj:=TLogic.Create(logger);
                nextscene:=TView.Create(lobj) ;
                Exit(TSceneResult.Switch) ;
              end;
              if (items[i]='newgame') then begin
                if TLogic.isSaveGameExist() then begin
                  nextscene:=TSceneConfirmNewGame.Create() ;
                  Exit(TSceneResult.Switch) ;
                end
                else begin
                  nextscene:=TViewStatic.Create() ;
                  TViewStatic(nextscene).AddTask(TCommonData.texts.getText('intro1'),44) ;
                  TViewStatic(nextscene).AddTask(TCommonData.texts.getText('intro2'),32) ;
                  TViewStatic(nextscene).AddTask(TCommonData.texts.getText('intro3'),32) ;
                  Exit(TSceneResult.Switch) ;
                end;
              end;
              if (items[i]='options') then begin
                nextscene:=TOptionsMenu.Create() ;
                Exit(TSceneResult.Switch) ;
              end;
              if (items[i]='fullscr') then begin
                profile.switchFullScreen() ;
                nextscene:=TOptionsMenu.Create() ;
                Exit(TSceneResult.RebuildWindow) ;
              end;
              if (items[i]='sound') then begin
                profile.switchSoundOn() ;
                rebuildItems() ;
                break ;
              end;
              if (items[i]='music') then begin
                profile.switchMusicOn() ;
                TCommonData.updateMusicVolume() ;
                rebuildItems() ;
                break ;
              end;
              if items[i]=LANG_ITEM then begin
                TCommonData.languages.switchCurrent() ;
                TCommonData.reloadTexts() ;
                newwindowtitle:=TCommonData.texts.getText('caption') ;
                if logo<>nil then loadLogo() ;
                Exit(TSceneResult.SetWindowTitle) ;
              end ;
              if items[i]='help' then begin
                localsubscene:=TSceneTextView.Create(
                  TCommonData.texts.getText('help_caption'),
                  TCommonData.texts.getText('help_text')) ;
                localsubscene.setWindow(window,wwidth,wheight) ;
                localsubscene.Init() ;
              end;
              if items[i]='credits' then begin
                localsubscene:=TSceneTextView.Create(
                  TCommonData.texts.getText('menu_credits'),
                  TCommonData.credits) ;
                localsubscene.setWindow(window,wwidth,wheight) ;
                localsubscene.Init() ;
              end;
              if items[i]='journal' then begin
                localsubscene:=TSceneJournalView.Create(spells) ;
                localsubscene.setWindow(window,wwidth,wheight) ;
                localsubscene.Init() ;
              end;
              if items[i]='exit' then Exit(TSceneResult.Close) ;
              if (items[i]='mainmenu')or(items[i]='back') then begin
                nextscene:=TMainMenu.Create() ;
                Exit(TSceneResult.Switch) ;
              end;
            end;
        end ;
    end ;
end;

function TCommonMenu.getButtonX(i: Integer): Integer;
begin
  Result:=wwidth div 2-200 div 2+(i-2)*3 ;
end;

function TCommonMenu.getButtonY(i: Integer): Integer;
begin
  Result:=154+i*33-40 div 2 +shifty ;
end;

function TCommonMenu.isMouseOver(i: Integer): Boolean;
begin
  Result:=(mousex>getButtonX(i))and(mousex<getButtonX(i)+200)and
    (mousey>getButtonY(i))and(mousey<getButtonY(i)+33) ;
end;

procedure TCommonMenu.loadLogo;
begin
  logo:=LoadSprite('images'+PATH_SEP+'logo.'+TCommonData.languages.getCurrent()+'.png') ;
  logo.Position:=SfmlVector2f(0,30) ;
end;

procedure TCommonMenu.InitMenu;
begin

end;

procedure TCommonMenu.RenderFunc();
var
  i: Integer;
  shiftlang:Integer ;
  j: Integer;
  str:string ;
begin
  if back<>nil then begin
    if back is TSfmlSprite then window.Draw(TSfmlSprite(back)) ;
    if back is TSfmlRectangleShape then window.Draw(TSfmlRectangleShape(back)) ;
  end ;
  if logo<>nil then window.Draw(logo) ;

  if localsubscene<>nil then begin
    localsubscene.setMousePos(mousex,mousey) ;
    localsubscene.RenderFunc() ;
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
    str:=TCommonData.texts.getText('menu_'+items[i]) ;
    if items[i]='fullscr' then str:=str+': '+IfThen(profile.isFullScreen(),
      TCommonData.texts.getText('text_on'),TCommonData.texts.getText('text_off')) ;
    if items[i]='sound' then str:=str+': '+IfThen(profile.isSoundOn(),
      TCommonData.texts.getText('text_on'),TCommonData.texts.getText('text_off')) ;
    if items[i]='music' then str:=str+': '+IfThen(profile.isMusicOn(),
      TCommonData.texts.getText('text_on'),TCommonData.texts.getText('text_off')) ;
    text.UnicodeString:=UTF8Decode(str) ;

    if items[i]=LANG_ITEM then shiftlang:=27+10 ;

    text.Position := SfmlVector2f(wwidth/2-(text.LocalBounds.Width+shiftlang)/2,getButtonY(i));
    window.Draw(text) ;

    if items[i]=LANG_ITEM then begin
      icons[TCommonData.languages.getCurrent()].Position:=SfmlVector2f(
        text.Position.X+text.LocalBounds.Width+10,getButtonY(i)+5) ;
      window.Draw(icons[TCommonData.languages.getCurrent()]) ;
    end;
  end;

  DrawSprite(TCommonData.Cursor,mousex,mousey) ;
end;

end.
