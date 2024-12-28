unit mainmenu;

interface
uses Classes,
  SfmlGraphics, SfmlSystem,
  CommonClasses, helpers, Scene, SfmlAnimation, Spell ;

type
  TMainMenu = class(TScene)
  private
    menu_back: TSfmlSprite ;
    text:TSfmlText ;
    items:TStringList ;
    icons:TUniDictionary<string,TSfmlSprite> ;
    ismainmenu:Boolean ;
    shifty:Integer ;
    back:TSfmlTransformable ;
    logo:TSfmlSprite ;
    spells:TUniList<TSpell> ;
    localsubscene:TScene ;
    function getButtonX(i:Integer):Integer ;
    function getButtonY(i:Integer):Integer ;
    function isMouseOver(i:Integer):Boolean ;
    procedure rebuildItems() ;
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
  SfmlWindow, Math, SysUtils, StrUtils, Logic, View,
  SceneConfirmNewGame, SceneTextView, SceneJournalView ;

const
  LANG_ITEM = 'lang' ;

{ TMainMenu }

constructor TMainMenu.CreateAsMainMenu() ;
begin
  ismainmenu:=True ;
  shifty:=110 ;
  spells:=TUniList<TSpell>.Create() ;

  back:=TCommonData.intro ;
  loadLogo() ;
end;

constructor TMainMenu.CreateAsGameMenu(Aspells:TUniList<TSpell>) ;
begin
  ismainmenu:=False ;
  spells:=Aspells ;

  back:=TCommonData.grayrect ;
  logo:=nil ;
end;

procedure TMainMenu.UnInit;
begin
  if logo<>nil then logo.Free ;
  menu_back.Free ;
  icons.Free ;
end;

function TMainMenu.Init():Boolean;
var lang:string ;
begin
  menu_back:=LoadSprite('images'+PATH_SEP+'menu_back.png',[sloCentered]) ;
  menu_back.Position:=SfmlVector2f(wwidth/2,(wheight-100)/2+shifty) ;
  text:=createText(TCommonData.font,'',22,SfmlWhite) ;
  items:=TStringList.Create ;

  icons:=TUniDictionary<string,TSfmlSprite>.Create ;
  for lang in TCommonData.languages.getAll() do
    icons.Add(lang,LoadSprite('images'+PATH_SEP+'lang.'+lang+'.png')) ;

  if ismainmenu then TCommonData.LoadMusicIfNew(TCommonData.INTRO_MUSIC) ;

  rebuildItems() ;
  localsubscene:=nil ;
end;

function TMainMenu.FrameFunc(dt:Single; events:TUniList<TSfmlEventEx>):TSceneResult ;
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
              if (items[i]='fullscr') then begin
                profile.switchFullScreen() ;
                nextscene:=TMainMenu.CreateAsMainMenu() ;
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
  Result:=wwidth div 2-200 div 2+(i-2)*3 ;
end;

function TMainMenu.getButtonY(i: Integer): Integer;
begin
  Result:=124+i*33-40 div 2 +shifty ;
end;

function TMainMenu.isMouseOver(i: Integer): Boolean;
begin
  Result:=(mousex>getButtonX(i))and(mousex<getButtonX(i)+200)and
    (mousey>getButtonY(i))and(mousey<getButtonY(i)+33) ;
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
    if TLogic.isSaveGameExist() then
      items.Add('resumegame') ;
    items.Add('newgame') ;
    items.Add(LANG_ITEM) ;
    items.Add('fullscr') ;
    items.Add('music') ;
    items.Add('sound') ;
    items.Add('help') ;
    items.Add('credits') ;
    items.Add('exit') ;
  end
  else begin
    items.Add('continue') ;
    items.Add('music') ;
    items.Add('sound') ;
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
