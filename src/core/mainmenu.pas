unit mainmenu;

interface
uses Classes,
  SfmlGraphics, SfmlSystem,
  CommonClasses, helpers, Scene ;

type
  TMainMenu = class(TScene)
  private
    Cursor:TSfmlSprite ;
    menu_back: TSfmlSprite ;
    button: TSfmlSprite ;
    icons:TUniDictionary<string,TSfmlSprite> ;
    text:TSfmlText ;
    buttonw:Integer ;
    buttonh:Integer ;
    items:TStringList ;
    help_back:TSfmlSprite ;
    helpmode:Boolean ;
    first_item:string ;
    shifty:Integer ;
    texthelptitle:TSfmlText ;
    texthelp:TSfmlText ;
    back:TSfmlSprite ;
    logo:TSfmlSprite ;
    function getButtonX(i:Integer):Integer ;
    function getButtonY(i:Integer):Integer ;
    function isMouseOver(i:Integer):Boolean ;
    procedure rebuildItems() ;
    procedure setHelpText() ;
    procedure loadLogo() ;
  public
    constructor Create(Astartmode:Boolean; Afirst_item:string; Ashifty:Integer=0) ;
    function Init():Boolean ; override ;
    procedure UnInit() ; override ;
    procedure RenderFunc() ; override ;
    function FrameFunc(dt:Single; events:TUniList<TSfmlEventEx>):TSceneResult ; override ;
  end;

implementation
uses sfmlutils, ViewStatic, CommonProc, CommonData,
  SfmlWindow, Math, SysUtils ;

const
  LANG_ITEM = 'lang' ;

{ TMainMenu }

constructor TMainMenu.Create(Astartmode:Boolean;
  Afirst_item:string; Ashifty:Integer);
var lang:string ;
begin
  first_item:=Afirst_item ;
  shifty:=Ashifty ;

  if not Astartmode then begin
    back:=LoadSprite('images'+PATH_SEP+'gray.png') ; ;
    back.Position:=SfmlVector2f(0,0) ;
    logo:=nil ;
  end
  else begin
    back:=LoadSprite('images'+PATH_SEP+'intro.png') ;
    back.Position:=SfmlVector2f(0,0) ;
    loadLogo() ;
  end;

  icons:=TUniDictionary<string,TSfmlSprite>.Create ;
  for lang in TCommonData.languages.getAll() do
    icons.Add(lang,LoadSprite('images'+PATH_SEP+'lang.'+lang+'.png')) ;
end;

procedure TMainMenu.UnInit;
begin
  if back<>nil then back.Free ;
  if logo<>nil then logo.Free ;
  menu_back.Free ;
  button.Free ;
  icons.Free ;
  Cursor.Free ;
end;

function TMainMenu.Init():Boolean;
begin
  menu_back:=LoadSprite('images'+PATH_SEP+'menu_back.png',[sloCentered]) ;
  menu_back.Position:=SfmlVector2f(WINDOW_W/2,(WINDOW_H-100)/2+shifty) ;
  help_back:=LoadSprite('images'+PATH_SEP+'help_back.png') ;
  help_back.Position:=SfmlVector2f(0,0) ;
  button:=LoadSprite('images'+PATH_SEP+'button.png') ;
  text:=createText(TCommonData.font,'',24,SfmlWhite) ;
  texthelptitle:=createText(TCommonData.font,'',28,createSFMLColor($493100)) ;
  texthelp:=createText(TCommonData.font,'',20,createSFMLColor($493100)) ;
  setHelpText() ;
  buttonw:=SfmlTextureGetSize(button.Texture).X ;
  buttonh:=SfmlTextureGetSize(button.Texture).Y ;
  items:=TStringList.Create ;
  helpmode:=False ;

  Cursor:=loadSprite('images'+PATH_SEP+'cursor.png');
  Cursor.Origin:=SfmlVector2f(0,10) ;

  rebuildItems() ;
end;

function TMainMenu.FrameFunc(dt:Single; events:TUniList<TSfmlEventEx>):TSceneResult ;
var Event:TSfmlEventEx;
    i:Integer ;
begin
  Result:=TSceneResult.Normal ;

  if helpmode then begin
      for Event in events do begin
        if (Event.Event.EventType = sfEvtKeyPressed) then helpmode:=False ;
        if (Event.Event.EventType = sfEvtMouseButtonPressed) then helpmode:=False ;
      end;
    Exit ;
  end;

  for Event in events do begin
      if (Event.Event.EventType = sfEvtKeyPressed) then begin
        if (event.Event.key.code = sfKeyEscape) then begin
              if (items[0]='continue') then begin
                Exit(TSceneResult.ExitSubScene) ;
              end;
              if (items[0]='newgame') then begin
                Exit(TSceneResult.Close) ;
              end;
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
                TViewStatic(nextscene).AddTask(TCommonData.texts.getText('intro1'),48) ;
                TViewStatic(nextscene).AddTask(TCommonData.texts.getText('intro2'),32) ;
                TViewStatic(nextscene).AddTask(TCommonData.texts.getText('intro3'),32) ;
                Exit(TSceneResult.Switch) ;
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
                newwindowtitle:=getWindowTitle() ;
                if logo<>nil then loadLogo() ;
                Exit(TSceneResult.SetWindowTitle) ;
              end ;
              if items[i]='help' then helpmode:=True ;
              if items[i]='exit' then Exit(TSceneResult.Close) ;
            end;
        end ;
    end ;
end;

function TMainMenu.getButtonX(i: Integer): Integer;
begin
  Result:=WINDOW_W div 2-buttonw div 2+(i-2)*3 ;
end;

function TMainMenu.getButtonY(i: Integer): Integer;
begin
  Result:=154+i*40-buttonh div 2 +shifty ;
end;

function TMainMenu.isMouseOver(i: Integer): Boolean;
begin
  Result:=(mousex>getButtonX(i))and(mousex<getButtonX(i)+buttonw)and
    (mousey>getButtonY(i))and(mousey<getButtonY(i)+buttonh) ;
end;

procedure TMainMenu.setHelpText;
begin
  texthelptitle.UnicodeString:=UTF8Decode(TCommonData.texts.getText('help_caption')) ;
  texthelp.UnicodeString:=UTF8Decode(TCommonData.texts.getText('help_text')) ;
end;

procedure TMainMenu.loadLogo;
begin
  logo:=LoadSprite('images'+PATH_SEP+'logo.'+TCommonData.languages.getCurrent()+'.png') ;
  logo.Position:=SfmlVector2f(0,30) ;
end;

procedure TMainMenu.rebuildItems;
begin
  items.Clear ;
  items.Add(first_item) ;
  items.Add(LANG_ITEM) ;
  if profile.isMusicOn then items.Add('music_on') else items.Add('music_off') ;
  if profile.isSoundOn then items.Add('sound_on') else items.Add('sound_off') ;
  items.Add('help') ;
  items.Add('exit') ;
end;

procedure TMainMenu.RenderFunc();
var
  i: Integer;
  shiftlang:Integer ;
begin
  if back<>nil then window.Draw(back) ;
  if logo<>nil then window.Draw(logo) ;

  if helpmode then begin
    window.Draw(help_back) ;
    texthelptitle.Position := SfmlVector2f(WINDOW_W/2-texthelptitle.LocalBounds.Width/2,60);
    window.Draw(texthelptitle) ;
    texthelp.Position := SfmlVector2f(WINDOW_W/2-texthelp.LocalBounds.Width/2,100);
    window.Draw(texthelp) ;
    DrawSprite(Cursor,mousex,mousey) ;
    Exit ;
  end;

  window.Draw(menu_back) ;

  for i := 0 to items.Count-1 do begin
    shiftlang:=0 ;
    button.Position:=SfmlVector2f(getButtonX(i),getButtonY(i)) ;
    if isMouseOver(i) then begin
      button.Color:=createSFMLColor($FFFFFF) ;
      text.Color:=createSFMLColor($996732) ;
      text.Style:=1 ;
    end
    else begin
      button.Color:=createSFMLColor($A0A0A0) ;
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
