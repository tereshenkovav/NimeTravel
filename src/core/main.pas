unit main ;

interface
uses SfmlWindow, SfmlGraphics, SfmlAudio,
  mainmenu, commonclasses ;

type
  TMain = class
  private
    mainmenu:TMainMenu ;
    Window:TSfmlRenderWindow ;
    Cursor:TSfmlSprite ;
    Music:TSfmlMusic ;
    options:TOptions ;
    back:TSfmlSprite ;
    logo:TSfmlSprite ;
    procedure RunMenu();
    procedure setUpMusicAndSoundVolumes() ;
    procedure setWindowTitle();
    procedure setLogo();
  public
    procedure Run() ;
  end;

implementation
uses SysUtils, StrUtils, Types, Math, Classes,
  SfmlSystem,
  view, viewstatic, logic, helpers, sfmlutils ;

procedure TMain.RunMenu();
var lasttime,newtime:Single ;
    clock:TSfmlClock ;
begin
  mainmenu.setActive(true) ;

  clock:=TSfmlClock.Create() ;
  lasttime:=clock.ElapsedTime.AsSeconds() ;
  while Window.IsOpen do
  begin
    newtime:=clock.ElapsedTime.asSeconds() ;
    mainmenu.Frame(newtime-lasttime,window.MousePosition.X,window.MousePosition.Y) ;
    Cursor.Position:=SfmlVector2f(window.MousePosition.X,window.MousePosition.Y) ;

    if not mainmenu.isActive() then break ;

    lasttime:=newtime ;

    Window.Clear(SfmlBlack);
    Window.Draw(back) ;
    Window.Draw(logo) ;
    mainmenu.Render() ;
    Window.Draw(Cursor) ;
    Window.Display;
  end;
end;

procedure TMain.setLogo;
begin
  logo:=LoadSprite('images'+PATH_SEP+'logo.'+options.getLang()+'.png') ;
  logo.Position:=SfmlVector2f(0,0) ;
end;

procedure TMain.setUpMusicAndSoundVolumes;
begin
  if Music<>nil then Music.Volume:=IfThen(options.isMusicOn(),100,0) ;
end;

procedure TMain.setWindowTitle;
begin
  with TStringList.Create do begin
    LoadFromFile('text'+PATH_SEP+'common.dat.'+Self.options.getLang());
    Window.SetTitle(UTF8Decode(Values['caption'])) ;
    Free ;
  end;
end;

procedure TMain.Run() ;
var vobj:TView ;
    vsobj:TViewStatic ;
    lobj:TLogic ;
    Mode: TSfmlVideoMode;
    Icon:TSfmlImage ;
    caption:string ;
    i:Integer ;
    tmp:TStringDynArray  ;
const INTRO_MUSIC = 'music_main.ogg' ;
begin
  ChDir(ExtractFilePath(ParamStr(0))) ;
  Randomize() ;

  options:=TOptions.Create ;
  for i := 1 to ParamCount do begin
    tmp:=SplitString(ParamStr(i),'=') ;
    if Length(tmp)=2 then
      if tmp[0]='--lang' then
        options.setLang(tmp[1]) ;
    SetLength(tmp,0) ;
  end;

  Mode.Width := WINDOW_W;
  Mode.Height := WINDOW_H;
  Mode.BitsPerPixel := 32;
  {$ifndef Darwin}
  if not SfmlVideoModeIsValid(Mode) then
    raise Exception.Create('Invalid video mode');
  {$endif}
  Window := TSfmlRenderWindow.Create(Mode, UTF8Decode('Window'),
    [sfResize, sfClose], nil);
  Window.SetVerticalSyncEnabled(True);
  Window.setFramerateLimit(60);
  Window.SetMouseCursorVisible(False);

  setWindowTitle() ;
  setLogo() ;

  Icon:=TSfmlImage.Create('images'+PATH_SEP+'icon.png') ;
  Window.SetIcon(Icon.Size.X,Icon.Size.Y,Icon.getPixelsPtr());

  Cursor:=loadSprite('images'+PATH_SEP+'cursor.png');
  Cursor.Origin:=SfmlVector2f(0,10) ;

  options.setProcSetMusicAndSound(setUpMusicAndSoundVolumes) ;
  options.addProcSetLanguage(setWindowTitle) ;
  options.addProcSetLanguage(setLogo) ;
  mainmenu:=TMainMenu.Create(window,'',options,'newgame',+110) ;

  back:=LoadSprite('images'+PATH_SEP+'intro.png') ;
  back.Position:=SfmlVector2f(0,0) ;

  Music:=TSFMLMusic.Create('music'+PATH_SEP+INTRO_MUSIC) ;
  Music.Loop:=True ;
  Music.Play() ;

  RunMenu() ;
  if mainmenu.getExitCode()='' then begin
    window.Close() ;
    Exit ;
  end;

  options.delProcSetLanguage(setLogo) ;

  vsobj:=TViewStatic.Create(window) ;
  with TStringList.Create do begin
    LoadFromFile('text'+PATH_SEP+'common.dat.'+Self.options.getLang());
    vsobj.AddTask(Values['intro1'].Replace('\n',#10),48) ;
    vsobj.AddTask(Values['intro2'].Replace('\n',#10),32) ;
    vsobj.AddTask(Values['intro3'].Replace('\n',#10),32) ;
    Free ;
  end;
  vsobj.Run() ;
  vsobj.Free ;

  lobj:=TLogic.Create();
  vobj:=TView.Create(lobj,window,options) ;
  vobj.insertMusic(music,INTRO_MUSIC) ;
  vobj.Run ;
  vobj.Free ;
end ;

end.
