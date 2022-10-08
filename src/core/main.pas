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
    procedure RunMenu();
    procedure setUpMusicAndSoundVolumes() ;
  public
    procedure Run() ;
  end;

implementation
uses SysUtils, Math, Classes,
  SfmlSystem,
  view, logic, helpers, sfmlutils ;

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
    mainmenu.Render() ;
    Window.Draw(Cursor) ;
    Window.Display;
  end;
end;

procedure TMain.setUpMusicAndSoundVolumes;
begin
  if Music<>nil then Music.Volume:=IfThen(options.isMusicOn(),100,0) ;
end;

procedure TMain.Run() ;
var vobj:TView ;
    lobj:TLogic ;
    Mode: TSfmlVideoMode;
    Icon:TSfmlImage ;
    caption:string ;
const INTRO_MUSIC = 'music_main.ogg' ;
begin
  ChDir(ExtractFilePath(ParamStr(0))) ;
  Randomize() ;

  with TStringList.Create do begin
    LoadFromFile('text'+PATH_SEP+'common.dat');
    caption:=Values['caption'] ;
    Free ;
  end;

  Mode.Width := WINDOW_W;
  Mode.Height := WINDOW_H;
  Mode.BitsPerPixel := 32;
  {$ifndef Darwin}
  if not SfmlVideoModeIsValid(Mode) then
    raise Exception.Create('Invalid video mode');
  {$endif}
  Window := TSfmlRenderWindow.Create(Mode, UTF8Decode(caption),
    [sfResize, sfClose], nil);
  Window.SetVerticalSyncEnabled(True);
  Window.setFramerateLimit(60);
  Window.SetMouseCursorVisible(False);

  Icon:=TSfmlImage.Create('images'+PATH_SEP+'icon.png') ;
  Window.SetIcon(Icon.Size.X,Icon.Size.Y,Icon.getPixelsPtr());

  Cursor:=loadSprite('images'+PATH_SEP+'cursor.png');
  Cursor.Origin:=SfmlVector2f(0,10) ;

  options:=TOptions.Create ;
  options.setProcSetMusicAndSound(setUpMusicAndSoundVolumes) ;
  mainmenu:=TMainMenu.Create(window,'images'+PATH_SEP+'intro.png',options,'newgame',+110) ;

  Music:=TSFMLMusic.Create('music'+PATH_SEP+INTRO_MUSIC) ;
  Music.Loop:=True ;
  Music.Play() ;

  RunMenu() ;
  if mainmenu.getExitCode()='' then begin
    window.Close() ;
    Exit ;
  end;

  lobj:=TLogic.Create();
  vobj:=TView.Create(lobj,window,options) ;
  vobj.insertMusic(music,INTRO_MUSIC) ;
  vobj.Run ;
  vobj.Free ;
end ;

end.
