unit main ;

interface
uses SfmlWindow, SfmlGraphics, SfmlAudio,
  mainmenu, commonclasses ;

type
  TMain = class
  private
    Music:TSfmlMusic ;
    options:TOptions ;
  public
    procedure Run() ;
  end;

implementation
uses SysUtils, StrUtils, Types, Math, Classes,
  SfmlSystem,
  Game, CommonProc,
  view, viewstatic, logic, helpers, sfmlutils ;

procedure TMain.Run() ;
var game:TGame ;
const INTRO_MUSIC = 'music_main.ogg' ;
begin
  ChDir(ExtractFilePath(ParamStr(0))) ;
  Randomize() ;
  options:=TOptions.Create ;
  game:=TGame.Create(WINDOW_W,WINDOW_H,'NimeTravel',
    getWindowTitle(options),'images'+PATH_SEP+'icon.png') ;

  Music:=TSFMLMusic.Create('music'+PATH_SEP+INTRO_MUSIC) ;
  Music.Loop:=True ;
  Music.Play() ;

  game.Run(TMainMenu.Create(true,options,'newgame',+110)) ;
  game.Free ;
end ;

end.
