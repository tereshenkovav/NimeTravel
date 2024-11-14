unit main ;

interface
uses SfmlWindow, SfmlGraphics, SfmlAudio,
  mainmenu, commonclasses ;

type
  TMain = class
  private
  public
    procedure Run() ;
  end;

implementation
uses SysUtils, StrUtils, Types, Math, Classes,
  SfmlSystem,
  Game, CommonProc, CommonData,
  view, viewstatic, logic, helpers, sfmlutils ;

procedure TMain.Run() ;
var game:TGame ;
begin
  ChDir(ExtractFilePath(ParamStr(0))) ;
  Randomize() ;
  TCommonData.Init() ;
  game:=TGame.Create(WINDOW_W,WINDOW_H,'NimeTravel',
    getWindowTitle(),'images'+PATH_SEP+'icon.png') ;

  TCommonData.setProfile(game.getProfile()) ;
  TCommonData.LoadMusic(TCommonData.INTRO_MUSIC) ;

  game.Run(TMainMenu.Create(true,'newgame',+110)) ;
  game.Free ;
  TCommonData.UnInit() ;
end ;

end.
