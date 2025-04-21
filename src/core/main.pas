unit main ;

interface
uses SfmlWindow, SfmlGraphics, SfmlAudio,
  mainmenu, commonclasses ;

type
  TMain = class
  private
  public
    procedure Run() ;
    procedure GenTestSpells() ;
  end;

implementation
uses SysUtils, StrUtils, Types, Math, Classes,
  SfmlSystem,
  Game, CommonProc, CommonData, Spell, SceneCloseHandler, Logger, ScreenSaver,
  view, viewstatic, logic, helpers, sfmlutils, SceneMagicRender ;

const WINDOW_W=800 ;
      WINDOW_H=600 ;

procedure TMain.GenTestSpells;
var f:textfile ;
    i:Integer ;
    sp:TSpell ;
    spells:TUniDictionary<Integer,TSpell> ;
begin
  spells:=TUniDictionary<Integer,TSpell>.Create() ;

  AssignFile(f,'spells.log') ;
  ReWrite(f) ;
  for i := 0 to 9 do begin
    sp.GenByParam(3, 2, False, spells);
    Writeln(f,sp.ToString()) ;
  end ;
  Writeln(f) ;
  for i := 0 to 9 do begin
    sp.GenByParam(3, 3, True, spells);
    Writeln(f,sp.ToString()) ;
  end ;
  Writeln(f) ;
  for i := 0 to 9 do begin
    sp.GenByParam(4, 3, False, spells);
    Writeln(f,sp.ToString()) ;
  end ;
  Writeln(f) ;
  for i := 0 to 9 do begin
    sp.GenByParam(4, 4, True, spells);
    Writeln(f,sp.ToString()) ;
  end ;
  Writeln(f) ;
  for i := 0 to 9 do begin
    sp.GenByParam(5, 4, False, spells);
    Writeln(f,sp.ToString()) ;
  end ;
  Writeln(f) ;
  for i := 0 to 9 do begin
    sp.GenByParam(5, 5, True, spells);
    Writeln(f,sp.ToString()) ;
  end ;
  spells.Free ;
  CloseFile(f) ;
end;

procedure TMain.Run() ;
var game:TGame ;
begin
  Randomize() ;

  if FileExists('developer') then GenTestSpells() ;

  ChDir(ExtractFilePath(ParamStr(0))) ;
  TCommonData.Init(WINDOW_W,WINDOW_H) ;
  game:=TGame.Create(WINDOW_W,WINDOW_H,'NimeTravel',
    TCommonData.texts.getText('caption'),'images'+PATH_SEP+'icon.png') ;
  game.setCloseHandler(TSceneCloseHandler.Create()) ;
  game.setCustomLogger(TLoggerBasic) ;
  TCommonData.setProfile(game.getProfile()) ;

  if FileExists('developer') then game.setCustomScreenSaver(
    TScreenSaverStd.Create(game.getGameCode(),sfKeyF12,sfKeyF8,sfKeyF9)) ;

  game.Run(TMainMenu.Create()) ;
  game.Free ;
  TCommonData.UnInit() ;
end ;

end.
