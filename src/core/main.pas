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
  Game, CommonProc, CommonData, Spell,
  view, viewstatic, logic, helpers, sfmlutils ;

procedure TMain.GenTestSpells;
var f:textfile ;
    i:Integer ;
    sp:TSpell ;
begin
  AssignFile(f,'spells.log') ;
  ReWrite(f) ;
  for i := 0 to 9 do begin
    sp.GenByParam(3, 2, False);
    Writeln(f,sp.ToString()) ;
  end ;
  Writeln(f) ;
  for i := 0 to 9 do begin
    sp.GenByParam(3, 3, True);
    Writeln(f,sp.ToString()) ;
  end ;
  Writeln(f) ;
  for i := 0 to 9 do begin
    sp.GenByParam(4, 3, False);
    Writeln(f,sp.ToString()) ;
  end ;
  Writeln(f) ;
  for i := 0 to 9 do begin
    sp.GenByParam(4, 4, True);
    Writeln(f,sp.ToString()) ;
  end ;
  Writeln(f) ;
  for i := 0 to 9 do begin
    sp.GenByParam(5, 4, False);
    Writeln(f,sp.ToString()) ;
  end ;
  Writeln(f) ;
  for i := 0 to 9 do begin
    sp.GenByParam(5, 5, True);
    Writeln(f,sp.ToString()) ;
  end ;
  CloseFile(f) ;
end;

procedure TMain.Run() ;
var game:TGame ;
begin
  Randomize() ;

  if FileExists('developer') then GenTestSpells() ;

  ChDir(ExtractFilePath(ParamStr(0))) ;
  TCommonData.Init() ;
  game:=TGame.Create(WINDOW_W,WINDOW_H,'NimeTravel',
    TCommonData.texts.getText('caption'),'images'+PATH_SEP+'icon.png') ;

  TCommonData.setProfile(game.getProfile()) ;

  game.Run(TMainMenu.CreateAsMainMenu()) ;
  game.Free ;
  TCommonData.UnInit() ;
end ;

end.
