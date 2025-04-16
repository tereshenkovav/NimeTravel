program NimeTravelDelphi;

{$APPTYPE GUI}

{$R *.res}

uses
  logic in 'core\logic.pas',
  view in 'core\view.pas',
  gameobject in 'core\gameobject.pas',
  main in 'core\main.pas',
  commonclasses in 'core\commonclasses.pas',
  wayrenderer in 'core\wayrenderer.pas',
  commonproc in 'core\commonproc.pas',
  scriptexecutor in 'core\scriptexecutor.pas',
  waysearch in 'core\waysearch.pas',
  mainmenu in 'core\mainmenu.pas',
  viewstatic in 'core\viewstatic.pas',
  commondata in 'core\commondata.pas',
  spell in 'core\spell.pas',
  sceneclosehandler in 'core\sceneclosehandler.pas',
  sceneconfirmnewgame in 'core\sceneconfirmnewgame.pas',
  scenetextview in 'core\scenetextview.pas',
  scenejournalview in 'core\scenejournalview.pas',
  scenedialogsview in 'core\scenedialogsview.pas',
  scenemagicrender in 'core\scenemagicrender.pas',
  auramaker in 'core\auramaker.pas';

begin
  with TMain.Create() do begin
    Run() ;
    Free ;
  end;
end.
