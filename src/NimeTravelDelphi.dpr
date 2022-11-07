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
  viewstatic in 'core\viewstatic.pas';

begin
  with TMain.Create() do begin
    Run() ;
    Free ;
  end;
end.
