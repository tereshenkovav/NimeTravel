unit ScriptExecutor;

interface

uses
  Classes, SysUtils,
  uPSUtils,uPSCompiler,uPSRuntime,
  Logic ;

type
  TScriptExecutor = class
  private
    lobj:TLogic ;
    procedure execProc(filename,proc:string) ;
  public
    procedure execSceneProc(scene, proc: string);
    procedure execScriptProc(script, proc: string);
    constructor Create(obj:TLogic) ;
  end;

implementation
uses Math,
  commonproc, helpers ;

{ TScriptExecutor }

constructor TScriptExecutor.Create(obj: TLogic);
begin
  lobj:=obj ;
end;

{$IFDEF UNICODE}
function ScriptOnUses(Sender: TPSPascalCompiler; const Name: AnsiString): Boolean;
{$ELSE}
function ScriptOnUses(Sender: TPSPascalCompiler; const Name: string): Boolean;
{$ENDIF}
begin
  if Name = 'SYSTEM' then
  begin

    Sender.AddTypeS('TGOProperty','(pFileName,pIcoFile,pCaption,pX,pY)') ;

    with Sender.AddClassN(nil, 'TLogic') do
    begin
      RegisterMethod('procedure addActiveObject(code, filename, icofile, caption: string; x, y: Integer; callbackinfo, callbackspell: string; way_idx: Integer)');
      RegisterMethod('procedure addPassiveObject(code, filename: string; x, y: Integer; z:Single)');
      RegisterMethod('procedure setBackground(filename:string)');
      RegisterMethod('procedure setMusic(filename:string)');
      RegisterMethod('procedure setText(text:string)');
      RegisterMethod('procedure setTextColored(text:string; color:Integer)');
      RegisterMethod('procedure clearText()');
      RegisterMethod('procedure wait(duration:Integer)');
      RegisterMethod('procedure addWayPoint(idx:Integer; x,y:Integer; z:Single; isout:Boolean)') ;
      RegisterMethod('procedure addWayLink(Alinks:array of Integer)') ;
      RegisterMethod('procedure setPlayerWayIdx(idx:Integer)') ;
      RegisterMethod('procedure setFlag(flagname:string)') ;
      RegisterMethod('procedure clearFlag(flagname:string)') ;
      RegisterMethod('function isFlagSet(flagname:string):Boolean') ;
      RegisterMethod('procedure genSpell(spellcode:Integer; spelllen:Integer; minincpos:Integer; symmetric:Boolean)') ;
      RegisterMethod('procedure setSpell(spellcode:Integer; spellseq:array of Integer)') ;
      RegisterMethod('procedure playSpell(spellcode:Integer; reverse:Boolean)') ;
      RegisterMethod('procedure goPicture(picture:string; text:string)') ;
      RegisterMethod('procedure goScene(scene:string; initx:Integer; inity:Integer; initz:Single; way_idx:Integer)') ;
      RegisterMethod('procedure lookPlayerLeft()') ;
      RegisterMethod('procedure lookPlayerRight()') ;
      RegisterMethod('procedure delActiveObject(code:string)') ;
      RegisterMethod('procedure setActiveObjectX(code: string; value: Integer)');
      RegisterMethod('procedure setActiveObjectY(code: string; value: Integer)');
      RegisterMethod('procedure setActiveObjectTransp(code: string; value: Integer)');
      RegisterMethod('procedure setActiveObjectFileName(code: string; value: string)');
      RegisterMethod('procedure setActiveObjectIcoFile(code: string; value: string)');
      RegisterMethod('procedure setActiveObjectCaption(code: string; value: string)');
      RegisterMethod('procedure setAllowedMarkerCount(value:Integer)') ;
      RegisterMethod('procedure writeLog(value:string)') ;
      RegisterMethod('procedure delWayPoint(idx:Integer)') ;
    end;

    AddImportedClassVariable(Sender, 'game', 'TLogic');

    Result := True;
  end else
    Result := False;
end;

function ErrorCode2Str(code:TPSError):string ;
begin
case code of
  ErNoError:                   Result:='ErNoError' ;
  erCannotImport:              Result:='erCannotImport' ;
  erInvalidType:               Result:='erInvalidType' ;
  ErInternalError:             Result:='ErInternalError' ;
  erInvalidHeader:             Result:='erInvalidHeader' ;
  erInvalidOpcode:             Result:='erInvalidOpcode' ;
  erInvalidOpcodeParameter:    Result:='erInvalidOpcodeParameter' ;
  erNoMainProc:                Result:='erNoMainProc' ;
  erOutOfGlobalVarsRange:      Result:='erOutOfGlobalVarsRange' ;
  erOutOfProcRange:            Result:='erOutOfProcRange' ;
  ErOutOfRange:                Result:='ErOutOfRange' ;
  erOutOfStackRange:           Result:='erOutOfStackRange' ;
  ErTypeMismatch:              Result:='ErTypeMismatch' ;
  erUnexpectedEof:             Result:='erUnexpectedEof' ;
  erVersionError:              Result:='erVersionError' ;
  ErDivideByZero:              Result:='ErDivideByZero' ;
  ErMathError:                 Result:='ErMathError' ;
  erCouldNotCallProc:          Result:='erCouldNotCallProc' ;
  erOutofRecordRange:          Result:='erOutofRecordRange' ;
  erOutOfMemory:               Result:='erOutOfMemory' ;
  erException:                 Result:='erException' ;
  erNullPointerException:      Result:='erNullPointerException' ;
  erNullVariantError:          Result:='erNullVariantError' ;
  erInterfaceNotSupported:     Result:='erInterfaceNotSupported' ;
  erCustomError:               Result:='erCustomError' ;
end;
end ;

procedure getError(Sender: TPSExec; ExError: TPSError; const ExParam: tbtstring; ExObject: TObject; ProcNo, Position: Cardinal);
begin
  Writeln('Error code: ',ErrorCode2Str(ExError)) ;
  Writeln(ExParam) ;
end ;

procedure TScriptExecutor.execSceneProc(scene, proc: string);
begin
  execProc('scenes'+PATH_SEP+scene+PATH_SEP+scene+'.script',proc) ;
end;

procedure TScriptExecutor.execScriptProc(script, proc: string);
begin
  execProc('scripts'+PATH_SEP+script,proc) ;
end;

procedure TScriptExecutor.execProc(filename, proc: string);
var i:Integer ;
    Compiler: TPSPascalCompiler;
    Exec: TPSExec;
    {$IFDEF UNICODE}Data: AnsiString;{$ELSE}Data: string;{$ENDIF}
    CI: TPSRuntimeClassImporter;
    scriptbody:string ;
begin
  scriptbody:='const ENGINE=0.1 ;'+#13#10 ;
  scriptbody:=scriptbody+ReadAllText('scripts'+PATH_SEP+'consts.inc')+#13#10 ;
  scriptbody:=scriptbody+ReadAllText('scripts'+PATH_SEP+'userproc.script')+#13#10 ;
  scriptbody:=scriptbody+ReadAllText(filename)+#13#10 ;

  Compiler := TPSPascalCompiler.Create;
  Compiler.OnUses := ScriptOnUses;
  if not Compiler.Compile(scriptbody+'begin '+proc+' ; end.') then
  begin
    Writeln('Error compile') ;
    for i:=0 to Compiler.MsgCount-1 do
      Writeln(Compiler.Msg[i].MessageToString) ;
    Compiler.Free;
    Exit;
  end;

  Compiler.GetOutput(Data);
  Compiler.Free;

  CI := TPSRuntimeClassImporter.Create;

  with CI.Add(TLogic) do
  begin
    RegisterMethod(@TLogic.setBackground, 'setBackground');
    RegisterMethod(@TLogic.setMusic, 'setMusic');
    RegisterMethod(@TLogic.addActiveObject, 'addActiveObject');
    RegisterMethod(@TLogic.addPassiveObject, 'addPassiveObject');
    RegisterMethod(@TLogic.setText, 'setText');
    RegisterMethod(@TLogic.setTextColored, 'setTextColored');
    RegisterMethod(@TLogic.clearText, 'clearText');
    RegisterMethod(@TLogic.wait, 'wait');
    RegisterMethod(@TLogic.addWayPoint, 'addWayPoint');
    RegisterMethod(@TLogic.addWayLink, 'addWayLink');
    RegisterMethod(@TLogic.setPlayerWayIdx, 'setPlayerWayIdx');
    RegisterMethod(@TLogic.setFlag, 'setFlag');
    RegisterMethod(@TLogic.clearFlag, 'clearFlag');
    RegisterMethod(@TLogic.isFlagSet, 'isFlagSet');
    RegisterMethod(@TLogic.genSpell, 'genSpell');
    RegisterMethod(@TLogic.setSpell, 'setSpell');
    RegisterMethod(@TLogic.playSpell, 'playSpell');
    RegisterMethod(@TLogic.goPicture, 'goPicture');
    RegisterMethod(@TLogic.goScene, 'goScene');
    RegisterMethod(@TLogic.lookPlayerLeft, 'lookPlayerLeft');
    RegisterMethod(@TLogic.lookPlayerRight, 'lookPlayerRight');
    RegisterMethod(@TLogic.delActiveObject, 'delActiveObject');
    RegisterMethod(@TLogic.setActiveObjectX,'setActiveObjectX') ;
    RegisterMethod(@TLogic.setActiveObjectY,'setActiveObjectY') ;
    RegisterMethod(@TLogic.setActiveObjectTransp,'setActiveObjectTransp') ;
    RegisterMethod(@TLogic.setActiveObjectFileName,'setActiveObjectFileName') ;
    RegisterMethod(@TLogic.setActiveObjectIcoFile,'setActiveObjectIcoFile') ;
    RegisterMethod(@TLogic.setActiveObjectCaption,'setActiveObjectCaption') ;
    RegisterMethod(@TLogic.setAllowedMarkerCount,'setAllowedMarkerCount') ;
    RegisterMethod(@TLogic.writeLog,'writeLog') ;
    RegisterMethod(@TLogic.delWayPoint,'delWayPoint') ;
  end;

  Exec := TPSExec.Create;
  Exec.OnException:=getError ;

  RegisterClassLibraryRuntime(Exec, CI);

  if not Exec.LoadData(Data) then
  begin
    Writeln('Error load') ;
    Exec.Free;
    Exit;
  end;

  SetVariantToClass(Exec.GetVarNo(Exec.GetVar('game')), lobj);

  Exec.RunScript;
  Exec.Free;
  CI.Free;

end;

end.

