unit Logic;

interface

uses
  Classes, SysUtils, syncobjs,
  Logger,
  gameobject, helpers, commonclasses, spell,
  ObjectSaver ;

type

  { TLogic }

  TLogic = class(TThread)
  protected
    procedure Execute; override;
  private
    activeobjects:TUniList<TGameObject> ;
    way:TUniList<TWayPoint> ;
    links:TUniList<TWayLink> ;
    dialogjournal:TUniList<TDialogText> ;
    world_activeobjects:TUniDictionary<string,TObject> ;
    world_way:TUniDictionary<string,TObject> ;
    world_links:TUniDictionary<string,TObject> ;
    world_backgrounds:TUniDictionary<string,string> ;
    spells:TUniDictionary<Integer,TSpell> ;
    flags:TStringList ;
    background:string ;
    musicfile:string ;
    herox:Single ;
    heroy:Single ;
    heroz:Single ;
    activescene:string ;
    allowedmarkercount:Integer ;

    dialogtext:string ;
    dialogcolor:Cardinal ;
    defaultcolor:Cardinal ;
    sect: TCriticalSection;
    scriptname:string ;
    isfinished:Boolean ;
    inscript:Boolean ;
    picturemode:Boolean ;
    breakwait:Boolean ;
    ActiveSpell:TSpell ;
    IsActiveSpellReverse:Boolean ;
    newway_idx:Integer ;
    lookState:TLookState ;
    logger:TLogger ;
    function WriteLogic(obj:TObject; store:TWriterAPI):Boolean ;
    function ReadLogic(obj:TObject; store:TReaderAPI):Boolean ;
    function WriteSpell(rec:Pointer; store:TWriterAPI):Boolean ;
    function ReadSpell(rec:Pointer; store:TReaderAPI):Boolean ;
    function WriteGameObject(obj:TObject; store:TWriterAPI):Boolean ;
    function ReadGameObject(obj:TObject; store:TReaderAPI):Boolean ;
    function WriteWayPoint(rec:Pointer; store:TWriterAPI):Boolean ;
    function ReadWayPoint(rec:Pointer; store:TReaderAPI):Boolean ;
    function WriteWayLink(rec:Pointer; store:TWriterAPI):Boolean ;
    function ReadWayLink(rec:Pointer; store:TReaderAPI):Boolean ;
    function WriteDialogText(rec:Pointer; store:TWriterAPI):Boolean ;
    function ReadDialogText(rec:Pointer; store:TReaderAPI):Boolean ;
    procedure LoadFromFile() ;
    class function getSaveGameFileName():string ;
    procedure RestoreSceneFromWorld() ;
  public

    procedure BeginWork() ;
    function getBackground():string ;
    function getMusic():string ;
    function getActiveObjects():TUniList<TGameObject> ;
    function getHeroX():Integer ;
    function getHeroY():Integer ;
    function getHeroZ():Single ;
    function getDialogText():string ;
    function getDialogColor():Cardinal ;
    procedure moveHeroXYZ(dx,dy,dz:Single) ;
    procedure setHeroXYZ(x,y,z:Single) ;
    function getWayPointCount():Integer ;
    function getWayLinkCount():Integer ;
    function getWayPoint(i:Integer):TWayPoint ;
    function getWayLink(i:Integer):TWayLink ;
    function buildWayStack(x,y:Integer):TUniList<Integer> ;
    function findSpell(test:TUniList<Integer>; var spellcode:Integer; var reverse:Boolean):Boolean ;
    procedure addToSpellStackIfNeed(spellstack:TUniList<Integer>) ;
    function isNewTarget(var way_idx:Integer):Boolean ;
    function isLookLeft():Boolean ;
    function isLookRight():Boolean ;
    procedure executeInfoProc(obj:TGameObject) ;
    procedure executeSpellProc(obj:TGameObject; spellcode:Integer; reverse:Boolean) ;
    procedure executeWayProc(wayidx:Integer) ;
    procedure executeUserProc(procname:string) ;
    procedure executeScript(filename,procname:string) ;
    procedure EndWork() ;
    procedure SendExit() ;
    function isPictureMode():Boolean ;
    procedure sendSkipClick(skipclick:TSkipClick) ;
    function isGameObjectExist(obj:TGameObject):Boolean ; overload ;
    function isGameObjectExist(code:string):Boolean ; overload ;
    function findGameObject(code:string):TGameObject ;
    function isLinkExist(way1,way2:Integer):Boolean ;
    function isWayOut(idx:Integer):Boolean ;
    function getActiveScenePath():string ;
    function getActiveScene():string ;
    function getAllowerMarkerCount():Integer ;
    function getActivatedSpells():TUniList<TSpell> ;
    function getDialogJournal():TUniList<TDialogText> ;
    function isInScript():Boolean ;
    procedure addDialogOnTopIfNew(text:string; color:Cardinal) ;
    // Методы, публикуемые в скрипте
    procedure setBackground(filename:string) ;
    procedure setMusic(filename:string) ;
    procedure addActiveObject(code,filename,icofile,caption:string; x,y:Integer;
      callbackinfo,callbackspell:string; way_idx:Integer) ;
    procedure addPassiveObject(code,filename:string; x,y:Integer; z:Single) ;
    procedure setText(text:string) ;
    procedure setTextColored(text:string; color:Cardinal) ;
    procedure wait(duration:Integer) ;
    procedure clearText() ;
    procedure addWayPoint(idx:Integer; x:Integer; y:Integer; z:Single; isout:Boolean=False) ;
    procedure addWayLink(Alinks:array of Integer) ;
    procedure setPlayerWayIdx(idx:Integer) ;
    procedure lookPlayerLeft() ;
    procedure lookPlayerRight() ;
    procedure setFlag(flagname:string) ;
    procedure clearFlag(flagname:string) ;
    function isFlagSet(flagname:string):Boolean ;
    procedure genSpell(spellcode:Integer; spelllen:Integer; minincpos:Integer;
      symmetric:Boolean; icoscene,icofile:string) ;
    procedure setSpell(spellcode:Integer; spellseq:array of Integer;
      icoscene,icofile:string) ;
    procedure playSpell(spellcode:Integer; reverse:Boolean) ;
    procedure goPicture(picture:string; text:string) ;
    procedure goScene(newactivescene:string; initx,inity:Integer; initz:Single; way_idx:Integer) ;
    procedure delActiveObject(code:string) ;
    procedure setActiveObjectX(code:string; value:Integer) ;
    procedure setActiveObjectY(code:string; value:Integer) ;
    procedure setActiveObjectTransp(code:string; value:Integer) ;
    procedure setActiveObjectFileName(code:string; value:string) ;
    procedure setActiveObjectIcoFile(code:string; value:string) ;
    procedure setActiveObjectCaption(code:string; value:string) ;
    procedure setAllowedMarkerCount(value:Integer) ;
    procedure writeLog(value:string) ;
    procedure delWayPoint(idx:Integer) ;
    //
    constructor Create(Alogger:TLogger) ;
    destructor Destroy ; override ;
    procedure SaveToFile() ;
    class function isSaveGameExist():Boolean ;
    class procedure clearSaveGame() ;
  end;

implementation
uses Math, StrUtils,
  {$ifndef fpc}IOUtils,{$endif}
  commonproc, ScriptExecutor, waysearch, HomeDir ;

function formatSpellIcon(icoscene,icofile:string):string ;
begin
  Result:='scenes'+PATH_SEP+icoscene+PATH_SEP+icofile ;
end;

const LOGIC_VERSION = 1 ; // Данные для сериализатора

{ TLogic }

function TLogic.buildWayStack(x, y: Integer): TUniList<Integer>;

begin
  with TWaySearch.Create(links,way) do begin
    Result:=buildWayStack(getHeroX(),getHeroY(),x,y) ;
    Free ;
  end;
end;

constructor TLogic.Create(Alogger:TLogger);
begin
  inherited Create(True);

  world_activeobjects:=TUniDictionary<string,TObject>.Create() ;
  world_way:=TUniDictionary<string,TObject>.Create() ;
  world_links:=TUniDictionary<string,TObject>.Create() ;
  world_backgrounds:=TUniDictionary<string,string>.Create() ;
  dialogjournal:=TUniList<TDialogText>.Create ;

  flags:=TStringList.Create() ;
  spells:=TUniDictionary<Integer,TSpell>.Create() ;

  logger:=Alogger;

  isfinished:=False ;
  sect:=TCriticalSection.Create;
  herox:=0 ;
  heroy:=0 ;
  scriptname:='' ;
  inscript:=False ;
  background:='' ;
  musicfile:='' ;
  newway_idx:=-1 ;

  defaultcolor:=$ffdef7 ;
  ActiveSpell.len:=0 ;

  if isSaveGameExist() then begin
    LoadFromFile() ;
    RestoreSceneFromWorld() ;
  end
  else begin
    with TScriptExecutor.Create(Self) do begin
      execScriptProc('gen_spells.script','genSpells()') ;
      Free ;
    end;
    goScene('scene4',0,0,0.0,-1) ;
  end;
end;

procedure TLogic.delActiveObject(code: string);
var i:Integer ;
begin
  BeginWork() ;
  for i:= 0 to activeobjects.Count-1 do
     if activeobjects[i].code=code then begin
       activeobjects.Delete(i);
       Break ;
     end;
  EndWork() ;
end;

procedure TLogic.delWayPoint(idx: Integer);
var i:Integer ;
begin
  BeginWork() ;
  for i:=0 to way.Count-1 do
    if way[i].idx=idx then begin
      way.Delete(i);
      Break ;
    end;
  i:=0 ;
  while i<links.Count do begin
    if (links[i].idx1=idx)or(links[i].idx2=idx) then links.Delete(i) else Inc(i) ;
  end;
  EndWork() ;
end;

procedure TLogic.Execute;
var localscript:string ;
begin
  while not isfinished do begin
    BeginWork() ;
    localscript:=scriptname ;
    if scriptname<>'' then scriptname:='' ;
    EndWork() ;
    if localscript<>'' then begin
      inscript:=True ;
      with TScriptExecutor.Create(Self) do begin
        execSceneProc(activescene,localscript);
        Free ;
      end;
      inscript:=False ;
    end;
    Sleep(10) ;
  end;
  sect.Free ;
  way.Free ;

  activeobjects.Free ;
end;

procedure TLogic.executeInfoProc(obj: TGameObject);
begin
  scriptname:=obj.callbackinfo+'('''+obj.code+''')' ;
end;

procedure TLogic.executeScript(filename, procname: string);
begin
  with TScriptExecutor.Create(Self) do begin
    execScriptProc(filename,procname) ;
    Free ;
  end;
end;

procedure TLogic.executeSpellProc(obj: TGameObject; spellcode: Integer; reverse:Boolean);
begin
  scriptname:=Format('%s(''%s'',%d,%s)',
    [obj.callbackspell,obj.code,spellcode,IfThen(reverse,'True','False')]) ;
end;

procedure TLogic.executeWayProc(wayidx: Integer);
begin
  scriptname:='ProcCallBackWay('+IntToStr(wayidx)+')' ;
end;

procedure TLogic.executeUserProc(procname:string);
begin
  scriptname:=procname+'()' ;
end;

function TLogic.findSpell(test: TUniList<Integer>; var spellcode: Integer; var reverse:Boolean): Boolean;
var code:Integer ;
begin
  for code in spells.AllKeys do begin
    if spells[code].isSpellMatchDirect(test) then begin
      spellcode:=code ;
      reverse:=False ;
      Exit(True) ;
    end;
    if spells[code].isSpellMatchReverse(test) then begin
      spellcode:=code ;
      reverse:=True ;
      Exit(True) ;
    end;
  end;
  Result:=False ;
end;

procedure TLogic.BeginWork();
begin
  sect.Enter ;
end;

procedure TLogic.genSpell(spellcode, spelllen, minincpos: Integer;
  symmetric: Boolean; icoscene,icofile:string);
var spell:TSpell ;
begin
  if spell.GenByParam(spelllen,minincpos,symmetric,spells) then begin
    spell.iconfile:=formatSpellIcon(icoscene,icofile) ;
    spells.Add(spellcode,spell);
  end
  else
    raise Exception.CreateFmt('Cant create spell with code: %d',[0]);
end;

function TLogic.getActiveScene: string;
begin
  Result:=activescene ;
end;

function TLogic.getActiveScenePath: string;
begin
  Result:='scenes'+PATH_SEP+activescene+PATH_SEP ;
end;

function TLogic.getAllowerMarkerCount: Integer;
begin
  Result:=allowedmarkercount ;
end;

function TLogic.getActivatedSpells: TUniList<TSpell>;
var code:Integer ;
begin
  Result:=TUniList<TSpell>.Create() ;
  for code in spells.AllKeys do
    if spells[code].activated then
      Result.Add(spells[code]) ;
end;

function TLogic.getActiveObjects: TUniList<TGameObject>;
begin
  Result:=activeobjects ;
end;

function TLogic.getBackground: string;
begin
  Result:=background ;
end;

function TLogic.getDialogText: string;
begin
  Result:=dialogtext ;
end;

function TLogic.getHeroX(): Integer;
begin
  Result:=Round(herox) ;
end;

function TLogic.getHeroY(): Integer;
begin
  Result:=Round(heroy) ;
end;

function TLogic.getHeroZ: Single;
begin
  Result:=heroz ;
end;

function TLogic.getMusic: string;
begin
  Result:=musicfile ;
end;

class function TLogic.getSaveGameFileName: string;
begin
  Result:=THomeDir.getFileNameInHome('NimeTravel','game.json') ;
end;

function TLogic.getDialogColor: Cardinal;
begin
  Result:=dialogcolor ;
end;

function TLogic.getDialogJournal: TUniList<TDialogText>;
begin
  Result:=dialogjournal ;
end;

function TLogic.getWayLink(i: Integer): TWayLink;
begin
  Result:=links[i] ;
end;

function TLogic.getWayLinkCount: Integer;
begin
  Result:=links.Count ;
end;

function TLogic.getWayPoint(i: Integer): TWayPoint;
begin
  Result:=way[i] ;
end;

function TLogic.getWayPointCount: Integer;
begin
  Result:=way.Count ;
end;

procedure TLogic.moveHeroXYZ(dx, dy, dz: Single);
begin
  herox:=herox+dx ;
  heroy:=heroy+dy ;
  heroz:=heroz+dz ;
end;

procedure TLogic.playSpell(spellcode: Integer; reverse:Boolean);
begin
  if spells.ContainsKey(spellcode) then begin
    BeginWork() ;
    ActiveSpell:=spells[spellcode] ;
    ActiveSpell.activated:=True ;
    spells[spellcode]:=ActiveSpell ;
    IsActiveSpellReverse:=reverse ;
    EndWork() ;
  end;
end;

function TLogic.ReadDialogText(rec: Pointer; store: TReaderAPI): Boolean;
begin
  TDialogText(rec^).text:=store.ReadString('text') ;
  TDialogText(rec^).color:=store.ReadInteger('color') ;
end;

function TLogic.ReadGameObject(obj: TObject; store: TReaderAPI): Boolean;
begin
  TGameObject(obj).code:=store.ReadString('code') ;
  TGameObject(obj).filename:=store.ReadString('filename') ;
  TGameObject(obj).icofile:=store.ReadString('icofile') ;
  TGameObject(obj).caption:=store.ReadString('caption') ;
  TGameObject(obj).x:=store.ReadInteger('x') ;
  TGameObject(obj).y:=store.ReadInteger('y') ;
  TGameObject(obj).z:=store.ReadSingle('z') ;
  TGameObject(obj).isactive:=store.ReadBoolean('isactive') ;
  TGameObject(obj).callbackinfo:=store.ReadString('callbackinfo') ;
  TGameObject(obj).callbackspell:=store.ReadString('callbackspell') ;
  TGameObject(obj).way_idx:=store.ReadInteger('way_idx') ;
  TGameObject(obj).transp:=store.ReadInteger('transp') ;
end;

function TLogic.ReadLogic(obj: TObject; store: TReaderAPI): Boolean;
begin
  background:=store.ReadString('background') ;
  musicfile:=store.ReadString('musicfile') ;
  activescene:=store.ReadString('activescene') ;
  herox:=store.ReadSingle('herox') ;
  heroy:=store.ReadSingle('heroy') ;
  heroz:=store.ReadSingle('heroz') ;
  allowedmarkercount:=store.ReadInteger('allowedmarkercount') ;
  Result:=True ;
end;

function TLogic.ReadSpell(rec: Pointer; store: TReaderAPI): Boolean;
var i:Integer ;
begin
  TSpell(rec^).len:=store.ReadInteger('len') ;
  for i := 0 to TSpell(rec^).len-1 do
    TSpell(rec^).seq[i]:=store.ReadInteger(Format('seq_%d',[i])) ;
  TSpell(rec^).activated:=store.ReadBoolean('activated') ;
  TSpell(rec^).iconfile:=store.ReadString('iconfile') ;
  Result:=True ;
end;

function TLogic.ReadWayLink(rec: Pointer; store: TReaderAPI): Boolean;
begin
  TWayLink(rec^).idx1:=store.ReadInteger('idx1') ;
  TWayLink(rec^).idx2:=store.ReadInteger('idx2') ;
end;

function TLogic.ReadWayPoint(rec: Pointer; store: TReaderAPI): Boolean;
begin
  TWayPoint(rec^).idx:=store.ReadInteger('idx') ;
  TWayPoint(rec^).x:=store.ReadInteger('x') ;
  TWayPoint(rec^).y:=store.ReadInteger('y') ;
  TWayPoint(rec^).z:=store.ReadSingle('z') ;
  TWayPoint(rec^).isout:=store.ReadBoolean('isout') ;
end;

procedure TLogic.RestoreSceneFromWorld;
begin
  activeobjects:=TUniList<TGameObject>(world_activeobjects[activescene]) ;
  way:=TUniList<TWayPoint>(world_way[activescene]) ;
  links:=TUniList<TWayLink>(world_links[activescene]) ;
  background:=world_backgrounds[activescene] ;
end;

procedure TLogic.EndWork();
begin
  sect.Leave ;
end;

procedure TLogic.SaveToFile();
var saver:TObjectSaver ;
    code:Integer ;
    scode:string ;

procedure WriteAllJSON(filename:string; const data:string) ;
var f:textfile ;
begin
  {$ifdef fpc}
  AssignFile(f,filename) ;
  ReWrite(f) ;
  Write(f,data) ;
  CloseFile(f) ;
  {$else}
  TFile.WriteAllText(filename,data,TEncoding.UTF8) ;
  {$endif}
end ;

begin
  saver:=TObjectSaver.Create() ;
  saver.SystemSection().WriteInteger('logic_version',LOGIC_VERSION) ;
  saver.WriteObject('vars',Self,WriteLogic) ;
  saver.WriteStringList('flags',flags) ;

  saver.WriteArrayInteger('spells',spells.AllKeys) ;
  for code in spells.AllKeys do
    saver.WriteRecord<TSpell>(Format('spell_%d',[code]),spells[code],WriteSpell) ;

  saver.WriteArrayString('worlds',world_activeobjects.AllKeys) ;
  saver.WriteStringDictionary('world_backgrounds',world_backgrounds) ;
  for scode in world_activeobjects.AllKeys do begin
    saver.WriteObjectList(Format('activeobjects_%s',[scode]),
      TUniList<TObject>(world_activeobjects[scode]),WriteGameObject) ;
    saver.WriteRecordList<TWayPoint>(Format('ways_%s',[scode]),
      TUniList<TWayPoint>(world_way[scode]),WriteWayPoint) ;
    saver.WriteRecordList<TWayLink>(Format('links_%s',[scode]),
      TUniList<TWayLink>(world_links[scode]),WriteWayLink) ;
  end;

  saver.WriteRecordList<TDialogText>('dialogjournal',
    dialogjournal,WriteDialogText) ;

  THomeDir.createDirInHomeIfNeed('NimeTravel') ;
  WriteAllJSON(getSaveGameFileName(),saver.getData()) ;
  saver.Free ;
end;

procedure TLogic.LoadFromFile();
var loader:TObjectLoader ;
    codes:TArray<Integer> ;
    code:Integer ;
    scode:string ;
    scodes:TArray<string> ;
    spell:TSpell ;
    tmp_active_objects:TUniList<TGameObject> ;
    tmp_ways:TUniList<TWayPoint> ;
    tmp_links:TUniList<TWayLink> ;

function ReadJSONText(filename:string):string ;
begin
  {$ifdef fpc}
  with TStringList.Create do begin
    LoadFromFile(FileName) ;
    Result:=Text ;
    Free ;
  end ;
  {$else}
  Result:=TFile.ReadAllText(filename,TEncoding.UTF8) ;
  {$endif}
end;

begin
  loader:=TObjectLoader.Create(ReadJSONText(getSaveGameFileName())) ;
  loader.ReadObject('vars',Self,ReadLogic) ;
  loader.ReadStringList('flags',flags) ;

  spells.Clear() ;
  loader.ReadArrayInteger('spells',codes) ;
  for code in codes do begin
    loader.ReadRecord<TSpell>(Format('spell_%d',[code]),@spell,ReadSpell) ;
    spells.Add(code,spell) ;
  end;

  world_activeobjects.Clear() ;
  world_way.Clear() ;
  world_links.Clear() ;
  loader.ReadArrayString('worlds',scodes) ;
  loader.ReadStringDictionary('world_backgrounds',world_backgrounds) ;
  for scode in scodes do begin
    tmp_active_objects:=TUniList<TGameObject>.Create() ;
    loader.ReadObjectList(Format('activeobjects_%s',[scode]),
      TUniList<TObject>(tmp_active_objects),ReadGameObject,TGameObject) ;
    world_activeobjects.Add(scode,tmp_active_objects) ;

    tmp_ways:=TUniList<TWayPoint>.Create() ;
    loader.ReadRecordList<TWayPoint>(Format('ways_%s',[scode]),
      tmp_ways,ReadWayPoint) ;
    world_way.Add(scode,tmp_ways) ;

    tmp_links:=TUniList<TWayLink>.Create() ;
    loader.ReadRecordList<TWayLink>(Format('links_%s',[scode]),
      tmp_links,ReadWayLink) ;
    world_links.Add(scode,tmp_links) ;
  end;

  dialogjournal.Clear() ;
  loader.ReadRecordList<TDialogText>('dialogjournal',
    dialogjournal,ReadDialogText) ;

  loader.Free ;
end;

procedure TLogic.SendExit();
begin
  isfinished:=True ;
end;

procedure TLogic.setHeroXYZ(x,y,z: Single);
begin
  herox:=x ;
  heroy:=y ;
  heroz:=z ;
end;

procedure TLogic.setMusic(filename: string);
begin
  BeginWork() ;
  musicfile:=filename ;
  EndWork() ;
end;

destructor TLogic.Destroy;
begin
  inherited Destroy;
end;

procedure TLogic.addActiveObject(code, filename, icofile, caption: string;
  x, y: Integer; callbackinfo, callbackspell: string; way_idx: Integer);
var ao:TGameObject ;
begin
  ao:=TGameObject.Create() ;
  ao.code:=code ;
  ao.filename:=getActiveScenePath()+filename ;
  ao.icofile:=getActiveScenePath()+icofile ;
  ao.caption:=caption ;
  ao.x:=x ;
  ao.y:=y ;
  ao.z:=999 ;
  ao.isactive:=True ;
  ao.callbackinfo:=callbackinfo ;
  ao.callbackspell:=callbackspell ;
  ao.way_idx:=way_idx ;
  ao.transp:=0 ;
  BeginWork() ;
  activeobjects.Add(ao) ;
  EndWork() ;
end;

procedure TLogic.addDialogOnTopIfNew(text: string; color: Cardinal);
var rec:TDialogText ;
begin
  rec:=TDialogText.Create(text,color) ;
  if dialogjournal.Count=0 then
    dialogjournal.Add(rec)
  else
    if not(dialogjournal[dialogjournal.Count-1]=rec) then
      dialogjournal.Add(rec) ;
end;

procedure TLogic.addPassiveObject(code, filename: string; x, y: Integer; z:Single);
var ao:TGameObject ;
begin
  ao:=TGameObject.Create() ;
  ao.code:=code ;
  ao.filename:=getActiveScenePath()+filename ;
  ao.icofile:='' ;
  ao.caption:='' ;
  ao.x:=x ;
  ao.y:=y ;
  ao.z:=z ;
  ao.isactive:=False ;
  ao.callbackinfo:='' ;
  ao.callbackspell:='' ;
  ao.way_idx:=0 ;
  ao.transp:=0 ;
  BeginWork() ;
  activeobjects.Add(ao) ;
  EndWork() ;
end;

procedure TLogic.sendSkipClick(skipclick: TSkipClick);
begin
  if picturemode then picturemode:=False else breakwait:=True;
end;

procedure TLogic.setActiveObjectX(code: string; value: Integer);
begin
  BeginWork() ;
  if isGameObjectExist(code) then findGameObject(code).x:=value ;
  EndWork() ;
end;

procedure TLogic.setActiveObjectY(code: string; value: Integer);
begin
  BeginWork() ;
  if isGameObjectExist(code) then findGameObject(code).y:=value ;
  EndWork() ;
end;

procedure TLogic.setAllowedMarkerCount(value: Integer);
begin
  BeginWork() ;
  allowedmarkercount:=value ;
  EndWork() ;
end;

procedure TLogic.setActiveObjectFileName(code: string; value: string);
begin
  BeginWork() ;
  if isGameObjectExist(code) then findGameObject(code).filename:=getActiveScenePath()+value ;
  EndWork() ;
end;

procedure TLogic.setActiveObjectIcoFile(code: string; value: string);
begin
  BeginWork() ;
  if isGameObjectExist(code) then findGameObject(code).icofile:=getActiveScenePath()+value ;
  EndWork() ;
end;

procedure TLogic.setActiveObjectTransp(code: string; value: Integer);
begin
  BeginWork() ;
  if isGameObjectExist(code) then findGameObject(code).transp:=value ;
  EndWork() ;
end;

procedure TLogic.setActiveObjectCaption(code: string; value: string);
begin
  BeginWork() ;
  if isGameObjectExist(code) then findGameObject(code).caption:=value ;
  EndWork() ;
end;

procedure TLogic.setBackground(filename: string);
begin
  BeginWork() ;
  background:=getActiveScenePath()+filename ;
  EndWork() ;
end;

procedure TLogic.setFlag(flagname: string);
begin
  if not isFlagSet(flagname) then flags.Add(LowerCase(flagname)) ;
end;

function TLogic.isFlagSet(flagname: string): Boolean;
begin
  Result:=flags.IndexOf(LowerCase(flagname))<>-1 ;
end;

function TLogic.isGameObjectExist(obj: TGameObject): Boolean;
begin
  Result:=activeobjects.IndexOf(obj)<>-1 ;
end;

function TLogic.isGameObjectExist(code:string): Boolean;
begin
  Result:=findGameObject(code)<>nil ;
end;

function TLogic.isInScript: Boolean;
begin
  Result:=inscript ;
end;

function TLogic.isLinkExist(way1, way2: Integer): Boolean;
var wl:TWayLink ;
begin
  for wl in links do begin
    if (wl.idx1=way1)and(wl.idx2=way2) then Exit(True) ;
    if (wl.idx2=way1)and(wl.idx1=way2) then Exit(True) ;
  end;
  Result:=False ;
end;

function TLogic.isLookLeft: Boolean;
begin
  if lookState=lsLeft then begin
    lookState:=lsNone ;
    Result:=True ;
  end
  else
    Result:=False ;
end;

function TLogic.isLookRight: Boolean;
begin
  if lookState=lsRight then begin
    lookState:=lsNone ;
    Result:=True ;
  end
  else
    Result:=False ;
end;

function TLogic.findGameObject(code:string): TGameObject;
var i:Integer ;
begin
  Result:=nil ;
  for i:= 0 to activeobjects.Count-1 do
     if activeobjects[i].code=code then
       Result:=activeobjects[i] ;
end;

function TLogic.isNewTarget(var way_idx: Integer): Boolean;
begin
  if newway_idx=-1 then Exit(False) ;
  way_idx:=newway_idx ;
  newway_idx:=-1 ;
  Result:=True ;
end;

function TLogic.isPictureMode: Boolean;
begin
  Result:=picturemode ;
end;

class function TLogic.isSaveGameExist: Boolean;
begin
  Result:=FileExists(getSaveGameFileName()) ;
end;

function TLogic.isWayOut(idx: Integer): Boolean;
begin
  Result:=way[idx].isout ;
end;

procedure TLogic.lookPlayerLeft;
begin
  BeginWork() ;
  lookState:=lsLeft ;
  EndWork() ;
end;

procedure TLogic.lookPlayerRight;
begin
  BeginWork() ;
  lookState:=lsRight ;
  EndWork() ;
end;

procedure TLogic.clearFlag(flagname: string);
begin
  if isFlagSet(flagname) then flags.Delete(flags.IndexOf(LowerCase(flagname))) ;
end;

class procedure TLogic.clearSaveGame;
begin
  if isSaveGameExist() then DeleteFile(getSaveGameFileName()) ;
end;

procedure TLogic.setPlayerWayIdx(idx: Integer);
begin
  BeginWork() ;
  herox:=way[idx].x ;
  heroy:=way[idx].y ;
  heroz:=way[idx].z ;
  EndWork() ;
end;

procedure TLogic.setSpell(spellcode: Integer; spellseq: array of Integer;
  icoscene,icofile:string);
var spell:TSpell ;
    i:Integer ;
begin
  spell.len:=Length(spellseq) ;
  spell.activated:=False ;
  spell.iconfile:=formatSpellIcon(icoscene,icofile) ;
  for i := 0 to spell.len-1 do
    spell.seq[i]:=spellseq[i] ;
  spells.Add(spellcode,spell);
end;

procedure TLogic.setTextColored(text: string; color: Cardinal);
begin
  BeginWork() ;
  dialogtext:=text ;
  dialogcolor:=color ;
  EndWork() ;
end;

procedure TLogic.setText(text: string);
begin
  setTextColored(text,defaultcolor) ;
end;

procedure TLogic.goPicture(picture: string; text:string);
var oldback:string ;
begin
  oldback:=background ;
  dialogtext:=text ;
  dialogcolor:=$FFFFFF ;
  setBackground(picture) ;
  picturemode:=True ;
  while True do begin
    BeginWork() ;
    try
    if not picturemode then Break ;
    finally
      EndWork() ;
    end;
    Sleep(10) ;
  end ;
  dialogtext:='' ;
  background:=oldback
end;

procedure TLogic.goScene(newactivescene: string; initx,inity:Integer; initz:Single; way_idx:Integer);
begin
  activescene:=newactivescene ;

  if world_activeobjects.ContainsKey(activescene) then begin
    RestoreSceneFromWorld() ;
  end
  else begin
    activeobjects:=TUniList<TGameObject>.Create() ;
    way:=TUniList<TWayPoint>.Create() ;
    links:=TUniList<TWayLink>.Create() ;
    with TScriptExecutor.Create(Self) do begin
      execSceneProc(activescene,'initScene()') ;
      Free ;
    end;
    world_activeobjects.Add(activescene,activeobjects);
    world_way.Add(activescene,way);
    world_links.Add(activescene,links);
    world_backgrounds.Add(activescene,background) ;
  end ;

  with TScriptExecutor.Create(Self) do begin
    execSceneProc(activescene,'enterScene()') ;
    Free ;
  end;

  if way_idx<>-1 then begin
    herox:=initx ;
    heroy:=inity ;
    heroz:=initz ;
    newway_idx:=way_idx ;
  end
  else
    newway_idx:=-1 ;

end;

procedure TLogic.wait(duration: Integer);
var i:Integer ;
begin
  if duration<=500 then
    Sleep(duration)
  else begin
  BeginWork() ;
  breakwait:=False ;
  EndWork() ;
  for i := 0 to duration div 55-1 do begin
    Sleep(55) ;
    try
    BeginWork() ;
    if breakwait then Break ;
    finally
      EndWork() ;
    end;
  end;
  end;
end;

function TLogic.WriteDialogText(rec: Pointer; store: TWriterAPI): Boolean;
begin
  store.WriteString('text',TDialogText(rec^).text) ;
  store.WriteInteger('color',TDialogText(rec^).color) ;
end;

function TLogic.WriteGameObject(obj: TObject; store: TWriterAPI): Boolean;
begin
  store.WriteString('code',TGameObject(obj).code) ;
  store.WriteString('filename',TGameObject(obj).filename) ;
  store.WriteString('icofile',TGameObject(obj).icofile) ;
  store.WriteString('caption',TGameObject(obj).caption) ;
  store.WriteInteger('x',TGameObject(obj).x) ;
  store.WriteInteger('y',TGameObject(obj).y) ;
  store.WriteSingle('z',TGameObject(obj).z) ;
  store.WriteBoolean('isactive',TGameObject(obj).isactive) ;
  store.WriteString('callbackinfo',TGameObject(obj).callbackinfo) ;
  store.WriteString('callbackspell',TGameObject(obj).callbackspell) ;
  store.WriteInteger('way_idx',TGameObject(obj).way_idx) ;
  store.WriteInteger('transp',TGameObject(obj).transp) ;
end;

procedure TLogic.writeLog(value: string);
begin
  logger.WriteLog('Msg from script: '+value) ;
end;

function TLogic.WriteLogic(obj: TObject; store: TWriterAPI): Boolean;
begin
  store.WriteString('background',background) ;
  store.WriteString('musicfile',musicfile) ;
  store.WriteString('activescene',activescene) ;
  store.WriteSingle('herox',herox) ;
  store.WriteSingle('heroy',heroy) ;
  store.WriteSingle('heroz',heroz) ;
  store.WriteInteger('allowedmarkercount',allowedmarkercount) ;
  Result:=True ;
end;

function TLogic.WriteSpell(rec: Pointer; store: TWriterAPI): Boolean;
var spell:TSpell ;
    i:Integer ;
begin
  spell:=TSpell(rec^) ;
  store.WriteInteger('len',spell.len) ;
  for i := 0 to spell.len-1 do
    store.WriteInteger(Format('seq_%d',[i]),spell.seq[i]) ;
  store.WriteBoolean('activated',spell.activated) ;
  store.WriteString('iconfile',spell.iconfile) ;
end;

function TLogic.WriteWayLink(rec: Pointer; store: TWriterAPI): Boolean;
begin
  store.WriteInteger('idx1',TWayLink(rec^).idx1) ;
  store.WriteInteger('idx2',TWayLink(rec^).idx2) ;
end;

function TLogic.WriteWayPoint(rec: Pointer; store: TWriterAPI): Boolean;
var wp:TWayPoint ;
begin
  wp:=TWayPoint(rec^) ;
  store.WriteInteger('idx',wp.idx) ;
  store.WriteInteger('x',wp.x) ;
  store.WriteInteger('y',wp.y) ;
  store.WriteSingle('z',wp.z) ;
  store.WriteBoolean('isout',wp.isout) ;
end;

procedure TLogic.addToSpellStackIfNeed(spellstack: TUniList<Integer>);
var i:Integer ;
begin
  if ActiveSpell.len>0 then begin
    if IsActiveSpellReverse then
      for i := ActiveSpell.len-1 downto 0 do
        spellstack.Add(ActiveSpell.seq[i])
    else
      for i := 0 to ActiveSpell.len-1 do
        spellstack.Add(ActiveSpell.seq[i]) ;
    ActiveSpell.len:=0 ;
  end ;
end;

procedure TLogic.addWayLink(Alinks: array of Integer);
var i:Integer ;
begin
  BeginWork() ;
  for i:=0 to Length(Alinks)-2 do
    if not isLinkExist(Alinks[i],Alinks[i+1]) then
      links.Add(TWayLink.Create(Alinks[i],Alinks[i+1])) ;
  EndWork() ;
end;

procedure TLogic.addWayPoint(idx:Integer; x:Integer; y: Integer; z: Single;
  isout:Boolean);
begin
  BeginWork() ;
  way.Add(TWayPoint.Create(idx,x,y,z,isout)) ;
  EndWork() ;
end;

procedure TLogic.clearText;
begin
  BeginWork() ;
  dialogtext:='' ;
  EndWork() ;
end;

end.

