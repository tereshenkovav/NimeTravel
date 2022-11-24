unit Logic;

interface

uses
  Classes, SysUtils, syncobjs,
  gameobject, helpers, commonclasses ;

type

  { TLogic }

  TLogic = class(TThread)
  protected
    procedure Execute; override;
  private
    activeobjects:TUniList<TGameObject> ;
    way:TUniList<TWayPoint> ;
    links:TUniList<TWayLink> ;
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
    procedure writeInfo() ;
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
    procedure setHeroZ(z:Single) ;
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
    function isInScript():Boolean ;
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
    procedure genSpell(spellcode:Integer; spelllen:Integer; minincpos:Integer; symmetric:Boolean) ;
    procedure setSpell(spellcode:Integer; spellseq:array of Integer) ;
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
    constructor Create() ;
    destructor Destroy ; override ;
  end;

implementation
uses Math, StrUtils,
  commonproc, ScriptExecutor, waysearch ;

{ TLogic }

function TLogic.buildWayStack(x, y: Integer): TUniList<Integer>;

begin
  with TWaySearch.Create(links,way) do begin
    Result:=buildWayStack(getHeroX(),getHeroY(),x,y) ;
    Free ;
  end;
end;

constructor TLogic.Create();
begin
  inherited Create(True);

  world_activeobjects:=TUniDictionary<string,TObject>.Create() ;
  world_way:=TUniDictionary<string,TObject>.Create() ;
  world_links:=TUniDictionary<string,TObject>.Create() ;
  world_backgrounds:=TUniDictionary<string,string>.Create() ;

  flags:=TStringList.Create() ;
  spells:=TUniDictionary<Integer,TSpell>.Create() ;

  isfinished:=False ;
  sect:=TCriticalSection.Create;
  herox:=0 ;
  heroy:=0 ;
  scriptname:='' ;
  inscript:=False ;
  background:='' ;
  musicfile:='' ;
  with TScriptExecutor.Create(Self) do begin
    execScriptProc('gen_spells.script','genSpells()') ;
    Free ;
  end;

  defaultcolor:=$ffdef7 ;
  ActiveSpell.len:=0 ;
  goScene('scene4',0,0,0.0,-1) ;

  //writeInfo() ;
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
  symmetric: Boolean);
var spell:TSpell ;
begin
  spell.GenByParam(spelllen,minincpos,symmetric);
  spells.Add(spellcode,spell);
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

function TLogic.getDialogColor: Cardinal;
begin
  Result:=dialogcolor ;
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

procedure TLogic.EndWork();
begin
  sect.Leave ;
end;

procedure TLogic.SendExit();
begin
  isfinished:=True ;
end;

procedure TLogic.setHeroZ(z: Single);
begin
  heroz:=z ;
end;

procedure TLogic.setMusic(filename: string);
begin
  BeginWork() ;
  musicfile:=filename ;
  EndWork() ;
end;

procedure TLogic.writeInfo;
var wp:TWayPoint ;
    wl:TWayLink ;
    code:Integer ;
begin
  for wp in way do
    Writeln(wp.idx,' ',wp.x,' ',wp.y,' ',wp.z) ;
  for wl in links do
    Writeln(wl.idx1,' ',wl.idx2) ;
  Writeln('Spells') ;
  for code in spells.AllKeys do
    Writeln(code,' ',spells[code].ToString()) ;
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

procedure TLogic.setPlayerWayIdx(idx: Integer);
begin
  BeginWork() ;
  herox:=way[idx].x ;
  heroy:=way[idx].y ;
  heroz:=way[idx].z ;
  EndWork() ;
end;

procedure TLogic.setSpell(spellcode: Integer; spellseq: array of Integer);
var spell:TSpell ;
    i:Integer ;
begin
  spell.len:=Length(spellseq) ;
  spell.activated:=False ;
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
    activeobjects:=TUniList<TGameObject>(world_activeobjects[activescene]) ;
    way:=TUniList<TWayPoint>(world_way[activescene]) ;
    links:=TUniList<TWayLink>(world_links[activescene]) ;
    background:=world_backgrounds[activescene] ;
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

procedure TLogic.writeLog(value: string);
begin
  Writeln('Log: ',value) ;
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

