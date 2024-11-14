unit View;

interface

uses
  Classes, SysUtils,
  SfmlGraphics,SfmlSystem,SfmlWindow,SfmlAudio, Scene,
  Logic,GameObject,Helpers, WayRenderer, SfmlAnimation,
  ListZOrders, MainMenu, CommonClasses ;

type

  { TView }

  TView = class(TScene)
  private
    // sfml objects
    HeroWait: TSfmlSprite;
    HeroAction: TSfmlSprite;
    HeroWalk: TSfmlAnimation;
    Horn:TSfmlSprite ;
    Zones:TUniList<TSfmlSprite> ;
    TextInfo:TSfmlText ;
    TextDialog:TSfmlText ;
    Marker:TSfmlAnimation ;
    MagicAura:TSfmlAnimation ;
    Galop:TSfmlSound ;
    Magic:TUniList<TSfmlSound> ;
    clock:TSfmlClock ;
    Cursor:TSfmlSprite ;
    CursorQuest:TSfmlSprite ;
    CursorWalk:TSfmlSprite ;
    DebugAllowed:Boolean ;
    gamemenu:TMainMenu ;
    lobj:TLogic ;
    Texts:TStringList ;
    overobject:TGameObject ;
    selectedobject:TGameObject ;
    targetobject:TGameObject ;
    sprites:TUniDictionary<string,TSfmlSprite> ;
    listzsprites:TListZOrders ;
    wayrenderer:TWayRenderer ;
    vx:Single ;
    vy:Single ;
    vz:Single ;
    targetx:Integer ;
    targety:Integer ;
    targetz:Single ;
    targetidx:Integer ;
    isgo:Boolean ;
    ismirr:Boolean ;
    waystack:TUniList<Integer> ;
    markerpos:TUniList<TSfmlVector2f> ;
    spellstack:TUniList<Integer> ;
    playedspellstack:TUniList<Integer> ;
    lastspellclicktime:Single ;
    flag_entered_menu:Boolean ;

    function loadSpriteOrAnimation(filename:string):TSfmlSprite ;
    function getSprite(prefix:string; ao:TGameObject):TSfmlSprite ;
    function getSpriteStatic(prefix:string; filename:string):TSfmlSprite ;
    function retAndPosSprite(spr:TSfmlSprite; x,y:Integer; transp:Integer):TSfmlSprite ;
    function isReachTarget():Boolean ;
    function isInDebugView():Boolean ;
    procedure trySetTarget(x,y:Integer) ; overload ;
    procedure trySetTarget(wayidx:Integer) ; overload ;
    procedure goTargetIdx(idx:Integer) ;
    procedure drawObjIcoAndText(obj:TGameObject);
    procedure drawObjIco(obj:TGameObject);
    function getTextOrKey(textname:string):string ;
    procedure tryStartMarker(i:Integer) ;
    function getZScale():Single ;
    procedure setUpMusicAndSoundVolumes() ;
    procedure loadTexts() ;
    function getScaleVector():TSfmlVector2f;
  public
    constructor Create(obj:TLogic) ;
    function Init():Boolean ; override ;
    procedure UnInit() ; override ;
    procedure RenderFunc() ; override ;
    function FrameFunc(dt:Single; events:TUniList<TSfmlEventEx>):TSceneResult ; override ;
    procedure FocusChanged(isfocus:Boolean) ; override ;
  end;

implementation
uses math,
  sfmlutils, commonproc, CommonData ;

const
  PREFIX_ACTIVEOBJECT='activeobject_' ;
  PREFIX_ACTIVEOBJECTICO='activeobjectico_' ;
  PREFIX_BACKGROUND='background_' ;

  MINDIST2 = 5*5 ;
  MARKERRADIUS2 = 8*8 ;
  SECS_BEFORE_DROPSPELL = 3 ;

{ TView }

constructor TView.Create(obj:TLogic);
begin
  lobj:=obj ;
end ;

function TView.Init():Boolean ;
var i:Integer ;
begin
  sprites:=TUniDictionary<string,TSfmlSprite>.Create() ;
  listzsprites:=TListZOrders.Create ;

  Cursor:=loadSprite('images'+PATH_SEP+'cursor.png');
  Cursor.Origin:=SfmlVector2f(0,10) ;
  CursorQuest:=loadSprite('images'+PATH_SEP+'cursor_quest.png');
  CursorQuest.Origin:=SfmlVector2f(0,10) ;
  CursorWalk:=loadSprite('images'+PATH_SEP+'cursor_walk.png');
  CursorWalk.Origin:=SfmlVector2f(0,10) ;

  HeroWait:=loadSprite('images'+PATH_SEP+'herowait.png');
  HeroWait.Origin := SfmlVector2f(SfmlTextureGetSize(HeroWait.Texture).x / 2, 123/0.400);

  HeroAction:=loadSprite('images'+PATH_SEP+'heroaction.png');
  HeroAction.Origin:=HeroWait.Origin ;

  HeroWalk:=TSfmlAnimation.Create('images',
  ['01.png','02.png','03.png','04.png','05.png','06.png','07.png','08.png','09.png','10.png','11.png','12.png'],
  12);
  HeroWalk.Origin:=HeroWait.Origin ;
  HeroWalk.Play() ;

  Horn:=loadSprite('images'+PATH_SEP+'horn.png');
  Horn.Position:=SfmlVector2f(0,500) ;
  Zones:=TUniList<TSfmlSprite>.Create() ;
  Zones.Add(LoadSprite('images'+PATH_SEP+'zone.png',[sloCentered]));
  Zones.Add(LoadSprite('images'+PATH_SEP+'zone1.png',[sloCentered]));
  Zones.Add(LoadSprite('images'+PATH_SEP+'zone2.png',[sloCentered]));
  for i := 3 to 10 do
    Zones.Add(LoadSprite('images'+PATH_SEP+'zone.png',[sloCentered]));

  Galop:=TSfmlSound.Create(TSfmlSoundBuffer.Create('sounds'+PATH_SEP+'galop.ogg'));
  Magic:=TUniList<TSfmlSound>.Create() ;
  Magic.Add(TSfmlSound.Create(TSfmlSoundBuffer.Create('sounds'+PATH_SEP+'magic1.ogg')));
  Magic.Add(TSfmlSound.Create(TSfmlSoundBuffer.Create('sounds'+PATH_SEP+'magic2.ogg')));
  Magic.Add(TSfmlSound.Create(TSfmlSoundBuffer.Create('sounds'+PATH_SEP+'magic3.ogg')));
  Magic.Add(TSfmlSound.Create(TSfmlSoundBuffer.Create('sounds'+PATH_SEP+'magic.ogg')));
  Magic.Add(TSfmlSound.Create(TSfmlSoundBuffer.Create('sounds'+PATH_SEP+'magic.ogg')));
  Magic.Add(TSfmlSound.Create(TSfmlSoundBuffer.Create('sounds'+PATH_SEP+'magic.ogg')));
  Magic.Add(TSfmlSound.Create(TSfmlSoundBuffer.Create('sounds'+PATH_SEP+'magic.ogg')));
  Magic.Add(TSfmlSound.Create(TSfmlSoundBuffer.Create('sounds'+PATH_SEP+'magic.ogg')));
  Magic.Add(TSfmlSound.Create(TSfmlSoundBuffer.Create('sounds'+PATH_SEP+'magic.ogg')));
  Magic.Add(TSfmlSound.Create(TSfmlSoundBuffer.Create('sounds'+PATH_SEP+'magic.ogg')));

  TextInfo:=createText(TCommonData.font,'',18,SfmlWhite) ;
  TextDialog:=createText(TCommonData.font,'',20,SfmlWhite) ;

  Marker:=TSfmlAnimation.Create('images'+PATH_SEP+'marker.png',30,34,16,20) ;
  Marker.Origin:=SfmlVector2f(15,17) ;

  MagicAura:=TSfmlAnimation.Create('images'+PATH_SEP+'magic.png',100,100,18,9) ;
  MagicAura.Origin:=SfmlVector2f(50,50) ;
  MagicAura.Play() ;

  wayrenderer:=TWayRenderer.Create(Window) ;

  waystack:=TUniList<Integer>.Create() ;

  markerpos:=TUniList<TSfmlVector2f>.Create() ;
  markerpos.Add(SfmlVector2f(13,566)) ;
  markerpos.Add(SfmlVector2f(25,543)) ;
  markerpos.Add(SfmlVector2f(34,580)) ;
  markerpos.Add(SfmlVector2f(65,557)) ;
  markerpos.Add(SfmlVector2f(82,535)) ;
  markerpos.Add(SfmlVector2f(118,500+38)) ;
  markerpos.Add(SfmlVector2f(136,500+47)) ;
  markerpos.Add(SfmlVector2f(138,500+22)) ;
  markerpos.Add(SfmlVector2f(167,500+30)) ;
  markerpos.Add(SfmlVector2f(192,500+20)) ;

  spellstack:=TUniList<Integer>.Create() ;
  playedspellstack:=TUniList<Integer>.Create() ;

  clock:=TSfmlClock.Create() ;

  Texts:=TStringList.Create ;
  loadTexts() ;

  setUpMusicAndSoundVolumes() ;

  flag_entered_menu:=False ;

  isgo:=False ;
  lobj.Start() ;

  DebugAllowed:=FileExists('developer') ;
end;

procedure TView.setUpMusicAndSoundVolumes;
var i:Integer ;
begin
  TCommonData.updateMusicVolume() ;
  Galop.Volume:=IfThen(profile.isSoundOn(),100,0) ;
  for i := 0 to Magic.Count-1 do
    Magic[i].Volume:=IfThen(profile.isSoundOn(),100,0) ;
end;

procedure TView.UnInit();
var key:string ;
begin
  HeroWait.Free ;
  HeroWalk.Free ;
  HeroAction.Free ;
  Horn.Free ;
  for key in sprites.AllKeys do
    sprites[key].Free ;

  Galop.Free ;
  TextInfo.Free ;
  TextDialog.Free ;

  sprites.Free ;
  wayrenderer.Free ;
  waystack.Free ;

  gamemenu.Free ;
  listzsprites.Free ;

  lobj.SendExit() ;
end;

procedure TView.trySetTarget(x,y:Integer) ;
begin
  waystack:=lobj.buildWayStack(x,y) ;
  if (waystack.Count>0) then begin
    goTargetIdx(waystack[0]) ;
    waystack.Delete(0);
  end;
end ;

procedure TView.trySetTarget(wayidx:Integer) ;
begin
  trySetTarget(lobj.getWayPoint(wayidx).x,lobj.getWayPoint(wayidx).y) ;
end ;

function TView.isInDebugView: Boolean;
begin
  Result:=DebugAllowed and SfmlKeyboardIsKeyPressed(sfKeyLControl) ;
end;

function TView.isReachTarget():Boolean ;
begin
  Result:=dist2(targetx,lobj.getHeroX(),targety,lobj.getHeroY())<MINDIST2 ;
end ;

function TView.loadSpriteOrAnimation(filename: string): TSfmlSprite;
var tmp:TArray<string> ;
begin
  tmp:=filename.Split(['@']) ;
  if Length(tmp)=1 then
    Result:=loadSprite(filename)
  else begin
    Result:=TSfmlAnimation.Create(tmp[0],StrToInt(tmp[1]),StrToInt(tmp[2])) ;
    TSfmlAnimation(Result).Play() ;
  end;
end;

procedure TView.loadTexts;
var i:Integer ;
    fname:string ;

  procedure addFileWithPrefix(filename:string; prefix:string) ;
  var j:Integer ;
  begin
     with TStringList.Create() do begin
        LoadFromFile(filename) ;
        for j:=0 to Count-1 do
          if Trim(Strings[j])<>'' then
            Texts.Add(prefix+'_'+Strings[j]) ;
        Free ;
     end ;
  end;

begin
  Texts.Clear() ;

  i:=1 ;
  while True do begin
    if not DirectoryExists(Format('scenes%sscene%d',[PATH_SEP,i])) then Break ;
    fname:=Format('scenes%sscene%d%sstrings.dat.%s',
      [PATH_SEP,i,PATH_SEP,TCommonData.languages.getCurrent()]) ;
    if FileExists(fname) then addFileWithPrefix(fname,'scene'+IntToStr(i)) ;
    Inc(i) ;
  end ;

  addFileWithPrefix(Format('text%sstrings.dat.%s',
     [PATH_SEP,TCommonData.languages.getCurrent()]),'global') ;
end;

procedure TView.tryStartMarker(i:Integer) ;
begin
  if i+1<=lobj.getAllowerMarkerCount() then begin
    spellstack.Add(i) ;
    playedspellstack.Add(i) ;
    lastspellclicktime:=clock.ElapsedTime.AsSeconds ;
  end;
end;

function TView.getScaleVector():TSfmlVector2f;
begin
  Result:=SfmlVector2f(IfThen(ismirr,-getZScale(),getZScale()),getZScale()) ;
end ;

procedure TView.FocusChanged(isfocus: Boolean);
begin
  if (isfocus) then begin
    if Galop.Status=sfPaused then Galop.Play ;
  end
  else
    Galop.Pause() ;
end;

function TView.FrameFunc(dt:Single; events:TUniList<TSfmlEventEx>):TSceneResult ;
var ao:TGameObject ;
    Event:TSfmlEventEx;
    kc:TSfmlKeyCode ;
    i:Integer ;
    spellcode:Integer ;
    reverse:Boolean ;
    way_id:Integer ;
    dtleft,ddt,speedupk:Single ;
begin
  Result:=TSceneResult.Normal ;

  lobj.BeginWork() ;
  try
  if isInDebugView() then
    dt:=dt*0.2;

  listzsprites.Clear() ;
  if lobj.getBackground()<>'' then
    listzsprites.Add(retAndPosSprite(getSpriteStatic(PREFIX_BACKGROUND,lobj.getBackground()),0,0,0),Single.MaxValue) ;

  if not lobj.isPictureMode() then begin
    for ao in lobj.getActiveObjects() do
      listzsprites.Add(retAndPosSprite(getSprite(PREFIX_ACTIVEOBJECT,ao),ao.x,ao.y,ao.transp),ao.z) ;
    HeroWalk.ScaleFactor:=getScaleVector() ;
    HeroAction.ScaleFactor:=getScaleVector() ;
    HeroWait.ScaleFactor:=getScaleVector() ;
    if isgo then
      listzsprites.Add(HeroWalk,lobj.getHeroZ())
    else
    if playedspellstack.Count>0 then
      listzsprites.Add(HeroAction,lobj.getHeroZ())
    else
      listzsprites.Add(HeroWait,lobj.getHeroZ());
  end;

  {if isInDebugView() then
    Window.SetTitle(Format('%d %d',[mx,my]));}

  if lobj.isPictureMode() then begin
    for Event in Events do begin
      if (Event.Event.EventType = sfEvtKeyPressed) then begin
        if (event.Event.key.code = sfKeyEscape) then lobj.sendSkipClick(scFull);
        if (event.Event.key.code = sfKeySpace) then lobj.sendSkipClick(scPartial);
        if (event.Event.key.code = sfKeyReturn) then lobj.sendSkipClick(scPartial);
      end;
      if (Event.Event.EventType = sfEvtMouseButtonPressed) then
        if (event.Event.MouseButton.Button = sfMouseLeft) then
          lobj.sendSkipClick(scPartial);
    end ;
    Exit ;
  end ;

  overobject:=nil ;
  for ao in lobj.getActiveObjects() do
    if ao.isactive then
    if (ao.x<mousex)and(ao.y<mousey)and
       (mousex<ao.x+getSprite(PREFIX_ACTIVEOBJECT,ao).LocalBounds.Width)and
       (mousey<ao.y+getSprite(PREFIX_ACTIVEOBJECT,ao).LocalBounds.Height) then
      overobject:=ao ;

  for Event in Events do begin
    if lobj.isInScript() then begin
      if (Event.Event.EventType = sfEvtKeyPressed) then begin
        if (event.Event.key.code = sfKeyEscape) then lobj.sendSkipClick(scFull);
        if (event.Event.key.code = sfKeySpace) then lobj.sendSkipClick(scPartial);
        if (event.Event.key.code = sfKeyReturn) then lobj.sendSkipClick(scPartial);
      end;
      if (Event.Event.EventType = sfEvtMouseButtonPressed) then
        if (event.Event.MouseButton.Button = sfMouseLeft) then
          lobj.sendSkipClick(scPartial);
    end
    else begin
    if (Event.Event.EventType = sfEvtKeyPressed) then begin
      if (event.Event.key.code = sfKeyF5)and DebugAllowed then lobj.executeScript('debug.script','runDebug1()') ;
      if (event.Event.key.code = sfKeyF6)and DebugAllowed then lobj.executeScript('debug.script','runDebug2()') ;
      if (event.Event.key.code = sfKeyF7)and DebugAllowed then lobj.executeScript('debug.script','runDebug3()') ;
      if (event.Event.key.code = sfKeyEscape)or(event.Event.key.code = sfKeyF10) then begin
        subscene:=TMainMenu.CreateAsGameMenu() ;
        Galop.Pause;
        flag_entered_menu:=True ;
        Exit(TSceneResult.SetSubScene) ;
      end;
      if selectedobject<>nil then begin
        kc:=sfKeyNum1 ;
        for i := 0 to markerpos.Count-1 do begin
          if event.Event.key.code = kc then tryStartMarker(i) ;
          Inc(kc) ;
        end;
      end;
    end;
    if (Event.Event.EventType = sfEvtMouseButtonPressed) then
      if (event.Event.MouseButton.Button = sfMouseLeft) then begin
        targetobject:=nil ;
        if (overobject<>nil) then begin
          trySetTarget(overobject.way_idx) ;
          targetobject:=overobject ;
        end
        else
          if (mousey<500) then trySetTarget(mousex,mousey) ;
        if (selectedobject<>nil) then begin
          if ((mousey>500)and(mousex>650))or(selectedobject=overobject) then lobj.executeInfoProc(selectedobject);
        end;
        if selectedobject<>nil then
          for i := 0 to markerpos.Count-1 do
            if dist2(markerpos[i].X,mousex,markerpos[i].Y,mousey)<MARKERRADIUS2 then tryStartMarker(i) ;
      end;
    end;
  end;

  if (spellstack.Count>0)and(not Marker.isPlayed()) then begin
    Marker.PlayOnce() ;
    Magic[spellstack[0]].Play() ;
    if playedspellstack.Count>0 then
      Marker.Position:=SfmlVector2f(
        HeroAction.Position.X+IfThen(ismirr,-1,1)*(421-446/2)*getZScale(),
        HeroAction.Position.Y+(85-123/0.400)*getZScale())
    else
      Marker.Position:=markerpos[spellstack[0]] ;
    Marker.ScaleFactor:=SfmlVector2f(2*getZScale(),2*getZScale()) ;
    Marker.Color:=TCommonData.markercolors[spellstack[0]] ;
    spellstack.Delete(0);
  end;

  if (playedspellstack.Count>0)and(not Marker.isPlayed()) then begin
    if lobj.findSpell(playedspellstack,spellcode,reverse) then begin
      playedspellstack.Clear() ;
      spellstack.Clear() ;
      lobj.executeSpellProc(selectedobject,spellcode,reverse);
    end;
  end;

  if (spellstack.Count=0)and(playedspellstack.Count>0)and(not Marker.isPlayed()) then
    if clock.ElapsedTime.AsSeconds-lastspellclicktime>SECS_BEFORE_DROPSPELL then
       playedspellstack.Clear() ;

  HeroWait.Position:=SfmlVector2f(lobj.getHeroX(),lobj.getHeroY()) ;
  HeroWalk.Position:=HeroWait.Position ;
  HeroAction.Position:=HeroWait.Position ;

  if (isgo) then begin
    speedupk:=IfThen(SfmlKeyboardIsKeyPressed(sfKeyLShift),3.0,1.0) ;
    dtleft:=dt ;
    ddt:=dt*0.01 ;
    // Уменьшение кванта времени
    while dtleft>0 do begin
      lobj.moveHeroXYZ(vx*speedupk*ddt,vy*speedupk*ddt,vz*speedupk*ddt) ;
      if isReachTarget() then begin
        lobj.setHeroZ(targetz) ;
        if waystack.Count>0 then begin
          goTargetIdx(waystack[0]) ;
          waystack.Delete(0);
        end
        else begin
          isgo:=False ;
          galop.Stop() ;
          lobj.executeWayProc(targetidx);
        end;
        break ;
      end;
      dtleft:=dtleft-ddt ;
    end;
    ismirr:=vx<0 ;
  end ;

  if (targetobject<>nil) then
    if dist2(lobj.getWayPoint(targetobject.way_idx).x,lobj.getHeroX(),
             lobj.getWayPoint(targetobject.way_idx).y,lobj.getHeroY())<MINDIST2 then begin
      ismirr:=targetobject.x+getSprite(PREFIX_ACTIVEOBJECT,targetobject).LocalBounds.Width/2<lobj.getHeroX ;
      selectedobject:=targetobject ;
      targetobject:=nil ;
    end ;
  if (selectedobject<>nil) then begin
    if dist2(lobj.getWayPoint(selectedobject.way_idx).x,lobj.getHeroX(),
             lobj.getWayPoint(selectedobject.way_idx).y,lobj.getHeroY())>MINDIST2 then begin
      selectedobject:=nil ;
      playedspellstack.Clear() ;
    end;
    if not lobj.isGameObjectExist(selectedobject) then selectedobject:=nil ;
  end;

  lobj.addToSpellStackIfNeed(spellstack) ;

  if lobj.isNewTarget(way_id) then trySetTarget(way_id) ;

  if lobj.isLookLeft() then ismirr:=True ;
  if lobj.isLookRight() then ismirr:=False ;

  HeroWalk.Update(dt);
  marker.Update(dt);
  MagicAura.Update(dt);

  for ao in lobj.getActiveObjects() do
    if getSprite(PREFIX_ACTIVEOBJECT,ao) is TSfmlAnimation then
      TSfmlAnimation(getSprite(PREFIX_ACTIVEOBJECT,ao)).Update(dt);

  // Возобновление после паузы при входе в меню
  if flag_entered_menu then begin
    setUpMusicAndSoundVolumes() ;
    if Galop.Status=sfPaused then Galop.Play ;
    flag_entered_menu:=False ;
  end;

  TCommonData.LoadMusicIfNew(lobj.getMusic()) ;

  finally
    lobj.EndWork() ;
  end;
end;

procedure TView.RenderFunc;
var spr:TSfmlSprite ;
    i:Integer ;
    tmpws:TUniList<Integer> ;
    usewalk:Boolean ;
begin
  listzsprites.Render(window) ;

  if lobj.getDialogText()<>'' then begin
    TextDialog.Color:=createSFMLColor(lobj.getDialogColor()) ;
    drawTextInBlockWidth(TextDialog,getTextOrKey(lobj.getDialogText()),
      240,505,450,2) ;
  end;

  if lobj.isPictureMode() then Exit ;

  if (selectedobject<>nil)or(marker.isPlayed()) then begin // Нужно ли marker.isPlayed()?
    Window.Draw(Horn);
    for i := 0 to lobj.getAllowerMarkerCount()-1 do
      drawSprite(zones[i],markerpos[i].X,markerpos[i].Y) ;
  end;

  if (marker.isPlayed()) then Window.Draw(marker);

  if playedspellstack.Count>0 then begin
    MagicAura.Position:=SfmlVector2f(
      selectedobject.x+getSprite(PREFIX_ACTIVEOBJECT,selectedobject).LocalBounds.Width/2,
      selectedobject.y+getSprite(PREFIX_ACTIVEOBJECT,selectedobject).LocalBounds.Height/2) ;
    MagicAura.ScaleFactor:=SfmlVector2f(
      getSprite(PREFIX_ACTIVEOBJECT,selectedobject).LocalBounds.Width/80,
      getSprite(PREFIX_ACTIVEOBJECT,selectedobject).LocalBounds.Height/80) ;
    Window.Draw(MagicAura);
  end;

  if (overobject<>nil)and(selectedobject<>nil) then begin
    if overobject=selectedobject then
      drawObjIcoAndText(selectedobject)
    else
      drawObjIco(overobject) ;
  end
  else
    if overobject<>nil then drawObjIco(overobject)
  else
    if selectedobject<>nil then drawObjIcoAndText(selectedobject) ;

  if not lobj.isInScript() then
  if subscene=nil then begin // Skip cursor for menu
  if (overobject<>nil)or((selectedobject<>nil)and(mousey>500)and(mousex>650)) then
    DrawSprite(CursorQuest,mousex,mousey)
  else begin

    usewalk:=False ;
    if mousey<500 then begin
      tmpws:=lobj.buildWayStack(mousex,mousey) ;
      if (tmpws.Count>0) then
        if lobj.isWayOut(tmpws[tmpws.Count-1]) then usewalk:=True ;
      tmpws.Free ;
    end;

    if usewalk then
      DrawSprite(CursorWalk,mousex,mousey)
    else begin
      Cursor.Color:=createSFMLColor($FFFFFF) ;
      // Дублирование условия показа рога
      if (selectedobject<>nil)or(marker.isPlayed()) then // Нужно ли marker.isPlayed()?
        for i := 0 to lobj.getAllowerMarkerCount()-1 do
          if (dist2(markerpos[i].X,mousex,markerpos[i].Y,mousey)<MARKERRADIUS2) then begin
            Cursor.Color:=TCommonData.markercolors[i] ;
            Break ;
          end ;
      DrawSprite(Cursor,mousex,mousey) ;
    end;
  end;

  end ;

  if isInDebugView() then wayrenderer.Render(lobj);
end;

function TView.getSprite(prefix: string; ao:TGameObject): TSfmlSprite;
begin
  if not sprites.ContainsKey(prefix+ao.code+ao.filename) then
    sprites.Add(prefix+ao.code+ao.filename,loadSpriteOrAnimation(ao.filename));
  Result:=sprites[prefix+ao.code+ao.filename] ;
end;

function TView.getSpriteStatic(prefix: string; filename:string): TSfmlSprite;
begin
  if not sprites.ContainsKey(prefix+filename) then
    sprites.Add(prefix+filename,loadSpriteOrAnimation(filename));
  Result:=sprites[prefix+filename] ;
  if prefix=PREFIX_ACTIVEOBJECTICO then
     Result.Origin:=SfmlVector2f(SfmlTextureGetSize(Result.Texture).X/2,0) ;
end;

function TView.getTextOrKey(textname:string): string;
var key:string ;
begin
  key:=lobj.getActiveScene()+'_'+textname.Substring(1) ;
  if Texts.IndexOfName(key)<>-1 then
    Result:=Texts.Values[key].Replace('\n',#10)
  else begin
    key:='global_'+textname.Substring(1) ;
    if Texts.IndexOfName(key)<>-1 then
      Result:=Texts.Values[key].Replace('\n',#10)
    else
      Result:='Key: '+textname ;
  end;
end;

function TView.getZScale: Single;
begin
  Result:=0.400*(1 - lobj.getHeroZ());
end;

procedure TView.goTargetIdx(idx: Integer);
var t,alldist:Single ;
const BASEV = 125 ;
begin
  targetx:=lobj.getWayPoint(idx).x ;
  targety:=lobj.getWayPoint(idx).y ;
  targetz:=lobj.getWayPoint(idx).z ;
  targetidx:=idx ;
  isgo:=True ;
  Galop.Play() ;
  alldist:=sqrt(dist2(targetx,lobj.getHeroX(),targety,lobj.getHeroY()));
  t:= alldist/BASEV ;
  if (t<=0) then t:=0.1 ;
  t:=t*(1+lobj.getWayPoint(idx).z)*(1+lobj.getWayPoint(idx).z) ;
  vx:=(targetx-lobj.getHeroX())/t ;
  vy:=(targety-lobj.getHeroY())/t ;
  vz:=(targetz-lobj.getHeroZ())/t ;
end;

function TView.retAndPosSprite(spr: TSfmlSprite; x, y: Integer; transp:Integer): TSfmlSprite;
begin
  spr.Position:=SfmlVector2f(x,y) ;
  spr.Color:=SfmlColorFromRGBA(252,255,255,Round(255*(100-transp)/100)) ;
  Result:=spr ;
end;

procedure TView.drawObjIco(obj:TGameObject);
begin
  drawSprite(getSpriteStatic(PREFIX_ACTIVEOBJECTICO,obj.icofile),720,510) ;
end ;

procedure TView.drawObjIcoAndText(obj:TGameObject);
begin
  drawObjIco(obj) ;
  TextInfo.UnicodeString:=UTF8Decode(getTextOrKey(obj.caption)) ;
  TextInfo.Position := SfmlVector2f(720-TextInfo.LocalBounds.Width/2,570);
  Window.Draw(TextInfo) ;
end ;

end.

