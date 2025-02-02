unit viewstatic;

interface
uses SfmlGraphics,
  CommonClasses, helpers, Scene ;

type
  TViewStatic = class(TScene)
  private
    Text:TSfmlText ;
    Tasks:TUniList<TStaticTask> ;
    tektaskidx:Integer ;
  public
    constructor Create() ;
    procedure AddTask(text:string; size:Integer) ;
    function Init():Boolean ; override ;
    procedure UnInit() ; override ;
    procedure RenderFunc() ; override ;
    function FrameFunc(dt:Single; events:TUniList<TSfmlEventEx>):TSceneResult ; override ;
  end;

implementation
uses SfmlWindow, SfmlSystem, SfmlUtils, View, Logic, CommonData ;

{ TViewStatic }

procedure TViewStatic.AddTask(text: string; size: Integer);
begin
  Tasks.Add(TStaticTask.Create(text,size)) ;
end;

constructor TViewStatic.Create();
begin
  Tasks:=TUniList<TStaticTask>.Create() ;
  tektaskidx:=0 ;
end;

function TViewStatic.Init():Boolean;
begin
  Text:=createText(TCommonData.font,'',18,SfmlWhite) ;
end;

procedure TViewStatic.UnInit() ;
begin
  Text.Free ;
end;

function TViewStatic.FrameFunc(dt:Single; events:TUniList<TSfmlEventEx>):TSceneResult ;
var Event:TSfmlEventEx ;
    next:Boolean ;
    lobj:TLogic ;
begin
  Result:=TSceneResult.Normal ;

    next:=False ;
    for Event in events do begin
      if (Event.Event.EventType = sfEvtKeyPressed) then begin
        if (event.Event.key.code in [sfKeyEscape,sfKeySpace,sfKeyReturn]) then next:=True ;
      end;
      if (Event.Event.EventType = sfEvtMouseButtonPressed) then
        if (event.Event.MouseButton.Button = sfMouseLeft) then next:=True ;
    end ;
    if next then begin
      if tektaskidx+1>=Tasks.Count then begin
        lobj:=TLogic.Create(logger);
        nextscene:=TView.Create(lobj) ;
        Exit(TSceneResult.Switch) ;
      end
      else
        Inc(tektaskidx) ;
    end;
end;

procedure TViewStatic.RenderFunc;
begin
  text.CharacterSize:=Tasks[tektaskidx].textsize ;
  drawTextInBlockWidth(text,Tasks[tektaskidx].Text,50,100,wwidth-100,3) ;
end;

end.
