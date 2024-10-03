unit viewstatic;

interface
uses SfmlGraphics,
  CommonClasses, helpers, Scene ;

type
  TViewStatic = class(TScene)
  private
    Font:TSfmlFont ;
    Text:TSfmlText ;
    Tasks:TUniList<TStaticTask> ;
    tektaskidx:Integer ;
    options:TOptions ;
  public
    constructor Create(Aoptions:TOptions) ;
    procedure AddTask(text:string; size:Integer) ;
    function Init():Boolean ; override ;
    procedure UnInit() ; override ;
    procedure RenderFunc() ; override ;
    function FrameFunc(dt:Single; events:TUniList<TSfmlEventEx>):TSceneResult ; override ;
  end;

implementation
uses SfmlWindow, SfmlSystem, SfmlUtils, View, Logic ;

{ TViewStatic }

procedure TViewStatic.AddTask(text: string; size: Integer);
begin
  Tasks.Add(TStaticTask.Create(text,size)) ;
end;

constructor TViewStatic.Create(Aoptions:TOptions);
begin
  options:=Aoptions ;
  Tasks:=TUniList<TStaticTask>.Create() ;
  tektaskidx:=0 ;
end;

function TViewStatic.Init():Boolean;
begin
  Font:=TSfmlFont.Create('fonts'+PATH_SEP+'arial.ttf');
  Text:=createText(font,'',18,SfmlWhite) ;
end;

procedure TViewStatic.UnInit() ;
begin
  Font.Free ;
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
        lobj:=TLogic.Create();
        nextscene:=TView.Create(lobj,options) ;
        Exit(TSceneResult.Switch) ;
      end
      else
        Inc(tektaskidx) ;
    end;
end;

procedure TViewStatic.RenderFunc;
begin
  text.UnicodeString:=UTF8Decode(Tasks[tektaskidx].Text) ;
  text.CharacterSize:=Tasks[tektaskidx].textsize ;
  text.Position:=SfmlVector2f((WINDOW_W-text.LocalBounds.Width)/2,
     (WINDOW_H-text.LocalBounds.Height)/2) ;
  Window.Draw(text) ;
end;

end.
