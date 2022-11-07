unit viewstatic;

interface
uses SfmlGraphics,
  CommonClasses, helpers ;

type
  TViewStatic = class
  private
    Window:TSfmlRenderWindow ;
    Font:TSfmlFont ;
    Text:TSfmlText ;
    Tasks:TUniList<TStaticTask> ;
    tektaskidx:Integer ;
    function FrameFunc(dt:Single):Boolean ;
    procedure RenderFunc() ;
  public
    constructor Create(Awindow:TSfmlRenderWindow) ;
    procedure AddTask(text:string; size:Integer) ;
    procedure Run() ;
    destructor Destroy() ; override ;
  end;

implementation
uses SfmlWindow, SfmlSystem ;

{ TViewStatic }

procedure TViewStatic.AddTask(text: string; size: Integer);
begin
  Tasks.Add(TStaticTask.Create(text,size)) ;
end;

constructor TViewStatic.Create(Awindow: TSfmlRenderWindow);
begin
  Window:=Awindow ;
  Font:=TSfmlFont.Create('fonts'+PATH_SEP+'arial.ttf');
  Text := TSfmlText.Create;
  Text.&String := '';
  Text.Font := Font.Handle;
  Text.CharacterSize := 18;
  Text.Color := SfmlWhite;

  Tasks:=TUniList<TStaticTask>.Create() ;
  tektaskidx:=0 ;
end;

destructor TViewStatic.Destroy;
begin
  Font.Free ;
  inherited Destroy ;
end;

function TViewStatic.FrameFunc(dt: Single):Boolean;
var Event:TSfmlEvent ;
    next:Boolean ;
begin
    Result:=True ;
    next:=False ;
    while Window.PollEvent(Event) do
    begin
      if Event.EventType = sfEvtClosed then Window.Close;
      if (Event.EventType = sfEvtKeyPressed) then begin
        if (event.key.code in [sfKeyEscape,sfKeySpace,sfKeyReturn]) then next:=True ;
      end;
      if (Event.EventType = sfEvtMouseButtonPressed) then
        if (event.MouseButton.Button = sfMouseLeft) then next:=True ;
    end ;
    if next then begin
      if tektaskidx+1>=Tasks.Count then Result:=False else Inc(tektaskidx) ;
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

procedure TViewStatic.Run;
begin
  while Window.IsOpen do
  begin
    if not FrameFunc(1.0) then Break ;
    Window.Clear(SfmlBlack);
    RenderFunc() ;
    Window.Display;
  end;
end;

end.
