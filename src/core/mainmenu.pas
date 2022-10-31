unit mainmenu;

interface
uses Classes,
  SfmlGraphics, SfmlSystem,
  CommonClasses, helpers ;

type
  TMainMenu = class
  private
    window: TSfmlRenderWindow ;
    back: TSfmlSprite ;
    sublayer: TSfmlSprite ;
    button: TSfmlSprite ;
    font:TSfmlFont ;
    text:TSfmlText ;
    active:Boolean ;
    menu_texts:TStringList ;
    buttonw:Integer ;
    buttonh:Integer ;
    lastmx:Single ;
    lastmy:Single ;
    options:TOptions ;
    items:TStringList ;
    help_back:TSfmlSprite ;
    helpmode:Boolean ;
    first_item:string ;
    exit_code:string ;
    shifty:Integer ;
    function getButtonX(i:Integer):Integer ;
    function getButtonY(i:Integer):Integer ;
    function isMouseOver(i:Integer):Boolean ;
    procedure rebuildItems() ;
    procedure loadItemsText() ;
  public
    constructor Create(Awindow:TSfmlRenderWindow; sublayerfile:string;
      Aoptions:TOptions; Afirst_item:string; Ashifty:Integer=0) ;
    destructor Destroy ; override ;
    procedure Render() ;
    procedure Frame(dt:Single; mx,my:Single) ;
    procedure setActive(value:Boolean) ;
    function isActive():Boolean ;
    function getExitCode():string ;
  end;

implementation
uses sfmlutils,
  SfmlWindow ;

const
  LANG_ITEM = 'lang' ;

{ TMainMenu }

constructor TMainMenu.Create(Awindow: TSfmlRenderWindow; sublayerfile:string;
  Aoptions:TOptions; Afirst_item:string; Ashifty:Integer);
begin
  window:=Awindow ;
  options:=Aoptions ;
  first_item:=Afirst_item ;
  shifty:=Ashifty ;
  back:=LoadSprite('images'+PATH_SEP+'menu_back.png',[sloCentered]) ;
  back.Position:=SfmlVector2f(WINDOW_W/2,(WINDOW_H-100)/2+shifty) ;
  help_back:=LoadSprite('images'+PATH_SEP+'help_back.png') ;
  help_back.Position:=SfmlVector2f(0,0) ;
  sublayer:=LoadSprite(sublayerfile) ;
  sublayer.Position:=SfmlVector2f(0,0) ;
  button:=LoadSprite('images'+PATH_SEP+'button.png') ;
  font:=TSfmlFont.Create('fonts'+PATH_SEP+'arial.ttf');
  text:=TSfmlText.Create;
  text.&String:='';
  text.Font:=Font.Handle;
  text.CharacterSize:=24;
  menu_texts:=TStringList.Create ;
  loadItemsText() ;
  buttonw:=SfmlTextureGetSize(button.Texture).X ;
  buttonh:=SfmlTextureGetSize(button.Texture).Y ;
  items:=TStringList.Create ;
  helpmode:=False ;
  exit_code:='' ;
end;

destructor TMainMenu.Destroy;
begin
  back.Free ;
  sublayer.Free ;
  button.Free ;
  font.Free ;
  menu_texts.Free ;
  inherited Destroy ;
end;

procedure TMainMenu.Frame(dt, mx, my: Single);
var Event:TSfmlEvent;
    i:Integer ;
begin
  if helpmode then begin
    while Window.PollEvent(Event) do
      begin
        if Event.EventType = sfEvtClosed then Window.Close;
        if (Event.EventType = sfEvtKeyPressed) then helpmode:=False ;
        if (Event.EventType = sfEvtMouseButtonPressed) then helpmode:=False ;
      end;
    Exit ;
  end;

  exit_code:='' ;
  lastmx:=mx ;
  lastmy:=my ;
  while Window.PollEvent(Event) do
    begin
      if Event.EventType = sfEvtClosed then Window.Close;
      if (Event.EventType = sfEvtKeyPressed) then begin
        if (event.key.code = sfKeyEscape) then setActive(False) ;
      end;
      if (Event.EventType = sfEvtMouseButtonPressed) then
        if (event.MouseButton.Button = sfMouseLeft) then begin
          for i :=0 to items.Count do
            if isMouseOver(i) then begin
              if (items[i]='continue')or(items[i]='newgame') then begin
                exit_code:=items[i] ;
                setActive(False) ;
              end;
              if (items[i]='sound_on')or(items[i]='sound_off') then begin
                options.switchSound() ;
                rebuildItems() ;
                break ;
              end;
              if (items[i]='music_on')or(items[i]='music_off') then begin
                options.switchMusic() ;
                rebuildItems() ;
                break ;
              end;
              if items[i]=LANG_ITEM then begin
                options.switchLang() ;
                loadItemsText() ;
                break ;
              end ;
              if items[i]='help' then helpmode:=True ;
              
              if items[i]='exit' then begin
                exit_code:='exit' ;
                window.Close() ;
              end;
            end;
        end ;
    end ;
end;

function TMainMenu.getButtonX(i: Integer): Integer;
begin
  Result:=WINDOW_W div 2-buttonw div 2+(i-2)*3 ;
end;

function TMainMenu.getButtonY(i: Integer): Integer;
begin
  Result:=154+i*40-buttonh div 2 +shifty ;
end;

function TMainMenu.getExitCode: string;
begin
  Result:=exit_code ;
end;

function TMainMenu.isActive: Boolean;
begin
  Result:=active ;
end;

function TMainMenu.isMouseOver(i: Integer): Boolean;
begin
  Result:=(lastmx>getButtonX(i))and(lastmx<getButtonX(i)+buttonw)and
    (lastmy>getButtonY(i))and(lastmy<getButtonY(i)+buttonh) ;
end;

procedure TMainMenu.loadItemsText;
begin
  menu_texts.LoadFromFile('text'+PATH_SEP+'menu.dat.'+options.getLang());
end;

procedure TMainMenu.rebuildItems;
begin
  items.Clear ;
  items.Add(first_item) ;
  items.Add(LANG_ITEM) ;
  if options.isMusicOn then items.Add('music_on') else items.Add('music_off') ;
  if options.isSoundOn then items.Add('sound_on') else items.Add('sound_off') ;
  items.Add('help') ;
  items.Add('exit') ;
end;

procedure TMainMenu.Render;
var
  i: Integer;
begin
  window.Draw(sublayer) ;

  if helpmode then begin
    window.Draw(help_back) ;
    Exit ;
  end;

  window.Draw(back) ;

  for i := 0 to items.Count-1 do begin
    button.Position:=SfmlVector2f(getButtonX(i),getButtonY(i)) ;
    if isMouseOver(i) then begin
      button.Color:=createSFMLColor($FFFFFF) ;
      text.Color:=createSFMLColor($996732) ;
      text.Style:=1 ;
    end
    else begin
      button.Color:=createSFMLColor($A0A0A0) ;
      text.Color:=createSFMLColor($895722) ;
      text.Style:=0 ;
    end;
    text.UnicodeString:=UTF8Decode(menu_texts.Values[items[i]]) ;
    text.Position := SfmlVector2f(WINDOW_W/2-text.LocalBounds.Width/2,getButtonY(i));
    window.Draw(text) ;
  end;
end;

procedure TMainMenu.setActive(value: Boolean);
begin
  active:=value ;
  rebuildItems() ;
end;

end.
