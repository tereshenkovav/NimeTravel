unit wayrenderer ;

interface
uses SfmlGraphics, SfmlSystem,
  Logic ;

type
  TWayRenderer = class
  private
    window: TSfmlRenderWindow ;
    circle:TSfmlRectangleShape ;
    line:TSfmlVertexArray;
  public
    constructor Create(Awindow:TSfmlRenderWindow) ;
    destructor Destroy ; override ;
    procedure Render(lobj:TLogic) ;
  end;

implementation

{ TWayRenderer }

constructor TWayRenderer.Create(Awindow: TSfmlRenderWindow);
begin
  window:=Awindow ;
  circle:=TSfmlRectangleShape.Create() ;
  circle.Size:=SfmlVector2f(20,5) ;
  circle.Origin:=SfmlVector2f(10,2) ;
  circle.OutlineColor:=SfmlBlue ;
  circle.OutlineThickness:=2 ;
  circle.FillColor:=SfmlTransparent ;

  line:=TSfmlVertexArray.Create() ;
  line.PrimitiveType:=sfLines ;
  line.Resize(2);
  line.Vertex[0].Color:=SfmlBlue ;
  line.Vertex[1].Color:=SfmlBlue ;
end;

destructor TWayRenderer.Destroy;
begin
  circle.Free ;
  line.Free ;
  inherited Destroy;
end;

procedure TWayRenderer.Render(lobj:TLogic);
var i:Integer ;
begin
  for i := 0 to lobj.getWayPointCount()-1 do begin
    circle.Position:=SfmlVector2f(lobj.getWayPoint(i).x,lobj.getWayPoint(i).y) ;
    window.Draw(circle);
  end;
  for i := 0 to lobj.getWayLinkCount()-1 do begin
    with lobj.getWayPoint(lobj.getWayLink(i).idx1) do
      line.Vertex[0].Position:=SfmlVector2f(x,y) ;
    with lobj.getWayPoint(lobj.getWayLink(i).idx2) do
      line.Vertex[1].Position:=SfmlVector2f(x,y) ;
    window.Draw(line)
  end;
end;

end.