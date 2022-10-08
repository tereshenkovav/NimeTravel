unit TestWaySearch;

interface
uses
  DUnitX.TestFramework,
  helpers, commonclasses, waysearch;

type

  [TestFixture]
  TTestWaySearch = class(TObject)
  private
    function addWays(R:TUniList<TWayPoint>; x,y,dx,dy:Integer;
  arr: array of Integer):TUniList<TWayPoint> ;
    procedure addLinks(links:TUniList<TWayLink>; arr:array of Integer) ;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    // Sample Methods
    // Simple single Test
    [Test]
    procedure Test1;
    [Test]
    procedure Test2;
    // Test with TestCase Attribute to supply parameters.
    [Test]
    [TestCase('TestA','1,2')]
    [TestCase('TestB','3,4')]
    procedure TestArg(const AValue1 : Integer;const AValue2 : Integer);
  end;

implementation
uses SysUtils ;

procedure TTestWaySearch.addLinks(links: TUniList<TWayLink>;
  arr: array of Integer);
var i:Integer ;
begin
  for i:=0 to Length(arr)-2 do
    links.Add(TWayLink.Create(arr[i],arr[i+1])) ;
end;

function TTestWaySearch.addWays(R:TUniList<TWayPoint>; x,y,dx,dy:Integer;
  arr: array of Integer): TUniList<TWayPoint>;
var v:Integer ;
    wp:TWayPoint ;
begin
  wp.x:=x ;
  wp.y:=y ;
  wp.z:=0 ;
  for v in arr do begin
    wp.idx:=v ;
    R.Add(wp) ;
    Inc(wp.x,dx) ;
    Inc(wp.y,dy) ;
  end;
  Result:=R ;
end;

procedure TTestWaySearch.Setup;
begin
end;

procedure TTestWaySearch.TearDown;
begin
end;

procedure TTestWaySearch.Test1;
var ws:TWaySearch ;
    links:TUniList<TWayLink> ;
    way:TUniList<Integer> ;
    v:Integer ;
    s:string ;
begin
  links:=TUniList<TWayLink>.Create() ;
  addLinks(links,[0,1,2,3,4]) ;

  ws:=TWaySearch.Create(links,addWays(TUniList<TWayPoint>.Create(),0,0,100,0,[0,1,2,3,4]));

  // Тест одиночного пути
  way:=ws.buildWayStack(90,0,110,0) ;
  Assert.AreEqual(0,way.Count);

  // Тест одиночного пути
  way:=ws.buildWayStack(10,0,110,0) ;
  Assert.AreEqual(0,way.Count);

  way:=ws.buildWayStack(-10,0,110,0) ;
  Assert.AreEqual(2,way.Count);
  Assert.AreEqual(0,way[0]);
  Assert.AreEqual(1,way[1]);

  way:=ws.buildWayStack(-10,0,210,0) ;
  Assert.AreEqual(3,way.Count);
  Assert.AreEqual(0,way[0],'startp');
  Assert.AreEqual(1,way[1],'midp');
  Assert.AreEqual(2,way[2],'endp');

  way:=ws.buildWayStack(90,0,310,0) ;
  Assert.AreEqual(3,way.Count);
  Assert.AreEqual(1,way[0],'startp');
  Assert.AreEqual(2,way[1],'midp');
  Assert.AreEqual(3,way[2],'endp');

  way:=ws.buildWayStack(310,0,110,0) ;
  Assert.AreEqual(3,way.Count);
  Assert.AreEqual(3,way[0],'startp');
  Assert.AreEqual(2,way[1],'midp');
  Assert.AreEqual(1,way[2],'endp');

  way:=ws.buildWayStack(-10,0,410,0) ;
  Assert.AreEqual(5,way.Count);
  Assert.AreEqual(0,way[0],'startp');
  Assert.AreEqual(2,way[2],'midp');
  Assert.AreEqual(4,way[4],'endp');

  // Теперь тест на ход ближе к точке клика вместо хода назад, потом вперед
  way:=ws.buildWayStack(10,0,210,0) ; // 1->2

  s:='*' ;
  for v in way do
    s:=s+' '+IntToStr(v) ;
  s:=s+'*' ;
  //System.WriteLn(s) ;

  Assert.AreEqual(2,way.Count);
  Assert.AreEqual(1,way[0]);
  Assert.AreEqual(2,way[1]);

  //System.WriteLn('Test1') ;

  // И напротив, ближе к цели
  way:=ws.buildWayStack(90,0,210,0) ; // 1->2
  Assert.AreEqual(2,way.Count);
  Assert.AreEqual(1,way[0]);
  Assert.AreEqual(2,way[1]);

end;

procedure TTestWaySearch.Test2;
var ws:TWaySearch ;
    links:TUniList<TWayLink> ;
    way:TUniList<Integer> ;
    wps:TUniList<TWayPoint> ;
begin
  links:=TUniList<TWayLink>.Create() ;
  addLinks(links,[0,1,2,3,4,5]) ;
  addLinks(links,[2,6,7,8]) ;
  addLinks(links,[6,3]) ;

  wps:=TUniList<TWayPoint>.Create() ;
  wps:=addWays(wps,0,0,100,0,[0,1,2,3,4,5]) ;
  wps:=addWays(wps,250,0,100,100,[6,7,8]) ;
  ws:=TWaySearch.Create(links,wps);

  way:=ws.buildWayStack(90,0,410,0) ;
  Assert.AreEqual(4,way.Count);
  Assert.AreEqual(1,way[0],'startp');
  Assert.AreEqual(2,way[1],'midp');
  Assert.AreEqual(3,way[2],'midp');
  Assert.AreEqual(4,way[3],'endp');

  way:=ws.buildWayStack(410,0,110,0) ;
  Assert.AreEqual(4,way.Count);
  Assert.AreEqual(4,way[0],'startp');
  Assert.AreEqual(3,way[1],'midp');
  Assert.AreEqual(2,way[2],'midp');
  Assert.AreEqual(1,way[3],'endp');

  way:=ws.buildWayStack(90,0,250,210) ;
  Assert.AreEqual(4,way.Count);
  Assert.AreEqual(1,way[0],'startp');
  Assert.AreEqual(2,way[1],'midp');
  Assert.AreEqual(6,way[2],'midp');
  Assert.AreEqual(7,way[3],'endp');

  way:=ws.buildWayStack(250,210,110,0) ;
  Assert.AreEqual(4,way.Count);
  Assert.AreEqual(7,way[0],'startp');
  Assert.AreEqual(6,way[1],'midp');
  Assert.AreEqual(2,way[2],'midp');
  Assert.AreEqual(1,way[3],'endp');

  way:=ws.buildWayStack(250,210,410,0) ;
  Assert.AreEqual(4,way.Count);
  Assert.AreEqual(7,way[0],'startp');
  Assert.AreEqual(6,way[1],'midp');
  Assert.AreEqual(3,way[2],'midp');
  Assert.AreEqual(4,way[3],'endp');

  way:=ws.buildWayStack(410,0,250,210) ;
  Assert.AreEqual(4,way.Count);
  Assert.AreEqual(4,way[0],'startp');
  Assert.AreEqual(3,way[1],'midp');
  Assert.AreEqual(6,way[2],'midp');
  Assert.AreEqual(7,way[3],'endp');

end;

procedure TTestWaySearch.TestArg(const AValue1 : Integer;const AValue2 : Integer);
begin
end;

initialization
  TDUnitX.RegisterTestFixture(TTestWaySearch);
end.

