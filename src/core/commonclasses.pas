unit commonclasses ;

interface
uses helpers, Classes ;

type
  TWayPoint = record
    idx:Integer ;
    x:Integer ;
    y:Integer ;
    z:Single ;
    isout:Boolean ;
    class operator Equal(a: TWayPoint; b: TWayPoint): Boolean;
    constructor Create(Aidx,Ax,Ay:Integer; Az:Single; Aisout:Boolean) ;
  end;

  TWayLink = record
    idx1:Integer ;
    idx2:Integer ;
    constructor Create(Aidx1,Aidx2:Integer) ;
    class operator Equal(a: TWayLink; b: TWayLink): Boolean;
    function isLinkBetween(i1,i2:Integer):Boolean ;
  end;

  TStaticTask = record
    text:string ;
    textsize:Integer ;
    constructor Create(Atext:string; Atextsize:Integer) ;
    class operator Equal(a: TStaticTask; b: TStaticTask): Boolean;
  end;

  TSkipClick = (scPartial,scFull) ;
  TLookState = (lsLeft,lsRight,lsNone) ;

function isLinked(links:TUniList<TWayLink>;idx1,idx2:Integer):Boolean ;

const
  DEFAULT_LANG = 'ru' ;

implementation

{ TWayPoint }

constructor TWayPoint.Create(Aidx, Ax, Ay: Integer; Az: Single; Aisout:Boolean);
begin
  idx:=Aidx ;
  isout:=Aisout ;
  x:=Ax ;
  y:=Ay ;
  z:=Az ;
end;

class operator TWayPoint.Equal(a, b: TWayPoint): Boolean;
begin
  Result:=(a.idx=b.idx)and(a.x=b.x)and(a.y=b.y)and(abs(a.z-b.z)<0.0001) ;
end;

{ TWayLink }

constructor TWayLink.Create(Aidx1, Aidx2: Integer);
begin
  idx1:=Aidx1 ;
  idx2:=Aidx2 ;
end;

class operator TWayLink.Equal(a, b: TWayLink): Boolean;
begin
  Result:=(a.idx1=b.idx1)and(b.idx1=b.idx2) ;
end;

function TWayLink.isLinkBetween(i1, i2: Integer): Boolean;
begin
  Result:=((idx1=i1)and(idx2=i2))or((idx1=i2)and(idx2=i1)) ;
end;

function isLinked(links:TUniList<TWayLink>;idx1,idx2:Integer):Boolean ;
var link:TWayLink ;
begin
  Result:=False ;
  for link in links do
    if link.isLinkBetween(idx1,idx2) then Exit(True) ;
end;

{ TStaticTask }

constructor TStaticTask.Create(Atext: string; Atextsize: Integer);
begin
  text:=Atext ;
  textsize:=Atextsize ;
end;

class operator TStaticTask.Equal(a, b: TStaticTask): Boolean;
begin
  Result:=(a.text=b.text)and(a.textsize=b.textsize) ;
end;

end.