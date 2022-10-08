unit commonclasses ;

interface
uses helpers ;

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

  TSpell = record
    seq:array[0..9] of Integer ;
    len:Integer ;
    activated:Boolean ;
    procedure GenByParam(Alen:Integer; minincpos:Integer; symmetric:Boolean) ;
    function isSpellMatchDirect(testseq:TUniList<Integer>):Boolean ;
    function isSpellMatchReverse(testseq:TUniList<Integer>):Boolean ;
    function isSpellCrossingWith(const spell:TSpell):Boolean ;
    function ToString():string ;
  end;

  TProcSetMusicAndSound = procedure of object ;

  TOptions = class
  private
    music:Boolean ;
    sound:Boolean ;
    procsetmusicandsound:TProcSetMusicAndSound ;
  public
    function isMusicOn():Boolean ;
    function isSoundOn():Boolean ;
    procedure switchMusic() ;
    procedure switchSound() ;
    procedure setProcSetMusicAndSound(proc:TProcSetMusicAndSound) ;
    constructor Create() ;
    destructor Destroy ; override ;
  end;

  TSkipClick = (scPartial,scFull) ;
  TLookState = (lsLeft,lsRight,lsNone) ;

function isLinked(links:TUniList<TWayLink>;idx1,idx2:Integer):Boolean ;

const
  WINDOW_W = 800 ;
  WINDOW_H = 600 ;

implementation
uses Math, SysUtils ;

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

{ TSpell }

procedure TSpell.GenByParam(Alen, minincpos: Integer; symmetric: Boolean);
var i,p:Integer ;
begin
  //+1 Переделать на более случайное создание
  // 2 Проверять пересечение с другими
  //+3 Задействовать симметрию
  len:=Alen ;
  for i := 0 to len-1 do
    seq[i]:=-1 ;

  if symmetric then begin
    seq[Random(len div 2)]:=minincpos-1 ;
    for i := 0 to (len+1)div 2-1 do
      if seq[i]=-1 then seq[i]:=Random(minincpos-1) ;
    for i := 0 to (len+1)div 2-1 do
      seq[len-1-i]:=seq[i] ;
  end
  else begin
    p:=Random((len+1)div 2 - 1) ;
    if Random(2)=0 then seq[p]:=minincpos-1 else seq[len-1-p]:=minincpos-1 ;
    for i := 0 to len-1 do
      if seq[i]=-1 then seq[i]:=Random(minincpos-1) ;
  end ;

  activated:=False ;
end;

function TSpell.isSpellCrossingWith(const spell: TSpell): Boolean;
var i:Integer ;
begin
  for i := 0 to Min(spell.len,len)-1 do
    if seq[i]<>spell.seq[i] then Exit(False) ;
  Result:=True ;
end;

function TSpell.isSpellMatchDirect(testseq: TUniList<Integer>): Boolean;
var i:Integer ;
begin
  if not activated then Exit(False) ;

  if testseq.Count<>len then Exit(False) ;
  for i := 0 to len-1 do
    if seq[i]<>testseq[i] then Exit(False) ;
  Result:=True ;
end;

function TSpell.isSpellMatchReverse(testseq: TUniList<Integer>): Boolean;
var i:Integer ;
begin
  if not activated then Exit(False) ;

  if testseq.Count<>len then Exit(False) ;
  for i := 0 to len-1 do
    if seq[i]<>testseq[len-1-i] then Exit(False) ;
  Result:=True ;
end;

function TSpell.ToString: string;
var i:Integer ;
begin
  Result:='' ;
  for i := 0 to len-1 do
    Result:=Result+IntToStr(seq[i]) ;
end;


function isLinked(links:TUniList<TWayLink>;idx1,idx2:Integer):Boolean ;
var link:TWayLink ;
begin
  Result:=False ;
  for link in links do
    if link.isLinkBetween(idx1,idx2) then Exit(True) ;
end;

{ TOptions }

constructor TOptions.Create;
begin
  music:=True ;
  sound:=True ;
  procsetmusicandsound:=nil ;
end;

destructor TOptions.Destroy;
begin
  inherited Destroy;
end;

function TOptions.isSoundOn: Boolean;
begin
  Result:=sound ;
end;

function TOptions.isMusicOn: Boolean;
begin
  Result:=music ;
end;

procedure TOptions.setProcSetMusicAndSound(proc: TProcSetMusicAndSound);
begin
  procsetmusicandsound:=proc ;
end;

procedure TOptions.switchMusic;
begin
  music:=not music ;
  if Assigned(procsetmusicandsound) then procsetmusicandsound() ;
end;

procedure TOptions.switchSound;
begin
  sound:=not sound ;
  if Assigned(procsetmusicandsound) then procsetmusicandsound() ;
end;

end.