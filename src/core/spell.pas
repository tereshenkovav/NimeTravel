unit Spell ;

interface
uses helpers ;

type
  TSpell = record
    seq:array[0..9] of Integer ;
    len:Integer ;
    activated:Boolean ;
    iconfile:string ;
    procedure GenByParam(Alen:Integer; minincpos:Integer; symmetric:Boolean) ;
    function isSpellMatchDirect(testseq:TUniList<Integer>):Boolean ;
    function isSpellMatchReverse(testseq:TUniList<Integer>):Boolean ;
    function isSpellCrossingWith(const spell:TSpell):Boolean ;
    function ToString():string ;
    class operator Equal(a: TSpell; b: TSpell): Boolean;
  end;

implementation
uses Math, SysUtils ;

{ TSpell }

class operator TSpell.Equal(a, b: TSpell): Boolean;
begin
  Result:=False ;
end;

procedure TSpell.GenByParam(Alen, minincpos: Integer; symmetric: Boolean);
var i,p:Integer ;
    testsym:Boolean ;
begin
  // 2 Проверять пересечение с другими
  len:=Alen ;
  for i := 0 to len-1 do
    seq[i]:=-1 ;

  if symmetric then begin
    p:=len div 2 ;
    if len mod 2 = 1 then Inc(p) ;
    seq[Random(p)]:=minincpos-1 ;
    for i := 0 to p-1 do
      if seq[i]=-1 then seq[i]:=Random(minincpos-1) ;
    for i := 0 to (len+1)div 2-1 do
      seq[len-1-i]:=seq[i] ;
  end
  else begin
    repeat
      for i := 0 to len-1 do
        seq[i]:=-1 ;
      seq[Random(len)]:=minincpos-1 ;
      for i := 0 to len-1 do
        if seq[i]=-1 then seq[i]:=Random(minincpos) ;
      testsym:=True ;
      for i := 0 to len-1 do
        testsym:=testsym and (seq[i]=seq[len-1-i]) ;
    until not testsym;
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

end.
