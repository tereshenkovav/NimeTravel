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
begin
  // 2 Проверять пересечение с другими
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

end.
