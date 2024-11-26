unit TestSpellGen;

interface
uses
  DUnitX.TestFramework,
  commonclasses, Spell;

type

  [TestFixture]
  TTestSpellGen = class(TObject)
  private
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    [TestCase('Test1','3,2,False')]
    [TestCase('Test2','3,2,True')]
    [TestCase('Test3','3,3,False')]
    [TestCase('Test4','3,3,True')]
    [TestCase('Test5','4,3,False')]
    [TestCase('Test6','4,3,True')]
    [TestCase('Test7','4,4,False')]
    [TestCase('Test8','4,4,True')]
    [TestCase('Test9','5,4,False')]
    [TestCase('Test10','5,4,True')]
    [TestCase('Test11','5,5,False')]
    [TestCase('Test12','5,5,True')]
    procedure TestSpellGen(const Alen : Integer;const Aminincpos : Integer; const Asymm:Boolean);
    [TestCase('TestUnique_1','0')]
    [TestCase('TestUnique_2','1')]
    [TestCase('TestUnique_3','2')]
    [TestCase('TestUnique_AsGame','3')]
    procedure TestUniqueSpells(const idx:Integer) ;
  end;

implementation
uses SysUtils, StrUtils,
  Helpers ;

const TEST_UNIQUE_0:array[0..5] of Integer = (3,2,0,3,3,1) ;
const TEST_UNIQUE_1:array[0..5] of Integer = (3,3,0,3,3,0) ;
const TEST_UNIQUE_2:array[0..5] of Integer = (4,3,0,3,3,0) ;
const TEST_UNIQUE_3:array[0..11] of Integer =
  (3,2,0,3,3,1,4,2,0,4,3,0) ;

procedure TTestSpellGen.Setup;
begin
  Randomize ;
end;

procedure TTestSpellGen.TearDown;
begin
end;

procedure TTestSpellGen.TestSpellGen(const Alen : Integer; const Aminincpos : Integer; const Asymm:Boolean);
var spell:TSpell ;
    q,i,cnt:Integer ;
    issym:Boolean ;
    spells:TUniDictionary<Integer,TSpell> ;
begin
  Randomize ;
  // Много попыток
  for q := 0 to 100 do begin
  spells:=TUniDictionary<Integer,TSpell>.Create ;

  spell.GenByParam(Alen,Aminincpos,Asymm,spells) ;

  {if (q=0) then System.WriteLn(Format('sym=%s min=%d spell= %s',
    [IfThen(Asymm,'true ','false'),Aminincpos,spell.ToString()]));}

  Assert.AreEqual(Alen,spell.len,'test_len') ;
  Assert.IsFalse(spell.activated,'test_activated') ;

  cnt:=0 ;
  for i := 0 to spell.len-1 do
    if spell.seq[i]=Aminincpos-1 then Inc(cnt) ;
  Assert.IsTrue(cnt>0,'test_mininc') ;

  cnt:=0 ;
  for i := 0 to spell.len-1 do
    if (spell.seq[i]<0) or (spell.seq[i]>=Aminincpos) then Inc(cnt) ;
  Assert.IsTrue(cnt=0,'test_interval') ;

  issym:=True ;
  for i := 0 to spell.len-1 do
    issym:=issym and (spell.seq[i]=spell.seq[spell.len-1-i]) ;

  Assert.AreEqual(Asymm,issym,'test_sym') ;

  spells.Free ;
  end;
end;

procedure TTestSpellGen.TestUniqueSpells(const idx:Integer);
var spells:TUniDictionary<Integer,TSpell> ;
    spell:TSpell ;
    q,i,code1,code2:Integer ;
    arr:array of Integer ;

procedure CopyArr(const src:array of Integer) ;
var i:Integer ;
begin
  SetLength(arr,Length(src)) ;
  for i := 0 to Length(src)-1 do
    arr[i]:=src[i] ;
end;

begin
  Randomize ;

  if idx=0 then CopyArr(TEST_UNIQUE_0) ;
  if idx=1 then CopyArr(TEST_UNIQUE_1) ;
  if idx=2 then CopyArr(TEST_UNIQUE_2) ;
  if idx=3 then CopyArr(TEST_UNIQUE_3) ;

  // Много попыток
  for q := 0 to 100 do begin
  spells:=TUniDictionary<Integer,TSpell>.Create ;

  for i := 0 to Length(arr) div 3-1 do
    if spell.GenByParam(arr[3*i],arr[3*i+1],arr[3*i+2]=1,spells) then spells.Add(i,spell) ;

  Assert.AreEqual(Length(arr) div 3,spells.Count,'test_unique_count') ;

  for code1 in spells.AllKeys do
    for code2 in spells.AllKeys do
      if code1<>code2 then
        Assert.IsFalse(spells[code1].isSpellCrossingWith(spells[code2])) ;

  spells.Free ;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestSpellGen);
end.
