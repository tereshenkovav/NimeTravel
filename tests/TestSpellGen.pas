unit TestSpellGen;

interface
uses
  DUnitX.TestFramework,
  commonclasses;

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
  end;

implementation
uses SysUtils, StrUtils ;

procedure TTestSpellGen.Setup;
begin
end;

procedure TTestSpellGen.TearDown;
begin
end;

procedure TTestSpellGen.TestSpellGen(const Alen : Integer; const Aminincpos : Integer; const Asymm:Boolean);
var spell:TSpell ;
    q,i,cnt:Integer ;
    issym:Boolean ;
begin
  Randomize ;
  // Много попыток
  for q := 0 to 100 do begin

  spell.GenByParam(Alen,Aminincpos,Asymm) ;

  {if (q=0) then System.WriteLn(Format('sym=%s min=%d spell= %s',
    [IfThen(Asymm,'true ','false'),Aminincpos,spell.ToString()]));}

  Assert.AreEqual(Alen,spell.len,'test_len') ;
  Assert.IsFalse(spell.activated,'test_activated') ;

  cnt:=0 ;
  for i := 0 to spell.len-1 do
    if spell.seq[i]=Aminincpos-1 then Inc(cnt) ;
  Assert.IsTrue((cnt=1) or (cnt=2),'test_mininc') ;

  cnt:=0 ;
  for i := 0 to spell.len-1 do
    if (spell.seq[i]<0) or (spell.seq[i]>=Aminincpos) then Inc(cnt) ;
  Assert.IsTrue(cnt=0,'test_interval') ;

  issym:=True ;
  for i := 0 to spell.len-1 do
    issym:=issym and (spell.seq[i]=spell.seq[spell.len-1-i]) ;

  Assert.AreEqual(Asymm,issym,'test_sym') ;


  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestSpellGen);
end.
