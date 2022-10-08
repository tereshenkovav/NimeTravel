unit helpers ;

interface

{$ifdef fpc}
uses fgl, classes ;
{$else}
uses Generics.Collections, Generics.Defaults, IOUtils, SysUtils ;
{$endif}

type

{$ifdef fpc}
TUniList<T> = class(TFPGList<T>)
private
public
  procedure AddRange(src:TUniList<T>) ;
end;

TUniDictionary<K,V> = class(TFPGMap<K,V>)
private
  FKeys:TUniList<K> ;
public
  function ContainsKey(const key:K):Boolean ;
  function AllKeys():TUniList<K> ;
end;

{$else}
TSortFunc<T> = function(const a:T; const b:T):Integer ;

TUniList<T> = class(TList<T>)
private
public
  procedure Sort(func:TSortFunc<T>) ;
end;

TUniDictionary<K,V> = class(TDictionary<K,V>)
private
public
  function AllKeys():TArray<K> ;
end;
{$endif}

function readAllText(filename:string):string ;

{$ifdef fpc}
{$ifdef unix}
const PATH_SEP = '/' ;
{$else}
const PATH_SEP = '\' ;
{$endif}
{$else}
const PATH_SEP = '\' ;
{$endif}

implementation

{$ifdef fpc}

procedure TUniList<T>.AddRange(src:TUniList<T>) ;
var el:T ;
begin
  for el in src do
    Self.Add(el) ;
end ;

function TUniDictionary<K,V>.ContainsKey(const key:K):Boolean ;
begin
  Result:=IndexOf(key)<>-1 ;
end ;

function TUniDictionary<K,V>.AllKeys():TUniList<K> ;
var i:Integer ;
begin
  if FKeys=nil then FKeys:=TUniList<K>.Create ;
  FKeys.Clear() ;
  for i:=0 to Count-1 do
    FKeys.Add(GetKey(i)) ;
  Result:=FKeys ;
end ;

{$else}

procedure TUniList<T>.Sort(func: TSortFunc<T>);
begin
  inherited Sort(TComparer<T>.Construct(func));
end;

function TUniDictionary<K,V>.AllKeys():TArray<K> ;
begin
  Result:=Keys.ToArray() ;
end ;

{$endif}

function readAllText(filename:string):string ;
begin
  {$ifdef fpc}
  with TStringList.Create do begin
    LoadFromFile(FileName) ;
    Result:=Text ;
    Free ;
  end ;
  {$else}
  Result:=TFile.ReadAllText(filename) ;
  {$endif}
end ;

end.