unit TypeConverters ;

///  Файл взят из библиотеки TAVPascalLib, коммит 197cd686

{
  Полезные процедуры преобразований
}

interface

type
  TRGBA = record
    R:Byte ;
    G:Byte ;
    B:Byte ;
    A:Byte ;
  end;
{
   Процедура принимает цвет в шестнадцатеричной форме в формате RGB или RGBA,
   с возможностью игнорирования первого символа, если он равен $ или #.
   Также игнорируется регистр.
   Таким образом, возможны варианты:
   FFFFFFFF
   FFFFFF
   ffffffff
   ffffff
   $FFFFFFFF
   $FFFFFF
   $ffffffff
   $ffffff
   #FFFFFFFF
   #FFFFFF
   #ffffffff
   #ffffff

   В остальных случаях должно кидать исключение
}
function String2RGBA(const str:string):TRGBA ;

implementation
uses SysUtils, Classes ;

function String2RGBA(const str:string):TRGBA ;
var s:string ;
    b:TBytes ;
begin
  s:=str ;
  if (s[1]='$') or (s[1]='#') then s:=s.Substring(1) ;

  SetLength(b,s.Length div 2) ;
  HexToBin(PWideChar(s),b,Length(b)) ;

  Result.R:=b[0] ;
  Result.G:=b[1] ;
  Result.B:=b[2] ;
  if Length(b)=4 then Result.A:=b[3] else
  if Length(b)=3 then Result.A:=$FF else
    raise Exception.Create('Invalid color string: '+str);
  SetLength(b,0) ;
end ;

end.
