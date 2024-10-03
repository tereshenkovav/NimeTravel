unit commonproc ;

interface
uses CommonClasses ;

function dist2(x1,x2,y1,y2:Single):Single ;
function getWindowTitle(opt:TOptions):string;

implementation
uses Classes, Helpers ;

function dist2(x1,x2,y1,y2:Single):Single ;
begin
  Result:=(x1-x2)*(x1-x2)+(y1-y2)*(y1-y2) ;
end ;

function getWindowTitle(opt:TOptions):string;
begin
  with TStringList.Create do begin
    LoadFromFile('text'+PATH_SEP+'common.dat.'+opt.getLang());
    Result:=Values['caption'] ;
    Free ;
  end;
end;

end.