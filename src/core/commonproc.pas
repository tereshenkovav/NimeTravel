unit commonproc ;

interface
uses CommonClasses ;

function dist2(x1,x2,y1,y2:Single):Single ;

implementation
uses Classes, Helpers ;

function dist2(x1,x2,y1,y2:Single):Single ;
begin
  Result:=(x1-x2)*(x1-x2)+(y1-y2)*(y1-y2) ;
end ;

end.