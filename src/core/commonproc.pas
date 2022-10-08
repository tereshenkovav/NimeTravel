unit commonproc ;

interface

function dist2(x1,x2,y1,y2:Single):Single ;

implementation

function dist2(x1,x2,y1,y2:Single):Single ;
begin
  Result:=(x1-x2)*(x1-x2)+(y1-y2)*(y1-y2) ;
end ;

end.