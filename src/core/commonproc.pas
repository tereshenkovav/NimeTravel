unit commonproc ;

interface
uses CommonClasses ;

function dist2(x1,x2,y1,y2:Single):Single ;
function getWindowTitle():string;

implementation
uses Classes, Helpers, CommonData ;

function dist2(x1,x2,y1,y2:Single):Single ;
begin
  Result:=(x1-x2)*(x1-x2)+(y1-y2)*(y1-y2) ;
end ;

function getWindowTitle():string;
begin
  Result:=TCommonData.texts.getText('caption') ;
end;

end.