unit waysearch ;

interface
uses helpers, commonclasses ;

type
  TWaySearch = class
  private
    links:TUniList<TWayLink>;
    way:TUniList<TWayPoint> ;
    procedure goToWay(tekway,minway:TUniList<Integer>; nextp,target:Integer) ;
    function getShortWay(way1,way2:Integer):TUniList<Integer> ;
  public
    constructor Create(Alinks:TUniList<TWayLink>; Away:TUniList<TWayPoint>) ;
    function buildWayStack(herox,heroy,x,y:Integer):TUniList<Integer> ;
  end ;

implementation
uses commonproc ;

function TWaySearch.buildWayStack(herox,heroy,x,y:Integer): TUniList<Integer>;
var mindistfirst:Single ;
    mindistlast:Single ;
    idxfirst,ridx:Integer ;
    idxlast:Integer ;
    i,delta:Integer ;
    ws:TWaySearch ;
    wp:TWayPoint ;
    waydef,wayalt:TUniList<Integer> ;
begin
  mindistfirst:=99999 ;
  mindistlast:=99999 ;
  idxfirst:=-1 ;
  idxlast:=-1 ;

  for i:=0 to way.Count-1 do begin
    if (dist2(way[i].x,herox,way[i].y,heroy)<mindistfirst) then begin
      mindistfirst:=dist2(way[i].x,herox,way[i].y,heroy) ;
      idxfirst:=i ;
    end ;
    if (dist2(way[i].x,x,way[i].y,y)<mindistlast) then begin
      mindistlast:=dist2(way[i].x,x,way[i].y,y) ;
      idxlast:=i ;
    end ;
  end ;

  if ((idxfirst<>-1)and(idxlast<>-1)and(idxfirst<>idxlast)) then begin
    // Если в одном шаге от финального пути и условие расстояния соблюдено
    if isLinked(links,way[idxlast].idx,way[idxfirst].idx) and
      (dist2(herox,way[idxlast].x,heroy,way[idxlast].y)<
       0.90*dist2(way[idxfirst].x,way[idxlast].x,way[idxfirst].y,way[idxlast].y)) then begin
      Result:=TUniList<Integer>.Create() ;
      Exit ;
    end ;

    ridx:=idxfirst ;
    waydef:=getShortWay(idxfirst,idxlast) ;

    for i:=0 to way.Count-1 do
      if (i<>idxfirst)and(i<>idxlast)and isLinked(links,way[i].idx,way[idxfirst].idx) then begin
        wayalt:=getShortWay(i,idxlast) ;
        if (wayalt.Count+1=waydef.Count)and(
         dist2(herox,way[i].x,heroy,way[i].y)<
         dist2(way[idxfirst].x,way[i].x,way[idxfirst].y,way[i].y)) then
           ridx:=i ;
        wayalt.Free ;
      end;
    waydef.Free ;

    Result:=getShortWay(ridx,idxlast) ;
  end
  else
    Result:=TUniList<Integer>.Create() ;
end;

constructor TWaySearch.Create(Alinks:TUniList<TWayLink>; Away:TUniList<TWayPoint>) ;
begin
  links:=Alinks ;
  way:=Away ;
end ;

function TWaySearch.getShortWay(way1,way2:Integer):TUniList<Integer> ;
var waystack:TUniList<Integer> ;
begin
  Result:=TUniList<Integer>.Create ;
  waystack:=TUniList<Integer>.Create ;
  goToWay(waystack,Result,way1,way2) ;
  waystack.Free ;
  //Result.Add(way2) ;
end ;

procedure TWaySearch.goToWay(tekway,minway: TUniList<Integer>; nextp,target: Integer);
var w:TWayPoint ;
    link:TWayLink ;
begin
  tekway.Add(nextp) ;
  if nextp=target then begin
    if (minway.Count=0)or(minway.Count>tekway.Count) then begin
      minway.Clear() ;
      minway.AddRange(tekway);
    end;
  end
  else
    for w in way do
      if (w.idx<>nextp)and (tekway.IndexOf(w.idx)=-1) then
        if isLinked(links,nextp,w.idx) then
          goToWay(tekway,minway,w.idx,target) ;
  tekway.Delete(tekway.Count-1);
end;

end.