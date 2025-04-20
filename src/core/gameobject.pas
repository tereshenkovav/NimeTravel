unit gameobject;

interface

type
  TGameObject = class
  public
    code:string ;
    filename:string ;
    aurafilename:string ;
    auraposx:Integer ;
    auraposy:Integer ;
    icofile:string ;
    caption:string ;
    x,y:Integer ;
    z:Single ;
    isactive:Boolean ;
    callbackinfo:string ;
    callbackspell:string ;
    way_idx:Integer ;
    transp:Integer ;
  end;

implementation

end.
