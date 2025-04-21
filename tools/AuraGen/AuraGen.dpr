program AuraGen;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  SfmlGraphics,
  Helpers, AuraMaker ;

var img:TSfmlImage ;
    imgout:TImageShifted ;
    list:TUniList<TImageShifted> ;
    i:Integer ;
    outfile:string ;
begin
  if ParamCount<3 then begin
    Writeln('Arguments: sourcefile freq bordersize') ;
    Writeln('Example: scene1\book.png 12 5') ;
    Exit ;
  end ;

  try
    img:=TSfmlImage.Create(ParamStr(1)) ;
    with TAuraMaker.Create(img) do begin
      list:=GenAuraImages(StrToInt(ParamStr(2)),StrToInt(ParamStr(3))) ;
      imgout:=TAuraMaker.ImagesToSolidImage(list) ;
      outfile:=Format('%s.aura.%d.%d.png',
        [ParamStr(1).Replace('.png',''),imgout.originx,imgout.originy]) ;
      imgout.img.SaveToFile(outfile) ;
      imgout.img.Free ;
      for i := 0 to list.Count-1 do
        list[i].img.Free ;
      list.Free ;
      Free ;
    end ;
    img.Free ;
    Writeln('File '+outfile+' saved') ;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
