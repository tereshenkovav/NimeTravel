program AuraGen;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  SfmlGraphics,
  TypeConverters,
  Helpers, AuraMaker ;

var img:TSfmlImage ;
    imgout:TImageShifted ;
    list:TUniList<TImageShifted> ;
    i:Integer ;
    outbase,outfile:string ;
    c:TSfmlColor ;
begin
  if ParamCount<4 then begin
    Writeln('Arguments: sourcefile freq bordersize auracolor [outfile]') ;
    Writeln('Example: scene1\book.png 12 5 scene1\customfilename $B0006080') ;
    Writeln('If outfile empty, uses sourcefile without ".png"') ;
    Exit ;
  end ;

  if ParamCount>4 then outbase:=ParamStr(5) else outbase:=ParamStr(1).Replace('.png','') ;

  try
    img:=TSfmlImage.Create(ParamStr(1)) ;

    with TAuraMaker.Create(img) do begin
      with TypeConverters.String2RGBA(ParamStr(4)) do
        c:=SFMLColorFromRGBA(R,G,B,A) ;
      list:=GenAuraImages(StrToInt(ParamStr(2)),StrToInt(ParamStr(3)),c) ;
      imgout:=TAuraMaker.ImagesToSolidImage(list) ;

      outfile:=outbase+'.aura.png' ;
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
