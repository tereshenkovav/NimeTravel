mkdir /tmp/ppu
ppcx64 -Fusfml -Futools -Fupascalscript -Fucore -FE../bin -FU/tmp/ppu -Fl/usr/local/lib -k"-lcsfml-graphics.2.5.1 -lcsfml-system.2.5.1 -lcsfml-window.2.5.1 -lcsfml-audio.2.5.1 -rpath @executable_path/../Frameworks" -Sm -Mdelphi -Sh NimeTravelFPC.pp

