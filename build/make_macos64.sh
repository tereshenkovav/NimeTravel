mkdir x86_64-darwin
ppcx64 @fpc-config -Fl/usr/local/lib -k"-lcsfml-graphics.2.5.1 -lcsfml-system.2.5.1 -lcsfml-window.2.5.1 -lcsfml-audio.2.5.1 -rpath @executable_path/../Frameworks" ../src/NimeTravelFPC.pp
