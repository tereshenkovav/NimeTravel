appdir=/tmp/NimeTravel.app
mkdir $appdir
mkdir $appdir/Contents
mkdir $appdir/Contents/MacOS
mkdir $appdir/Contents/Frameworks
mkdir $appdir/Contents/Resources

cp Info.plsit $appdir/Contents
cp Pkginfo $appdir/Contents

cp NimeTravel.icns $appdir/Contents/Resources

cp ../../bin/NimeTravelFPC $appdir/Contents/MacOS/NimeTravel
cp -r ../../bin/fonts $appdir/Contents/MacOS
cp -r ../../bin/images $appdir/Contents/MacOS
cp -r ../../bin/scenes $appdir/Contents/MacOS
cp -r ../../bin/scripts $appdir/Contents/MacOS
cp -r ../../bin/sounds $appdir/Contents/MacOS
cp -r ../../bin/music $appdir/Contents/MacOS
cp -r ../../bin/text $appdir/Contents/MacOS

cp /usr/local/lib/libcsfml*.dylib $appdir/Contents/Frameworks
cp /usr/local/lib/libsfml*.dylib $appdir/Contents/Frameworks

cp -r /Library/Frameworks/FLAC.framework $appdir/Contents/Frameworks
cp -r /Library/Frameworks/freetype.framework $appdir/Contents/Frameworks
cp -r /Library/Frameworks/ogg.framework $appdir/Contents/Frameworks
cp -r /Library/Frameworks/OpenAL.framework $appdir/Contents/Frameworks
cp -r /Library/Frameworks/vorbis.framework $appdir/Contents/Frameworks
cp -r /Library/Frameworks/vorbisenc.framework $appdir/Contents/Frameworks
cp -r /Library/Frameworks/vorbisfile.framework $appdir/Contents/Frameworks

cd /tmp 
echo en > $appdir/text/default
zip -r NimeTravel-prologue-EN-1.1.0-MacOS.app.zip NimeTravel.app

echo ru > $appdir/text/default
zip -r NimeTravel-prologue-RU-1.1.0-MacOS.app.zip NimeTravel.app
