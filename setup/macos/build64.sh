for i in `git tag --list --sort=v:refname`; do BUILDTAG=$i; done

for i in `git rev-parse HEAD`; do BUILDCOMMIT=$i; done
BUILDCOMMIT=${BUILDCOMMIT:0:8}

for i in `git rev-parse --abbrev-ref HEAD`; do BUILDBRANCH=$i; done

echo $BUILDTAG $BUILDCOMMIT $BUILDBRANCH

VERSION=${BUILDTAG:1}

echo $BUILDTAG > ../../bin/text/version.txt
echo $BUILDCOMMIT >> ../../bin/text/version.txt
echo $BUILDBRANCH >> ../../bin/text/version.txt

appdir=/tmp/NimeTravel.app
mkdir $appdir
mkdir $appdir/Contents
mkdir $appdir/Contents/MacOS
mkdir $appdir/Contents/Frameworks
mkdir $appdir/Contents/Resources

cp Info.plist $appdir/Contents
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

cp -R /usr/local/lib/libcsfml*.dylib $appdir/Contents/Frameworks
cp -R /usr/local/lib/libsfml*.dylib $appdir/Contents/Frameworks

cp -R /Library/Frameworks/FLAC.framework $appdir/Contents/Frameworks
cp -R /Library/Frameworks/freetype.framework $appdir/Contents/Frameworks
cp -R /Library/Frameworks/ogg.framework $appdir/Contents/Frameworks
cp -R /Library/Frameworks/OpenAL.framework $appdir/Contents/Frameworks
cp -R /Library/Frameworks/vorbis.framework $appdir/Contents/Frameworks
cp -R /Library/Frameworks/vorbisenc.framework $appdir/Contents/Frameworks
cp -R /Library/Frameworks/vorbisfile.framework $appdir/Contents/Frameworks

cd /tmp 

echo en > $appdir/Contents/MacOS/text/default
zip -r9 NimeTravel-prologue-EN-$VERSION-MacOS.app.zip NimeTravel.app
hdiutil create -srcfolder $appdir -volname "NimeTravel" -fs HFS+ -fsargs "-c c=64,a=16,e=16" -format UDZO -size 30000k -imagekey zlib-level=9 NimeTravel-EN-$VERSION-MacOS.dmg

echo ru > $appdir/Contents/MacOS/text/default
zip -r9 NimeTravel-prologue-RU-$VERSION-MacOS.app.zip NimeTravel.app
hdiutil create -srcfolder $appdir -volname "NimeTravel" -fs HFS+ -fsargs "-c c=64,a=16,e=16" -format UDZO -size 30000k -imagekey zlib-level=9 NimeTravel-RU-$VERSION-MacOS.dmg
