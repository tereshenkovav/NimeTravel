#!/bin/bash

for i in `git tag --list --sort=committerdate`; do BUILDTAG=$i; done

for i in `git rev-parse HEAD`; do BUILDCOMMIT=$i; done
BUILDCOMMIT=${BUILDCOMMIT:0:8}

for i in `git rev-parse --abbrev-ref HEAD`; do BUILDBRANCH=$i; done

echo $BUILDTAG $BUILDCOMMIT $BUILDBRANCH

VERSION=${BUILDTAG:1}

echo $BUILDTAG > ../../bin/text/version.txt
echo $BUILDCOMMIT >> ../../bin/text/version.txt
echo $BUILDBRANCH >> ../../bin/text/version.txt

appdir=/tmp/NimeTravel.AppDir

rm -rf $appdir

mkdir $appdir
cp appruns/AppRun-x86_64 $appdir/AppRun
cp ../../graphics/main.png $appdir/NimeTravel.png
pushd $appdir
ln -s NimeTravel.png .DirIcon
popd

cp NimeTravel.desktop $appdir
mkdir $appdir/usr
mkdir $appdir/usr/bin
mkdir $appdir/usr/lib

cp /usr/lib64/libsfml* $appdir/usr/lib
cp /usr/local/lib/libcsfml* $appdir/usr/lib
cp /usr/lib64/libvorbis* $appdir/usr/lib
cp /usr/lib64/libasound.so* $appdir/usr/lib
cp /usr/lib64/libfreetype.so* $appdir/usr/lib
cp /usr/lib64/libjpeg.so* $appdir/usr/lib
cp /usr/lib64/libopenal.so* $appdir/usr/lib
cp /usr/lib64/libatomic.so* $appdir/usr/lib
cp /usr/lib64/libGLU.so* $appdir/usr/lib
cp /usr/lib64/libharfbuzz.so* $appdir/usr/lib
cp /usr/lib64/libpng16.so* $appdir/usr/lib
cp /usr/lib64/libogg.so* $appdir/usr/lib
cp /usr/lib64/libFLAC.so* $appdir/usr/lib

cp ../../bin/NimeTravelFPC $appdir/usr/bin/NimeTravel
cp -r ../../bin/fonts $appdir/usr/bin
cp -r ../../bin/images $appdir/usr/bin
cp -r ../../bin/scenes $appdir/usr/bin
cp -r ../../bin/scripts $appdir/usr/bin
cp -r ../../bin/sounds $appdir/usr/bin
cp -r ../../bin/music $appdir/usr/bin
cp -r ../../bin/text $appdir/usr/bin

export ARCH=x86_64

echo en > $appdir/usr/bin/text/default
appimagetool-x86_64.AppImage $appdir /tmp/NimeTravel-EN-$VERSION-x86_64.AppImage

echo ru > $appdir/usr/bin/text/default
appimagetool-x86_64.AppImage $appdir /tmp/NimeTravel-RU-$VERSION-x86_64.AppImage
