#!/bin/bash

appdir=/tmp/NimeTravel.AppDir

rm -rf $appdir

mkdir $appdir
cp appruns/AppRun-x86_64 $appdir/AppRun
chmod 777 $appdir/AppRun
cp ../../graphics/main.png $appdir/NimeTravel.png
pushd $appdir
ln -s NimeTravel.png .DirIcon
popd

cp NimeTravel.desktop $appdir
mkdir $appdir/usr
mkdir $appdir/usr/bin
mkdir $appdir/usr/lib

cp /usr/lib/x86_64-linux-gnu/libsfml* $appdir/usr/lib
cp /usr/lib/x86_64-linux-gnu/libcsfml* $appdir/usr/lib
cp /usr/lib/x86_64-linux-gnu/libvorbis* $appdir/usr/lib
cp /usr/lib/x86_64-linux-gnu/libasound.so* $appdir/usr/lib
cp /usr/lib/x86_64-linux-gnu/libbsd.so* $appdir/usr/lib
cp /usr/lib/x86_64-linux-gnu/libfreetype.so* $appdir/usr/lib
cp /usr/lib/x86_64-linux-gnu/libjpeg.so* $appdir/usr/lib
cp /usr/lib/x86_64-linux-gnu/libopenal.so* $appdir/usr/lib
cp /usr/lib/x86_64-linux-gnu/libsndio.so* $appdir/usr/lib

cp ../../bin/NimeTravelFPC $appdir/usr/bin/NimeTravel
cp -r ../../bin/fonts $appdir/usr/bin
cp -r ../../bin/images $appdir/usr/bin
cp -r ../../bin/scenes $appdir/usr/bin
cp -r ../../bin/scripts $appdir/usr/bin
cp -r ../../bin/sounds $appdir/usr/bin
cp -r ../../bin/music $appdir/usr/bin
cp -r ../../bin/text $appdir/usr/bin

chmod 777 appimagetool-x86_64.AppImage
export ARCH=x86_64
./appimagetool-x86_64.AppImage $appdir
