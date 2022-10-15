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
