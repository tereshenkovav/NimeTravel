move ..\..\bin\NimeTravelDelphi.exe ..\..\bin\NimeTravel.exe

@echo off
for /F %%i in ('git tag --list --sort=committerdate') do set BUILDTAG=%%i
for /F %%i in ('git rev-parse HEAD') do set BUILDCOMMIT=%%i
set BUILDCOMMIT=%BUILDCOMMIT:~0,8%
for /F %%i in ('git branch --show-current') do set BUILDBRANCH=%%i

echo %BUILDTAG% %BUILDCOMMIT% %BUILDBRANCH%

echo %BUILDTAG%> ..\..\bin\text\version.txt
echo %BUILDCOMMIT%>> ..\..\bin\text\version.txt
echo %BUILDBRANCH%>> ..\..\bin\text\version.txt

del ..\..\bin\text\default

SET VERSION=%BUILDTAG:~1%
"C:\Program Files (x86)\NSIS\makensis.exe" /DVERSION=%VERSION% /DGAMELANG=ru /DUPPERLANG=RU NimeTravel.nsi
"C:\Program Files (x86)\NSIS\makensis.exe" /DVERSION=%VERSION% /DGAMELANG=en /DUPPERLANG=EN NimeTravel.nsi

SmartZipBuilder.exe script.szb /LANGL=ru /LANGH=RU
SmartZipBuilder.exe script.szb /LANGL=en /LANGH=EN
