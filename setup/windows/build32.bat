move ..\..\bin\NimeTravelDelphi.exe ..\..\bin\NimeTravel.exe
"C:\Program Files (x86)\NSIS\makensis.exe" NimeTravel.nsi

set ZipName=NimeTravel-prologue-1.0.0.2-Win32

mkdir M:\%ZipName%
mkdir M:\%ZipName%\fonts
mkdir M:\%ZipName%\images
mkdir M:\%ZipName%\music
mkdir M:\%ZipName%\scenes
mkdir M:\%ZipName%\scripts
mkdir M:\%ZipName%\sounds
mkdir M:\%ZipName%\text

xcopy /S /Y ..\..\bin\fonts M:\%ZipName%\fonts
xcopy /S /Y ..\..\bin\images M:\%ZipName%\images
xcopy /S /Y ..\..\bin\music M:\%ZipName%\music
xcopy /S /Y ..\..\bin\scenes M:\%ZipName%\scenes
xcopy /S /Y ..\..\bin\scripts M:\%ZipName%\scripts
xcopy /S /Y ..\..\bin\sounds M:\%ZipName%\sounds
xcopy /S /Y ..\..\bin\text M:\%ZipName%\text
copy ..\..\bin\*.dll M:\%ZipName%
copy ..\..\bin\NimeTravel.exe M:\%ZipName%

M:
cd M:\
7z a -mx9 %ZipName%.zip %ZipName%
rmdir /S /Q M:\%ZipName%

pause

