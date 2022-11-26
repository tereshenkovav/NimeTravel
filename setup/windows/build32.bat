move ..\..\bin\NimeTravelDelphi.exe ..\..\bin\NimeTravel.exe
"C:\Program Files (x86)\NSIS\makensis.exe" /DGAMELANG=ru /DUPPERLANG=RU NimeTravel.nsi
"C:\Program Files (x86)\NSIS\makensis.exe" /DGAMELANG=en /DUPPERLANG=EN NimeTravel.nsi

call create_zip32.bat ru RU
call create_zip32.bat en EN
