Unicode True
RequestExecutionLevel admin
SetCompressor /SOLID zlib
LoadLanguageFile "${NSISDIR}\Contrib\Language files\Russian.nlf"
AutoCloseWindow true
Icon ..\..\graphics\main.ico
XPStyle on

;LicenseData ..\docs\license.rtf
;LicenseForceSelection off

!include "FileFunc.nsh"
!insertmacro GetTime

!define TEMP1 $R0 ;Temp variable

ReserveFile /plugin InstallOptions.dll
ReserveFile "runapp.ini"

; Результирующий файл
!ifdef updatemode
OutFile "M:\NimeTravel-prologue-1.0.0-Win32-update.exe"
!else
OutFile "M:\NimeTravel-prologue-1.0.0-Win32.exe"
!endif

var is_update

;Page license
Page directory
!ifndef updatemode
Page components
!endif
Page instfiles
Page custom SetRunApp ValidateRunApp ": успешно" 

UninstPage uninstConfirm
UninstPage instfiles

Name "Игра Путешествие Нимэ"

; Страницы

Function .onInit

  InitPluginsDir
  File /oname=$PLUGINSDIR\runapp.ini "runapp.ini"

  StrCpy $INSTDIR $PROGRAMFILES\NimeTravel

  IfFileExists $INSTDIR\NimeTravel.exe +3
  StrCpy $is_update "0"
  Goto +2
  StrCpy $is_update "1"

FunctionEnd

Function .onInstSuccess
  StrCmp $is_update "1" SkipAll

  ReadINIStr ${TEMP1} "$PLUGINSDIR\runapp.ini" "Field 1" "State"
  StrCmp ${TEMP1} "0" SkipDesktop

  SetOutPath $INSTDIR
  CreateShortCut "$DESKTOP\Путешествие Нимэ.lnk" "$INSTDIR\NimeTravel.exe" "" 

SkipDesktop:

  ReadINIStr ${TEMP1} "$PLUGINSDIR\runapp.ini" "Field 2" "State"
  StrCmp ${TEMP1} "0" SkipRun

  Exec $INSTDIR\NimeTravel.exe

  SkipRun:
  SkipAll:

FunctionEnd

Function un.onUninstSuccess
  MessageBox MB_OK "Игра Путешествие Нимэ успешно удалена!"
FunctionEnd

Function un.onUninstFailed
  MessageBox MB_OK "Ошибка удаления игры Путешествие Нимэ!"
FunctionEnd

Function .onInstFailed
  MessageBox MB_OK "Ошибка установки игры Путешествие Нимэ!"
FunctionEnd

Section "Игра Путешествие Нимэ (пролог)"
  SectionIn RO

  StrCmp $is_update "0" SkipSleep
  Sleep 3000
  SkipSleep:

  SetOutPath $INSTDIR
  File ..\..\bin\*.dll
  File ..\..\bin\NimeTravel.exe
  File ..\..\graphics\main.ico

  SetOutPath $INSTDIR\fonts
  File /r ..\..\bin\fonts\*
  SetOutPath $INSTDIR\images
  File /r ..\..\bin\images\*
  SetOutPath $INSTDIR\music
  File /r ..\..\bin\music\*
  SetOutPath $INSTDIR\scenes
  File /r ..\..\bin\scenes\*
  SetOutPath $INSTDIR\scripts
  File /r ..\..\bin\scripts\*
  SetOutPath $INSTDIR\sounds
  File /r ..\..\bin\sounds\*
  SetOutPath $INSTDIR\text
  File /r ..\..\bin\text\*

  StrCmp $is_update "1" Skip2

;  CreateDirectory $LOCALAPPDATA\NimeTravel
  
  WriteUninstaller $INSTDIR\Uninst.exe

  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\NimeTravel" \
                 "DisplayName" "Игра Путешествие Нимэ"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\NimeTravel" \
                 "UninstallString" "$\"$INSTDIR\Uninst.exe$\""
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\NimeTravel" \
                 "EstimatedSize" 0x00003200
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\NimeTravel" \
                 "DisplayIcon" $INSTDIR\main.ico

  ${GetTime} "" "L" $0 $1 $2 $3 $4 $5 $6
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\NimeTravel" \
                 "InstallDate"  "$2$1$0"

  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\NimeTravel" \
                 "Publisher"  "Терешенков А.В."
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\NimeTravel" \
                 "DisplayVersion"  "1.0.0"

  SetOutPath $INSTDIR
  CreateDirectory "$SMPROGRAMS\Путешествие Нимэ"
  CreateShortCut "$SMPROGRAMS\Путешествие Нимэ\Путешествие Нимэ.lnk" "$INSTDIR\NimeTravel.exe" "" 

Skip2:

SectionEnd

Section "Uninstall"
  RMDir /r $INSTDIR
  ;RMDir /r $LOCALAPPDATA\NimeTravel
  RMDir /r "$SMPROGRAMS\Путешествие Нимэ"
  Delete "$DESKTOP\Путешествие Нимэ.lnk"

  DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\NimeTravel"
SectionEnd

Function SetRunApp

  Push ${TEMP1}

  InstallOptions::dialog "$PLUGINSDIR\runapp.ini"
    Pop ${TEMP1}
  
  Pop ${TEMP1}

FunctionEnd

Function ValidateRunApp

FunctionEnd
