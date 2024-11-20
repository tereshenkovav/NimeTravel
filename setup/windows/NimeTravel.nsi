!ifndef GAMELANG
  !error "Value of GAMELANG not defined"
!endif

Unicode True
RequestExecutionLevel admin
SetCompressor /SOLID zlib
AutoCloseWindow true
Icon ..\..\graphics\main.ico
XPStyle on

!include LangData_${GAMELANG}.nsi

!include "FileFunc.nsh"
!insertmacro GetTime

!define TEMP1 $R0 

ReserveFile /plugin InstallOptions.dll
ReserveFile "runapp_${GAMELANG}.ini"

OutFile "NimeTravel-${UPPERLANG}-${VERSION}-Win32.exe"

Page directory
Page components
Page instfiles
Page custom SetRunApp ValidateRunApp "$(AfterParams)" 

UninstPage uninstConfirm
UninstPage instfiles

Name $(GameGameName)

Function .onInit
  InitPluginsDir
  File /oname=$PLUGINSDIR\runapp_${GAMELANG}.ini "runapp_${GAMELANG}.ini"

  StrCpy $INSTDIR $PROGRAMFILES\NimeTravel
FunctionEnd

Function .onInstSuccess
  ReadINIStr ${TEMP1} "$PLUGINSDIR\runapp_${GAMELANG}.ini" "Field 1" "State"
  StrCmp ${TEMP1} "0" SkipDesktop

  SetOutPath $INSTDIR
  CreateShortCut "$DESKTOP\$(GameName).lnk" "$INSTDIR\NimeTravel.exe" "" 

SkipDesktop:

  ReadINIStr ${TEMP1} "$PLUGINSDIR\runapp_${GAMELANG}.ini" "Field 2" "State"
  StrCmp ${TEMP1} "0" SkipRun

  Exec $INSTDIR\NimeTravel.exe

  SkipRun:
FunctionEnd

Function un.onUninstSuccess
  MessageBox MB_OK "$(MsgUninstOK)"
FunctionEnd

Function un.onUninstFailed
  MessageBox MB_OK "$(MsgUninstError)"
FunctionEnd

Function .onInstFailed
  MessageBox MB_OK "$(MsgInstError)"
FunctionEnd

Section "$(GamePrologName)"
  SectionIn RO

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

  FileOpen $0 "$INSTDIR\text\default" w
  FileWrite $0 ${GAMELANG}
  FileClose $0

  WriteUninstaller $INSTDIR\Uninst.exe

  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\NimeTravel" \
                 "DisplayName" "$(GameGameName)"
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
                 "Publisher"  "$(PublisherName)"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\NimeTravel" \
                 "DisplayVersion"  "${VERSION}"

  SetOutPath $INSTDIR
  CreateDirectory "$SMPROGRAMS\$(GameName)"
  CreateShortCut "$SMPROGRAMS\$(GameName)\$(GameName).lnk" "$INSTDIR\NimeTravel.exe" "" 

SectionEnd

Section "Uninstall"
  RMDir /r $INSTDIR
  RMDir /r "$SMPROGRAMS\$(GameName)"
  Delete "$DESKTOP\$(GameName).lnk"

  DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\NimeTravel"
SectionEnd

Function SetRunApp

  Push ${TEMP1}

  InstallOptions::dialog "$PLUGINSDIR\runapp_${GAMELANG}.ini"
    Pop ${TEMP1}
  
  Pop ${TEMP1}

FunctionEnd

Function ValidateRunApp

FunctionEnd
