unit CommonData;

interface

uses
  Classes, SysUtils,
  SfmlSystem,SfmlWindow,SfmlGraphics, SfmlAudio,
  Profile, Texts, Languages ;

type

  { TCommonData }

  TCommonData = class
  private
    class var profile:TProfile ;
    class var musiccode:string ;
  public
    class var markercolors:array of TSfmlColor ;

    class var font:TSfmlFont ;
    class var texts:TTexts ;
    class var languages:TLanguages ;
    class var music:TSfmlMusic ;
    class var credits:string ;
    class var intro:TSFMLSprite ;
    class var grayrect:TSfmlRectangleShape ;
    const INTRO_MUSIC = 'music_main.ogg' ;
    class function Init(window_w,window_h:Integer):Boolean ;
    class procedure reloadTexts() ;
    class procedure UnInit() ;
    class procedure updateMusicVolume() ;
    class procedure setProfile(Aprofile:TProfile) ;
    class procedure LoadMusicIfNew(Amusiccode:string) ;
  end;

implementation
uses SfmlUtils, Helpers, Scene, Math ;

{ TCommonData }

class function TCommonData.Init(window_w,window_h:Integer):Boolean ;
begin
  font:=TSfmlFont.Create('fonts'+PATH_SEP+'arial.ttf');
  languages:=TLanguages.Create() ;
  languages.loadFromFile('text'+PATH_SEP+'languages');
  languages.setCurrentByFile('text'+PATH_SEP+'default');
  texts:=TTexts.Create() ;
  reloadTexts() ;
  musiccode:='' ;
  intro:=LoadSprite('images'+PATH_SEP+'intro.png') ;

  grayrect:=TSfmlRectangleShape.Create() ;
  grayrect.Position:=SfmlVector2f(0,0);
  grayrect.FillColor:=SfmlColorFromRGBA(0,0,0,128) ;
  grayrect.OutlineThickness:=0;
  grayrect.Size:=SfmlVector2f(window_w,500) ;

  SetLength(markercolors,10) ;
  markercolors[0]:=createSFMLColor($00FF00) ;
  markercolors[1]:=createSFMLColor($0000FF) ;
  markercolors[2]:=createSFMLColor($FF0000) ;
  markercolors[3]:=createSFMLColor($FFFF00) ;
  markercolors[4]:=createSFMLColor($FF00FF) ;
  markercolors[5]:=createSFMLColor($00FFFF) ;
  markercolors[6]:=createSFMLColor($FFFFFF) ;
  markercolors[7]:=createSFMLColor($FFFFFF) ;
  markercolors[8]:=createSFMLColor($FFFFFF) ;
  markercolors[9]:=createSFMLColor($FFFFFF) ;

  Result:=True ;
end ;

class procedure TCommonData.LoadMusicIfNew(Amusiccode: string);
begin
  if musiccode=Amusiccode then Exit ;
  musiccode:=Amusiccode ;

  if music<>nil then music.Free ;
  music:=TSFMLMusic.Create('music'+PATH_SEP+musiccode) ;
  music.Loop:=True ;
  updateMusicVolume() ;
  music.Play() ;
end;

class procedure TCommonData.reloadTexts;
begin
  texts.loadFromFile('text'+PATH_SEP+'texts.'+languages.getCurrent()) ;
  credits:=Helpers.readAllText('text'+PATH_SEP+'credits.'+languages.getCurrent()) ;
  // Fix for working primitive procedure TScene.drawTextInBlockWidth
  credits:=credits.Replace(#13,'') ;
  credits:=credits.Replace(#10,#10' ') ;
end;

class procedure TCommonData.setProfile(Aprofile: TProfile);
begin
  profile:=Aprofile ;
end;

class procedure TCommonData.UnInit() ;
begin
  font.Free ;
  texts.Free ;
  languages.Free ;
  intro.Free ;
  grayrect.Free ;
end ;

class procedure TCommonData.updateMusicVolume();
begin
  if music<>nil then music.Volume:=IfThen(profile.isMusicOn(),100,0) ;
end;

end.

