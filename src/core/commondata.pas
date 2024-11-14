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
  public
    class var font:TSfmlFont ;
    class var texts:TTexts ;
    class var languages:TLanguages ;
    class var music:TSfmlMusic ;
    const INTRO_MUSIC = 'music_main.ogg' ;
    class function Init():Boolean ;
    class procedure reloadTexts() ;
    class procedure UnInit() ;
    class procedure updateMusicVolume() ;
    class procedure setProfile(Aprofile:TProfile) ;
    class procedure LoadMusic(musicname:string) ;
  end;

implementation
uses SfmlUtils, Helpers, Scene, Math ;

{ TCommonData }

class function TCommonData.Init():Boolean ;
begin
  font:=TSfmlFont.Create('fonts'+PATH_SEP+'arial.ttf');
  languages:=TLanguages.Create() ;
  languages.loadFromFile('text'+PATH_SEP+'languages');
  languages.setCurrentByFile('text'+PATH_SEP+'default');
  texts:=TTexts.Create() ;
  reloadTexts() ;
  Result:=True ;
end ;

class procedure TCommonData.LoadMusic(musicname: string);
begin
  if music<>nil then music.Free ;
  music:=TSFMLMusic.Create('music'+PATH_SEP+musicname) ;
  music.Loop:=True ;
  updateMusicVolume() ;
  music.Play() ;
end;

class procedure TCommonData.reloadTexts;
begin
  texts.loadFromFile('text'+PATH_SEP+'texts.'+languages.getCurrent()) ;
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
end ;

class procedure TCommonData.updateMusicVolume();
begin
  if music<>nil then music.Volume:=IfThen(profile.isMusicOn(),100,0) ;
end;

end.

