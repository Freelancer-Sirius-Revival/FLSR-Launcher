unit USettings;

{$mode ObjFPC}
{$H+}
{$WriteableConst OFF}

interface

type
  TSettings = record
    LivePath: String;
  end;

function ReadSettings: Boolean;
function GetSettings: TSettings;
function WriteFlsrPathToConfig(const Path: String): Boolean;

implementation

uses
  SysUtils,
  Classes,
  IniFiles;

const
  SettingsFileName: String = 'settings.ini';
  InstallationSectionName: String = 'Installation';
  LivePathKey: String = 'live';

var
  Settings: TSettings;

function GetSettingsFileName: String;
begin
  Result := GetAppConfigDir(False) + SettingsFileName;
end;

function ReadSettings: Boolean;
var
  Ini: TIniFile = nil;
begin       
  Result := False;
  try
    try
      Ini := TIniFile.Create(GetSettingsFileName);
      Settings.LivePath := Ini.ReadString(InstallationSectionName, LivePathKey, '').Trim;
      Result := True;
    except
    end;
  finally
    if Assigned(Ini) then
      Ini.Free;
  end;
end;

function GetSettings: TSettings;
begin
  Result := Settings;
end;

function WriteFlsrPathToConfig(const Path: String): Boolean;
var
  Ini: TIniFile = nil;
begin
  Settings.LivePath := Path;
  Result := False;
  try
    try
      Ini := TIniFile.Create(GetSettingsFileName);
      Ini.WriteString(InstallationSectionName, LivePathKey, Path.Trim.TrimRight('/').TrimRight('\') + DirectorySeparator);
      Result := True;
    except
    end;
  finally
    if Assigned(Ini) then
      Ini.Free;
  end;
end;

initialization
  Settings.LivePath := '';

end.

