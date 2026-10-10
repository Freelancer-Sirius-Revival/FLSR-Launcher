unit USettings;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils;

function WriteFlsrPathToConfig(const Path: String): Boolean;

implementation

uses
  IniFiles;


function WriteFlsrPathToConfig(const Path: String): Boolean;
var
  Ini: TIniFile = nil;
begin
  Result := False;
  try
    try
      Ini := TIniFile.Create(GetAppConfigDir(False) + 'settings.ini');
      Ini.WriteString('Installation', 'live', Path.Trim.TrimRight('/').TrimRight('\') + DirectorySeparator);
      Result := True;
    except
    end;
  finally
    if Assigned(Ini) then
      Ini.Free;
  end;
end;

end.

