unit UDataSizes;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils;


function GetFreeDiskSpace(const FileName: String): Int64; 
function GetFileSize(const FileName: String): Int64;

implementation

function GetFreeDiskSpace(const FileName: String): Int64;
begin
  Result := -1;
  {$IfDef linux}
    Result := SysUtils.DiskFree(SysUtils.AddDisk(ExtractFileDir(FileName)));
  {$EndIf}

  {$IfDef windows}
    Result:=SysUtils.DiskFree(SysUtils.GetDriveIDFromLetter(ExtractFileDrive(FileName)));
  {$EndIf}
end;

function GetFileSize(const FileName: String): Int64;
var
  Info: TSearchRec;
begin
  if FindFirst(FileName, faAnyFile, Info) = 0 then
    Result := Info.Size
  else
    Result := -1;
  FindClose(Info);
end;

end.

