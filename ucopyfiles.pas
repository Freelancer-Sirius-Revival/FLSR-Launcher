unit UCopyFiles;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils;

type
  TCopyFilesProgressCallback = procedure(const FileCount, FilesCopied: Int64) of object;

function CopyFiles(const SourceBasePath, TargetBasePath: String; const RelativeFilePaths: TStrings; const Aborted: PLongBool; const OnCopyProgress: TCopyFilesProgressCallback; out ErrorPath: String): Boolean;

implementation

uses
  FileUtil;

function CopyFiles(const SourceBasePath, TargetBasePath: String; const RelativeFilePaths: TStrings; const Aborted: PLongBool; const OnCopyProgress: TCopyFilesProgressCallback; out ErrorPath: String): Boolean;
var
  Index: ValSInt;
begin
  Result := True;
  ErrorPath := '';
  for Index := 0 to RelativeFilePaths.Count - 1 do
  begin
    if Aborted^ then
      Exit(False);

    if CopyFile(SourceBasePath + RelativeFilePaths.Strings[Index], TargetBasePath + RelativeFilePaths.Strings[Index], [cffOverwriteFile, cffCreateDestDirectory, cffPreserveTime]) then
    begin
      if Assigned(OnCopyProgress) then
        OnCopyProgress(RelativeFilePaths.Count, Index);
    end
    else
    begin
      ErrorPath := RelativeFilePaths.Strings[Index];
      Exit(False);
    end;
  end;
end;

end.
