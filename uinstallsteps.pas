unit UInstallSteps;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  UBundle;

function ValidateOriginalFreelancer(const Path: String): Boolean;
function ValidateCanCreateFiles(const Path: String): Boolean;
function FindOriginalFilesToCopy(FreelancerPath: String; const BundleFiles: TFileEntries): TStringList;
function EvaluateMissingDiskSpace(const FreelancerFiles: TStringList; const TargetPath: String; const BundleFiles: TFileEntries): Int64;

implementation

uses
  SysUtils,
  Math,
  FileUtil,
  UDataSizes;

function ValidateOriginalFreelancer(const Path: String): Boolean;
begin
  Result := FileExists(Path + '/EXE/Freelancer.exe');
end;

function ValidateCanCreateFiles(const Path: String): Boolean;
begin
  Result := (FileCreate(Path + '/accesstest') <> THandle(-1)) and DeleteFile(Path + '/accesstest');
end;

function FindOriginalFilesToCopy(FreelancerPath: String; const BundleFiles: TFileEntries): TStringList;
var
  Index: ValSInt;
  FoundIndex: Integer;
begin
  FreelancerPath := FreelancerPath.TrimRight('/').TrimRight('\');
  Result := FindAllFiles(FreelancerPath, AllFilesMask, True, faDirectory or faHidden or faReadOnly);
  for Index := 0 to Result.Count - 1 do
    Result.Strings[Index] := Result.Strings[Index].Remove(0, FreelancerPath.Length);
  Result.Sorted := True;
  // Remove original files from list if they are going to be replaced by bundle files.
  for Index := 0 to High(BundleFiles) do
    if Result.Find(BundleFiles[Index].Path, FoundIndex) then
      Result.Delete(FoundIndex);
end;

function EvaluateMissingDiskSpace(const FreelancerFiles: TStringList; const TargetPath: String; const BundleFiles: TFileEntries): Int64;
var
  Index: ValSInt;
begin
  // Collect the total disk usage requirement based on mod files and remaining original files.
  Result := 0;
  for Index := 0 to High(BundleFiles) do
    Result += BundleFiles[Index].Size;
  for Index := 0 to FreelancerFiles.Count - 1 do
    Result += GetFileSize(FreelancerFiles.Strings[Index]);

  Result := Max(0, Result - GetFreeDiskSpace(TargetPath));
end;

end.
