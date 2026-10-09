unit UInstallSteps;

{$mode ObjFPC}
{$H+}               
{$WriteableConst OFF}

interface

uses
  Classes,
  UBundle;

function ValidateOriginalFreelancer(Path: String): Boolean;
function ValidateCanCreateFiles(Path: String): Boolean;
function FindOriginalFilesToCopy(FreelancerPath: String; const BundleFiles: TFileEntries): TStringList;
function EvaluateMissingDiskSpace(const FreelancerFiles: TStrings; const TargetPath: String; const BundleFiles: TFileEntries; const BundleFileSize: Int64): Int64;

implementation

uses
  SysUtils,
  Math,
  FileUtil,
  md5,
  UDataSizes;

function ValidateOriginalFreelancer(Path: String): Boolean;     
const                                                                                                       
  FreelancerIniChecksum: TMD5Digest = ($67, $9E, $59, $8A, $5E, $78, $B7, $6A, $7D, $7E, $09, $1E, $E5, $F3, $DA, $9A);
  DacomIniChecksum: TMD5Digest = ($51, $EB, $D5, $D1, $A3, $17, $63, $08, $4F, $F2, $3C, $75, $9D, $B2, $77, $1C);
begin
  Path := Path.TrimRight('/').TrimRight('\').Trim + DirectorySeparator + 'EXE' + DirectorySeparator;
  // Freelancer.exe to verify the game is actually installed there.
  // freelancer.ini and dacom.ini being checked, assuming any bigger mod modifies these files. Smaller mods most likely will be overwritten by FL:SR's own data anyway.
  Result := FileExists(Path + 'Freelancer.exe') and FileExists(Path + 'freelancer.ini') and FileExists(Path + 'dacom.ini') and MD5Match(FreelancerIniChecksum, MD5File(Path + 'freelancer.ini'))  and MD5Match(DacomIniChecksum, MD5File(Path + 'dacom.ini'));
end;

function ValidateCanCreateFiles(Path: String): Boolean;
begin
  Path := Path.TrimRight('/').TrimRight('\').Trim;
  Result := ForceDirectories(Path) and (FileCreate(Path + '/accesstest') <> THandle(-1)) and DeleteFile(Path + '/accesstest');
end;

function FindOriginalFilesToCopy(FreelancerPath: String; const BundleFiles: TFileEntries): TStringList;
var
  Index: ValSInt;
  FoundIndex: Integer;
begin
  FreelancerPath := FreelancerPath.TrimRight('/').TrimRight('\').Trim;
  Result := FindAllFiles(FreelancerPath, AllFilesMask, True, faDirectory or faHidden or faReadOnly);
  for Index := 0 to Result.Count - 1 do
    Result.Strings[Index] := Result.Strings[Index].Remove(0, FreelancerPath.Length);
  Result.Sorted := True;
  // Remove original files from list if they are going to be replaced by bundle files.
  for Index := 0 to High(BundleFiles) do
    if Result.Find(BundleFiles[Index].Path, FoundIndex) then
      Result.Delete(FoundIndex);
end;

function EvaluateMissingDiskSpace(const FreelancerFiles: TStrings; const TargetPath: String; const BundleFiles: TFileEntries; const BundleFileSize: Int64): Int64;
var
  Index: ValSInt;
begin
  // Collect the total disk usage requirement based on mod files and remaining original files.
  Result := BundleFileSize;
  for Index := 0 to High(BundleFiles) do
    Result += BundleFiles[Index].Size;
  for Index := 0 to FreelancerFiles.Count - 1 do
    Result += GetFileSize(FreelancerFiles.Strings[Index]);

  Result := Max(0, Result - GetFreeDiskSpace(TargetPath));
end;

end.
