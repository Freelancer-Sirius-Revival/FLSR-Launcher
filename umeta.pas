unit UMeta;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  UBundle;

const
  MetaFileExtension = '.meta';

type
  TBundleMeta = record
    BundleType: TBundleType;
    ContentVersion: Uint32;
    BundleFileSize: Int64;
    FileEntries: TFileEntries;
  end;

function ReadMetaFile(const Stream: TStream): TBundleMeta;

implementation

uses
  SysUtils,
  md5;

function ReadMetaFile(const Stream: TStream): TBundleMeta;
var
  Index: ValSInt;
begin
  Result.BundleType := TBundleType.TUnknownBundle;
  Result.ContentVersion := High(Uint32);
  Result.BundleFileSize := -1;
  Result.FileEntries := nil;
  if Assigned(Stream) then
  begin
    // Version of file contents.
    Result.ContentVersion := Stream.ReadDWord;
    // Bundle type.
    Result.BundleType := TBundleType(Stream.ReadByte);
    // Total size of the compressed bundle.
    Result.BundleFileSize := Stream.ReadQWord;
    // Count of files in this bundle.
    SetLength(Result.FileEntries, Stream.ReadDWord);

    for Index := 0 to High(Result.FileEntries) do
    begin
      // First read a normalized relative path of the file.
      Result.FileEntries[Index].Path := Stream.ReadAnsiString;
      // Second read the uncompressed size of the file.
      Result.FileEntries[Index].Size := Stream.ReadQWord;
      // Third read an MD5 hash of the file's contents.
      Stream.Read(Result.FileEntries[Index].Checksum, SizeOf(TMD5Digest));
    end;
  end;
end;

end.
