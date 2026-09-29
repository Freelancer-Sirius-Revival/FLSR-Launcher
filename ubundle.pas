unit UBundle;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  md5;

const
  FlsrFileVersion = 1;
  FlsrFileMagicNumbers = 'FLSR';
  FullBundleFileName = 'release';
  UpdateBundleFileName = 'update';
  BundleFileExtension = '.flsr';

type
  TFileEntry = record
    Path: String;
    Size: Int64;
    Checksum: TMD5Digest;
  end;
  TFileEntries = array of TFileEntry;
  TFilesChunks = array of TFileEntries;

  TBundleType = (TFullBundle = 0, TUpdateBundle = 1, TUnknownBundle = 255);

  TBundle = record
    ContentVersion: Uint32;
    BundleType: TBundleType;
    FilesChunks: TFilesChunks;
  end;

function IsMatchingBundleVersion(const BundleStream: TStream; const ExpectedContentVersion: Uint32; const ExpectedBundleType: TBundleType): Boolean;
function ReadBundleMetaData(const Stream: TStream): TBundle;

implementation

uses
  SysUtils;

function IsMatchingBundleVersion(const BundleStream: TStream; const ExpectedContentVersion: Uint32; const ExpectedBundleType: TBundleType): Boolean;
var
  MagicNumbers: array [0..3] of Char;
  ChunkIndex: ValSInt;
  ChunkFileIndex: ValSInt;
begin
  if BundleStream.Position + SizeOf(MagicNumbers) + SizeOf(Uint8) + SizeOf(Uint32) + SizeOf(TBundleType) > BundleStream.Size then
    Exit(False);
  // Magic number of file format.
  BundleStream.Read(MagicNumbers, SizeOf(FlsrFileMagicNumbers));
  if CompareByte(MagicNumbers, FlsrFileMagicNumbers, SizeOf(MagicNumbers)) <> 0 then
    Exit(False);
  // Version of file format.
  if BundleStream.ReadByte <> FlsrFileVersion then
    Exit(False);
  // Version of file contents.
  if BundleStream.ReadDWord <> ExpectedContentVersion then
    Exit(False);
  // Bundle type.
  if TBundleType(BundleStream.ReadByte) <> ExpectedBundleType then
    Exit(False);

  Result := True;
end;

function ReadBundleMetaData(const Stream: TStream): TBundle;
var
  MagicNumbers: array [0..3] of Char;
  ChunkIndex: ValSInt;
  ChunkFileIndex: ValSInt;
begin
  Result.ContentVersion := High(Uint32);
  Result.BundleType := TUnknownBundle;
  Result.FilesChunks := nil;

  // Magic number of file format.
  Stream.Read(MagicNumbers, SizeOf(FlsrFileMagicNumbers));
  if (Length(MagicNumbers) = Length(FlsrFileMagicNumbers)) and (CompareByte(MagicNumbers, FlsrFileMagicNumbers, SizeOf(MagicNumbers)) <> 0) then
    Exit;
  // Version of file format.
  if Stream.ReadByte <> FlsrFileVersion then
    Exit;
  // Version of file contents.
  Result.ContentVersion := Stream.ReadDWord;
  // Bundle type.
  Result.BundleType := TBundleType(Stream.ReadByte);
  // Count of Chunks.
  SetLength(Result.FilesChunks, Stream.ReadWord);

  for ChunkIndex := 0 to High(Result.FilesChunks) do
  begin
    // Count of files in this chunk.
    SetLength(Result.FilesChunks[ChunkIndex], Stream.ReadDWord);

    for ChunkFileIndex := 0 to High(Result.FilesChunks[ChunkIndex]) do
    begin
      // First read a normalized relative path of the file.
      Result.FilesChunks[ChunkIndex][ChunkFileIndex].Path := Stream.ReadAnsiString;
      // Second read the uncompressed size of the file.
      Result.FilesChunks[ChunkIndex][ChunkFileIndex].Size := Stream.ReadQWord;
      // Third read an MD5 hash of the file's contents.
      Stream.Read(Result.FilesChunks[ChunkIndex][ChunkFileIndex].Checksum, SizeOf(TMD5Digest));
    end;
  end;
end;

end.
