unit UBundle;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  md5;

const
  FlsrFileVersion = 1;
  FlsrFileMagicNumbers = 'FLSR';
  FullBundleFileName = 'main';
  UpdateBundleFileName = 'update';
  BundleFileExtension = '.flsr';
  ChecksumFileExtension = '.md5';

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

function ReadBundleMetaData(const Stream: TStream): TBundle;

implementation

uses
  SysUtils;

procedure WriteMetaData(const ContentVersion: Uint32; const BundleType: TBundleType; const FilesChunks: TFilesChunks; const BasePath: String; const Stream: TStream);
var
  FilesChunk: TFileInfoArray;
  ChunkFile: TFileInfo;
begin
  // Magic number of file format.
  Stream.Write(FlsrFileMagicNumbers, SizeOf(FlsrFileMagicNumbers));

  // Version of file format.
  Stream.WriteByte(FlsrFileVersion);

  // Version of file contents.
  Stream.WriteDWord(ContentVersion);

  // Bundle type.
  Stream.WriteByte(Ord(BundleType));

  // Count of Chunks.
  Stream.WriteWord(Length(FilesChunks));

  for FilesChunk in FilesChunks do
  begin
    // Count of files in this chunk.
    Stream.WriteDWord(Length(FilesChunk));

    for ChunkFile in FilesChunk do
    begin
      // First write a normalized relative path of the file.
      Stream.WriteAnsiString(ChunkFile.Path.Remove(0, BasePath.Length).Trim.Replace('\', '/'));
      // Second write the uncompressed size of the file.
      Stream.WriteQWord(ChunkFile.Size);
      // Third write an MD5 hash of the file's contents.
      Stream.Write(MD5File(ChunkFile.Path), SizeOf(TMD5Digest));
    end;
  end;
end;

function ReadBundleMetaData(const Stream: TStream): TBundle;
var
  MagicNumbers: array [0..3] of Char;
  ChunkIndex: ValSInt;
  ChunkFileIndex: ValSInt;
begin
  Result.ContentVersion := -1;
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
  ;

  // Count of Chunks.
  SetLength(Result.FilesChunks, Stream.ReadWord);

  for ChunkIndex := 0 to High(Result.FilesChunks) do
  begin
    // Count of files in this chunk.
    SetLength(Result.FilesChunks[ChunkFileIndex], Stream.ReadDWord);

    for ChunkFileIndex := 0 to High(esult.FilesChunks[ChunkFileIndex]) do
    begin
      // First read a normalized relative path of the file.
      Result.FilesChunks[ChunkFileIndex][ChunkFileIndex].Path := Stream.ReadAnsiString;
      // Second read the uncompressed size of the file.
      Result.FilesChunks[ChunkFileIndex][ChunkFileIndex].Size := Stream.ReadQWord;
      // Third read an MD5 hash of the file's contents.
      Stream.Read(Result.FilesChunks[ChunkFileIndex][ChunkFileIndex].Checksum, SizeOf(TMD5Digest));
    end;
  end;
end;

end.
