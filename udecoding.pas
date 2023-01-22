unit UDecoding;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  UBundle;

procedure DecodeFilesChunks(const FilesChunks: TFilesChunks; const SourceStream: TStream; const OutputPath: String);

implementation

uses
  {$IFDEF UNIX}
  UTF8Process,
  {$ENDIF}
  SysUtils,
  Math,
  UDecoder;

type
  // This class contains all file chunks that will be processed by the bundler threads.
  TChunksManager = class
  private
    FFilesChunks: TFilesChunks;
    FProcessedFilesChunkCount: ValSInt;
    FSourceStream: TStream;
    FCriticalSection: TRTLCriticalSection;
  public
    constructor Create(const FilesChunks: TFilesChunks; const SourceStream: TStream);
    destructor Destroy; override;
    function GetNextFilesChunk(out FileEntries: TFileEntries; out Stream: TStream): Boolean;
  end;

constructor TChunksManager.Create(const FilesChunks: TFilesChunks; const SourceStream: TStream);
begin
  inherited Create;
  FFilesChunks := FilesChunks;
  FProcessedFilesChunkCount := 0;
  FSourceStream := SourceStream;
  InitCriticalSection(FCriticalSection);
end;

destructor TChunksManager.Destroy;
begin
  DoneCriticalSection(FCriticalSection);
  inherited Destroy;
end;

function TChunksManager.GetNextFilesChunk(out FileEntries: TFileEntries; out Stream: TStream): Boolean;
begin
  Result := False;
  FileEntries := nil;
  Stream := nil;

  EnterCriticalSection(FCriticalSection);
  if FProcessedFilesChunkCount < Length(FFilesChunks) then
  begin
    // Read the chunk ID to get the correct file list.
    FileEntries := FFilesChunks[FSourceStream.ReadWord];

    Stream := TMemoryStream.Create;
    // Reads the size of the encoded block before copying the contents out of it into separate memory.
    Stream.CopyFrom(FSourceStream, FSourceStream.ReadQWord);

    Inc(FProcessedFilesChunkCount);
    Result := True;
  end;
  LeaveCriticalSection(FCriticalSection);
end;

type
  // The decoder thread gathers file chunks from the chunk manager and writes the decoded result directly on the disk.
  TDecoderThread = class(TThread)
  private
    FChunksManager: TChunksManager;
    FOutputPath: String;
  protected
    procedure Execute; override;
  public
    constructor Create(const ChunksManager: TChunksManager; const OutputPath: String);
  end;

constructor TDecoderThread.Create(const ChunksManager: TChunksManager; const OutputPath: String);
begin
  inherited Create(True);
  FChunksManager := ChunksManager;
  FOutputPath := OutputPath;
end;

procedure TDecoderThread.Execute;
var
  FileEntries: TFileEntries;
  FileEntry: TFileEntry;
  EncodedStream: TStream;
  DecodedStream: TStream;
  OutputStream: TStream;
  FileMode: Uint16;
  FullPath: String;
begin
  if not Assigned(FChunksManager) then
    Self.Terminate;

  while not Self.Terminated do
  begin
    // Get a stream of encoded data to process.
    if not FChunksManager.GetNextFilesChunk(FileEntries, EncodedStream) then
    begin
      Self.Terminate;
      Break;
    end;

    // Decode the data.
    DecodedStream := TMemoryStream.Create;
    EncodedStream.Position := 0;
    if Decode(EncodedStream, DecodedStream, nil) < 0 then
    begin
      EncodedStream.Free;
      DecodedStream.Free;
      Self.Terminate;
      Break;
    end;
    EncodedStream.Free;
    DecodedStream.Position := 0;

    // The decoded data block contains one or more files. Each of those must be now written back on the disk.
    for FileEntry in FileEntries do
    begin
      FullPath := FOutputPath + FileEntry.Path;
      if FileExists(FullPath) then
        FileMode := fmOpenWrite
      else
        FileMode := fmCreate;

      try
        // Write the part of the decoded data into individual files.
        OutputStream := TFileStream.Create(FullPath, FileMode);
        if OutputStream.CopyFrom(DecodedStream, FileEntry.Size) <> FileEntry.Size then
        begin
          Self.Terminate;
          Break;
        end;
      finally
        OutputStream.Free;
      end;
    end;
    DecodedStream.Free;
  end;
end;

procedure DecodeFilesChunks(const FilesChunks: TFilesChunks; const SourceStream: TStream; const OutputPath: String);
var
  ChunksManager: TChunksManager;
  Decoders: array of TDecoderThread;
  DecodersFinished: Boolean;
  Index: ValSInt;
  FileChunk: TFileEntries;
  FileEntry: TFileEntry;
begin
  // Create entire directory structure in advance.
  for FileChunk in FilesChunks do
    for FileEntry in FileChunk do
      if not ForceDirectories(OutputPath + ExtractFilePath(FileEntry.Path)) then
        Exit;

  // Prepare all threads and file chunks.
  ChunksManager := TChunksManager.Create(FilesChunks, SourceStream);

  Decoders := nil;
  // Use n-1 CPUs to leave one for other processes on the clients' computers.
  SetLength(Decoders, Math.Min({$IFDEF UNIX}GetSystemThreadCount{$ELSE}GetCPUCount{$ENDIF} - 1, Length(FilesChunks)));
  for Index := 0 to High(Decoders) do
  begin
    Decoders[Index] := TDecoderThread.Create(ChunksManager, OutputPath);
    // Let the games begin!
    Decoders[Index].Start;
  end;

  // Wait for all decoder threads to finish work by polling every now and then.
  repeat
    DecodersFinished := True;
    for Index := 0 to High(Decoders) do
      DecodersFinished := DecodersFinished and Decoders[Index].Finished;
    Sleep(50);
  until DecodersFinished;

  for Index := 0 to High(Decoders) do
  begin
    Decoders[Index].WaitFor;
    Decoders[Index].Free;
  end;

  ChunksManager.Free;
end;

end.
