unit UDecoding;

{$mode ObjFPC}
{$H+}
{$WriteableConst OFF}
{$ScopedEnums ON}
{$PACKENUM 4}

interface

uses
  Classes,
  UBundle;

type
  TDecodingProgressCallback = procedure(const BytesDecoded, TotalBytesWritten: Int64) of object;

  TDecoderErrorType = (None, Decoder, WritingOutputPath);
  TDecoderError = record
    ErrorType: TDecoderErrorType;
    Reason: String;
  end;
  TDecoderErrorArray = array of TDecoderError;

function DecodeFilesChunks(const FilesChunks: TFilesChunks; const SourceStream: TStream; const OutputPath: String; const Aborted: PLongBool; const OnDecodingProgress: TDecodingProgressCallback; out Errors: TDecoderErrorArray): Boolean;

implementation

uses
  {$IFDEF UNIX}
  UTF8Process,
  {$ENDIF}
  SysUtils,
  Math,
  ULZMACommon,
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
    FAborted: PLongBool;
    FChunksManager: TChunksManager;
    FOutputPath: String;
    FPreviousProgressData: Int64;
    FTotalBytesDecoded: Int64;
    FLastError: TDecoderError;
    procedure OnDecoderProcess(const Action: TLZMAProgressAction; const Value: Int64);
  protected
    procedure Execute; override;
  public
    // Thread-safe to read.
    property TotalBytesDecoded: Int64 read FTotalBytesDecoded;
    // Thread-safe to read.
    property LastErrorType: TDecoderErrorType read FLastError.ErrorType;
    // Not thread-safe to read.
    property LastError: TDecoderError read FLastError;
    constructor Create(const ChunksManager: TChunksManager; const OutputPath: String; const Aborted: PLongBool);
  end;

constructor TDecoderThread.Create(const ChunksManager: TChunksManager; const OutputPath: String; const Aborted: PLongBool);
begin
  inherited Create(True);
  FAborted := Aborted;
  FChunksManager := ChunksManager;
  FOutputPath := OutputPath;
  FTotalBytesDecoded := 0;
  FLastError.ErrorType := TDecoderErrorType.None;
end;

procedure TDecoderThread.OnDecoderProcess(const Action: TLZMAProgressAction; const Value: Int64);
begin
  if Action = LPAPos then
  begin
    InterlockedExchangeAdd64(FTotalBytesDecoded, Value - FPreviousProgressData);
    FPreviousProgressData := Value;
  end;
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
    Terminate;

  while not Terminated and not FAborted^ do
  begin
    // Get a stream of encoded data to process.
    if not FChunksManager.GetNextFilesChunk(FileEntries, EncodedStream) then
    begin
      Terminate;
      Break;
    end;

    // Decode the data.
    DecodedStream := TMemoryStream.Create;
    EncodedStream.Position := 0;
    FPreviousProgressData := 0;
    if Decode(EncodedStream, DecodedStream, @OnDecoderProcess, FAborted) < 0 then
    begin
      InterlockedExchange(Int32(FLastError.ErrorType), Int32(TDecoderErrorType.Decoder));
      FLastError.Reason := 'Chunk';
      EncodedStream.Free;
      DecodedStream.Free;
      Terminate;
      Break;
    end;
    EncodedStream.Free;
    DecodedStream.Position := 0;

    // The decoded data block contains one or more files. Each of those must be now written back on the disk.
    for FileEntry in FileEntries do
    begin
      if Terminated or FAborted^ then
        Break;

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
          InterlockedExchange(Int32(FLastError.ErrorType), Int32(TDecoderErrorType.WritingOutputPath));
          FLastError.Reason := FileEntry.Path;
          Terminate;
          Break;
        end;
      finally
        OutputStream.Free;
      end;
    end;
    DecodedStream.Free;
  end;
end;

var
  Decoders: array of TDecoderThread = nil;

function DecodeFilesChunks(const FilesChunks: TFilesChunks; const SourceStream: TStream; const OutputPath: String; const Aborted: PLongBool; const OnDecodingProgress: TDecodingProgressCallback; out Errors: TDecoderErrorArray): Boolean;
var
  ChunksManager: TChunksManager;
  DecodersFinished: Boolean = False;
  Index: ValSInt;
  FileChunk: TFileEntries;
  FileEntry: TFileEntry;
  TotalDecodedBytes: Int64 = 0;
  BytesDecoded: Int64 = 0;
  AbortDecoders: Boolean = False;
begin             
  Result := True;
  Errors := nil;
  if Length(FilesChunks) = 0 then
    Exit(True);

  if Assigned(Decoders) then
    Exit(False);

  // Create entire directory structure in advance.
  for FileChunk in FilesChunks do
    for FileEntry in FileChunk do
    begin
      TotalDecodedBytes += FileEntry.Size;
      if not ForceDirectories(OutputPath + ExtractFilePath(FileEntry.Path)) then
      begin
        SetLength(Errors, 1);
        Errors[0].ErrorType := TDecoderErrorType.WritingOutputPath;
        Errors[0].Reason := OutputPath + ExtractFilePath(FileEntry.Path);
        Exit(False);
      end;
    end;

  // Prepare all threads and file chunks.
  ChunksManager := TChunksManager.Create(FilesChunks, SourceStream);
  SetLength(Decoders, Math.Min(Length(FilesChunks), {$IFDEF UNIX}GetSystemThreadCount{$ELSE}GetCPUCount{$ENDIF}));
  for Index := 0 to High(Decoders) do
    Decoders[Index] := TDecoderThread.Create(ChunksManager, OutputPath, Aborted);
                               
  // Let the games begin!
  for Index := 0 to High(Decoders) do
    Decoders[Index].Start;

  // Wait for all decoder threads to finish work by polling every now and then.
  repeat
    BytesDecoded := 0;
    DecodersFinished := True;
    for Index := 0 to High(Decoders) do
    begin
      BytesDecoded := BytesDecoded + Decoders[Index].TotalBytesDecoded;
      DecodersFinished := DecodersFinished and Decoders[Index].Finished;

      // Stop all threads once there is a single error anywhere.
      if Decoders[Index].LastErrorType <> TDecoderErrorType.None then
        AbortDecoders := True;
    end;

    if Assigned(OnDecodingProgress) then
      OnDecodingProgress(TotalDecodedBytes, BytesDecoded);

    if AbortDecoders then       
      for Index := 0 to High(Decoders) do
        Decoders[Index].Terminate;

    Sleep(50);
  until DecodersFinished;

  for Index := 0 to High(Decoders) do
  begin
    Decoders[Index].WaitFor;
    if Decoders[Index].LastErrorType <> TDecoderErrorType.None then
    begin
      SetLength(Errors, Length(Errors) + 1);
      Errors[High(Errors)] := Decoders[Index].LastError;     
      Result := False;
    end;
    Decoders[Index].Free;
  end;

  ChunksManager.Free;
end;

end.
