unit UBundleDownload;

{$mode ObjFPC}{$H+}

interface

uses
  UProgress,
  UMeta;

function DownloadModData(const GamePath: String): TProcessProgress;
function DownloadMetaData(var Meta: TBundleMeta): TProcessProgress;
procedure AbortDownloads;

implementation

uses
  Classes,
  SysUtils,
  fphttpclient,
  UBundle,
  md5,
  FileUtil,
  UDataSizes;

const
  URL = 'https://fl-sr.eu/files/';

procedure GetAvailableFiles(out Response: TStringList);
var
  HttpClient: TFPHttpClient;
begin
  Response := TStringList.Create;
  try
    try
      HttpClient := TFPHttpClient.Create(nil);
      HttpClient.Get(URL, Response);
    except
      Response.Free;
      Response := nil;
    end;
  finally
    HttpClient.Free;
  end;
end;

type
  TFileDownloadThread = class(TThread)
  private
    FDownloadFileName: String;
    FOutput: TStream;
    FStartAtBytes: Int64;
    FProcessProgress: TProcessProgress;
    FHttpClient: TFPHttpClient;
    FCriticalSection: TRTLCriticalSection;
    FProcessEvent: TDataEvent;
  protected
    procedure Execute; override;
  public
    constructor Create(const DownloadFileName: String; const Output: TStream; const StartAtBytes: Int64; const ProcessEvent: TDataEvent);
    destructor Destroy; override;
    procedure TerminateDownload;
  end;

constructor TFileDownloadThread.Create(const DownloadFileName: String; const Output: TStream; const StartAtBytes: Int64; const ProcessEvent: TDataEvent);
begin
  inherited Create(False);
  FDownloadFileName := DownloadFileName;
  FOutput := Output;
  FStartAtBytes := StartAtBytes;
  FProcessEvent := ProcessEvent;
  InitCriticalSection(FCriticalSection);
  FHttpClient := TFPHttpClient.Create(nil);
end;

destructor TFileDownloadThread.Destroy;
begin
  DoneCriticalSection(FCriticalSection);
  FHttpClient.Free;
  inherited Destroy;
end;

procedure TFileDownloadThread.TerminateDownload;
begin
  FHttpClient.Terminate;
end;

procedure TFileDownloadThread.Execute;
begin
  try
    if FStartAtBytes > 0 then
      FHttpClient.AddHeader('Range', 'bytes=' + IntToStr(FStartAtBytes) + '-');
    FHttpClient.OnDataReceived := FProcessEvent;
    FHttpClient.HTTPMethod('GET', URL + FDownloadFileName, FOutput, [200, 206]);
  except
    WriteLn('ERROR');
    // Write to Log later
  end;
end;

function GetMetaData(const FileName: String; const MetaData: PBundleMeta): Boolean;
var
  HttpClient: TFPHttpClient;
  Response: TStream;
begin
  Result := False;
  try
    try
      Response := TMemoryStream.Create;
      HttpClient := TFPHttpClient.Create(nil);
      HttpClient.Get(URL + FileName, Response);
      Response.Position := 0;
      MetaData^ := ReadMetaFile(Response);
      Result := True;
    except
      MetaData^.BundleFileSize := 0;
      MetaData^.BundleType := TBundleType.TUnknownBundle;
      MetaData^.ContentVersion := 0;
      MetaData^.FileEntries := nil;
    end;
  finally
    HttpClient.Free;
    Response.Free;
  end;
end;

type
  THttpClientProcess = object
    Stream: TStream;
    procedure DownloadingData(Sender: TObject; const ContentLength, CurrentPos: Int64);
  end;

procedure THttpClientProcess.DownloadingData(Sender: TObject; const ContentLength, CurrentPos: Int64);
var
  OldPosition: Int64;
begin
  OldPosition := Stream.Position;
  Stream.Position := Stream.Size - SizeOf(Int64);
  Stream.WriteQWord(OldPosition); // Use the actual Stream's position to make sure the data was really written.
  Stream.Position := OldPosition;
end;

function FindNotMatchingFiles(const GamePath: String; const ReferenceFiles: TFileEntries): TStringList;
var
  ReferenceFileEntry: TFileEntry;
  LocalFilePath: String;
begin
  Result := TStringList.Create;
  for ReferenceFileEntry in ReferenceFiles do
  begin
    LocalFilePath := GamePath + DirectorySeparator + ReferenceFileEntry.Path;
    if not (FileExists(LocalFilePath) and (GetFileSize(LocalFilePath) = ReferenceFileEntry.Size) and (CompareByte(MD5File(LocalFilePath), ReferenceFileEntry.Checksum, SizeOf(TMD5Digest)) = 0)) then
      Result.Append(ReferenceFileEntry.Path);
  end;
end;

type
  TWholeModDownloadThread = class(TThread)
  private
    FGamePath: String;
    FProcessProgress: TProcessProgress;
    function FilterOnlyMetaFiles(const Line: String): Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(const GamePath: String; const ProcessResult: TProcessProgress);
  end;

constructor TWholeModDownloadThread.Create(const GamePath: String; const ProcessResult: TProcessProgress);
begin
  inherited Create(False);
  FGamePath := GamePath;
  FProcessProgress := ProcessResult;
end;

function TWholeModDownloadThread.FilterOnlyMetaFiles(const Line: String): Boolean;
begin
  Result := Line.EndsWith(MetaFileExtension, True);
end;

procedure TWholeModDownloadThread.Execute;
var
  ServerFiles: TStringList = nil;
  FoundMetaFiles: TStringList;
  FoundMetaFileIndex: ValSInt;
  FileEntry: TFileEntry;
  ListIndex: Int32;
  TempMeta: PBundleMeta;
  Meta: TBundleMeta;
  NotMatchingFileNames: TStringList;
  Stream: TFileStream = nil;
  StartAtBytes: Int64;
  DownloadProcess: THttpClientProcess;
  DownloadThread: TFileDownloadThread;
begin
  GetAvailableFiles(ServerFiles);
  if Assigned(ServerFiles) and not Terminated then
  begin
    if (ServerFiles.IndexOf('release.flsr.meta') >= 0) and GetMetaData('release.flsr.meta', @Meta) then
    begin
      NotMatchingFileNames := FindNotMatchingFiles(FGamePath, Meta.FileEntries);
      NotMatchingFileNames.Sorted := True;
      if NotMatchingFileNames.Count > 0 then
      begin
        FoundMetaFiles := TStringList.Create;
        ServerFiles.Filter(@FilterOnlyMetaFiles, FoundMetaFiles);
        // This places "release" at the top and any "update.n" file, with n being the version, in the right order.
        FoundMetaFiles.Sort;

        for FoundMetaFileIndex := FoundMetaFiles.Count - 1 downto 0 do
        begin
          if FoundMetaFiles.Strings[FoundMetaFileIndex] = 'release.flsr.meta' then
            TempMeta := @Meta
          else
            New(TempMeta);
          if GetMetaData(FoundMetaFiles.Strings[FoundMetaFileIndex], TempMeta) then
          begin
            FoundMetaFiles.Objects[FoundMetaFileIndex] := TObject(TempMeta);
            for FileEntry in TempMeta^.FileEntries do
              if NotMatchingFileNames.Find(FileEntry.Path, ListIndex) then
                NotMatchingFileNames.Delete(ListIndex);
          end;
          if FoundMetaFiles.Strings[FoundMetaFileIndex] <> 'release.flsr.meta' then
            Dispose(TempMeta);
          if NotMatchingFileNames.Count = 0 then
            Break;
        end;
        FoundMetaFiles.Free;
      end;   
      NotMatchingFileNames.Free;

      try
        // Reserve space for the mod download
        if FileExists('temp.flsr') then
        begin
          Stream := TFileStream.Create(FGamePath + '/temp.flsr', fmOpenReadWrite);
          StartAtBytes := Stream.ReadQWord;
        end
        else
        begin
          Stream := TFileStream.Create(FGamePath + '/temp.flsr', fmCreate);
          Stream.Size := Meta.BundleFileSize + SizeOf(Int64);
          Stream.Position := Stream.Size - SizeOf(Int64);
          StartAtBytes := 0;
          Stream.WriteQWord(StartAtBytes);
        end;        
        Stream.Position := 0;
      except

      end;
      DownloadProcess.Stream := Stream;

      DownloadThread := TFileDownloadThread.Create('release.flsr', Stream, StartAtBytes, @DownloadProcess.DownloadingData);
      DownloadThread.Start;
      while not DownloadThread.Finished do
      begin
        if Terminated then
        begin
          DownloadThread.TerminateDownload;
          DownloadThread.WaitFor;
        end;
        Sleep(10);
      end;
      DownloadThread.Free;
      // Remove position offset for resume download
      Stream.Size := Stream.Size - SizeOf(Int64);
      // Now compare the file contents to be the exact same as the meta file tells us.
      if CompareByte(MD5File(FGamePath + '/temp.flsr', SizeOf(TMD5Digest)), Meta.ContentChecksum, SizeOf(TMD5Digest)) <> 0 then
        WriteLn('ERROR Not the same contents!');

      Stream.Free;
      // Uncompressed size is sum of all included files in Meta.FileEntries;


    end;
    ServerFiles.Free;
  end;
  FProcessProgress.Done := True;
end;

type
  TMetaDataDownloadThread = class(TThread)
  private
    FMeta: PBundleMeta;
    FProcessProgress: TProcessProgress;
  protected
    procedure Execute; override;
  public
    constructor Create(const Meta: PBundleMeta; const ProcessResult: TProcessProgress);
  end;

constructor TMetaDataDownloadThread.Create(const Meta: PBundleMeta; const ProcessResult: TProcessProgress);
begin
  inherited Create(False);
  FMeta := Meta;
  FProcessProgress := ProcessResult;
end;

procedure TMetaDataDownloadThread.Execute;
var
  ServerFiles: TStringList = nil;
begin
  GetAvailableFiles(ServerFiles);
  if Assigned(ServerFiles) and not Terminated then
  begin
    if ServerFiles.IndexOf('main.flsr.meta') >= 0 then
      GetMetaData('main.flsr.meta', FMeta);
    ServerFiles.Free;
  end;
  FProcessProgress.Done := True;
end;

var
  Thread: TThread = nil;

function DownloadModData(const GamePath: String): TProcessProgress;
begin
  Result := TProcessProgress.Create;
  Thread := TWholeModDownloadThread.Create(GamePath, Result);
  Thread.FreeOnTerminate := True;
end;

function DownloadMetaData(var Meta: TBundleMeta): TProcessProgress;
begin
  Result := TProcessProgress.Create;
  Thread := TMetaDataDownloadThread.Create(@Meta, Result);
  Thread.FreeOnTerminate := True;
end;

procedure AbortDownloads;
begin
  if Assigned(Thread) then
  begin
    Thread.Terminate;
    Thread.WaitFor;
  end;
end;

end.
