unit UBundleDownload;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils;

type
  TProcessProgress = class
  private
    FTotalBytes: Int64;
    FPercentage: Single;
    FDone: Int32;
    function GetDone: Boolean;
    procedure SetDone(const Done: Boolean);
    procedure SetPercentage(const NewPercentage: Single);
    procedure OnEncodingProgress(const BytesWritten: Int64);
  public
    constructor Create;
    property Percentage: Single read FPercentage write SetPercentage;
    property Done: Boolean read GetDone write SetDone;
  end;

function UpdateMod(const GamePath: String): TProcessProgress;

implementation

uses
  fphttpclient,
  UBundle,
  UMeta;

function TProcessProgress.GetDone: Boolean;
begin
  Result := Boolean(FDone);
end;

procedure TProcessProgress.SetDone(const Done: Boolean);
begin
  InterlockedExchange(FDone, Int32(Done));
end;

procedure TProcessProgress.SetPercentage(const NewPercentage: Single);
begin
  InterlockedExchange(Int32(FPercentage), Int32(NewPercentage));
end;

procedure TProcessProgress.OnEncodingProgress(const BytesWritten: Int64);
begin
  SetPercentage(BytesWritten / FTotalBytes);
end;

constructor TProcessProgress.Create;
begin
  inherited Create;
  FTotalBytes := 0;
  FPercentage := 0;
  Done := False;
end;

const
  URL = 'https://fl-sr.eu/files/';

procedure GetAvailableFiles(out Response: TStrings);
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

function GetFile(const FileName: String; const Output: TStream; const StartAtBytes: Int64; const ProcessEvent: TDataEvent): Boolean;
var
  HttpClient: TFPHttpClient;
begin
  Result := False;
  try
  try
    HttpClient := TFPHttpClient.Create(nil);
    if StartAtBytes > 0 then
      HttpClient.AddHeader('Range', 'bytes=' + IntToStr(StartAtBytes) + '-');
    HttpClient.OnDataReceived := ProcessEvent;
    HttpClient.HTTPMethod('GET', URL + FileName, Output, [200, 206]);
    Result := True;
  except
    // Write to Log later
  end;
  finally
    HttpClient.Free;
  end;
end;

procedure GetMetaData(const FileName: String; out MetaData: TBundleMeta);
var
  HttpClient: TFPHttpClient;
  Response: TStream;
begin
  try
  try
    Response := TMemoryStream.Create;
    HttpClient := TFPHttpClient.Create(nil);
    HttpClient.Get(URL + FileName, Response);
    Response.Position := 0;
    MetaData := ReadMetaFile(Response);
  except
    MetaData.BundleFileSize := 0;
    MetaData.BundleType := TBundleType.TUnknownBundle;
    MetaData.ContentVersion := 0;
    MetaData.FileEntries := nil;
    // Write to Log later
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
  Stream.Position := 0;
  Stream.WriteQWord(OldPosition - SizeOf(Int64)); // Use the actual Stream's position to make sure the data was really written.
  Stream.Position := OldPosition;
end;

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

procedure DownloadMod(const GamePath: String);
var
  ServerFiles: TStrings;
  Meta: TBundleMeta;
  Stream: TStream;
  StartAtBytes: Int64;
  DownloadProcess: THttpClientProcess;
begin
  GetAvailableFiles(ServerFiles);
  if Assigned(ServerFiles) then
  begin
    if ServerFiles.IndexOf('main.flsr.meta') >= 0 then
    begin
      GetMetaData('main.flsr.meta', Meta);
      // Reserve space for the mod download
      if FileExists('main.flsr') then
      begin
        Stream := TFileStream.Create(GamePath + 'temp.flsr', fmOpenReadWrite);
        StartAtBytes := Stream.ReadQWord;
      end
      else
      begin
        Stream := TFileStream.Create(GamePath + 'temp.flsr', fmCreate);
        Stream.Size := Meta.BundleFileSize + SizeOf(Int64);
        StartAtBytes := -1;
      end;
      Stream.Position := SizeOf(Int64);
      DownloadProcess.Stream := Stream;
      GetFile('main.flsr', Stream, StartAtBytes, @DownloadProcess.DownloadingData);


      Stream.Free;

      // Uncompressed size is sum of all included files in Meta.FileEntries;
    end;
    ServerFiles.Free;
  end;
end;

type
  TDownloadThread = class(TThread)
  private
    FGamePath: String;
    FProcessProgress: TProcessProgress;
  protected
    procedure Execute; override;
  public
    constructor Create(const GamePath: String; const ProcessResult: TProcessProgress);
  end;

constructor TDownloadThread.Create(const GamePath: String; const ProcessResult: TProcessProgress);
begin
  inherited Create(False);
  FGamePath := GamePath;
  FProcessProgress := ProcessResult;
end;

procedure TDownloadThread.Execute;
begin
  DownloadMod(FGamePath);
  FProcessProgress.Done := True;
end;

function UpdateMod(const GamePath: String): TProcessProgress;
var
  ProcessThread: TDownloadThread;
begin
  Result := TProcessProgress.Create;
  ProcessThread := TDownloadThread.Create(GamePath, Result);
  ProcessThread.FreeOnTerminate := True;
end;

end.
