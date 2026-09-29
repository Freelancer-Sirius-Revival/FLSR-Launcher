unit UDownloading;

{$mode ObjFPC}
{$H+}
{$WriteableConst OFF}
{$ScopedEnums ON}

interface

uses
  UMeta;

type
  TDownloadProgressCallback = procedure(const TotalBytes, DownloadedBytes: Int64) of object;
  TDownloadResult = (Unknown, Success, WritingFailed, NoAccess, NotFound, DownloadFailed, ChecksumMismatch, Aborted);

function DownloadModData(const Meta: TBundleMeta; const FileName: String; const Aborted: PLongBool; const ProgressCallback: TDownloadProgressCallback): TDownloadResult;
function DownloadMetaData(out Meta: TBundleMeta; const Aborted: PLongBool; const ProgressCallback: TDownloadProgressCallback): TDownloadResult;
procedure TerminateActiveHttpClient;

implementation

uses
  Classes,
  SysUtils,
  fphttpclient,
  UBundle,
  md5,
  FileUtil,
  DateUtils;

const
  URL = 'https://fl-sr.eu/files/';

var
  ActiveHttpClient: TFPHTTPClient = nil;
  ActiveHttpClientCriticalSection: TRTLCriticalSection;

function CreateActiveHttpClient: TFPHTTPClient;
begin
  EnterCriticalSection(ActiveHttpClientCriticalSection);
  Assert(not Assigned(ActiveHttpClient));
  Result := TFPHttpClient.Create(nil);
  ActiveHttpClient := Result;
  LeaveCriticalSection(ActiveHttpClientCriticalSection);
end;

procedure FreeActiveHttpClient;
begin
  EnterCriticalSection(ActiveHttpClientCriticalSection);
  Assert(Assigned(ActiveHttpClient));
  ActiveHttpClient.Free;
  ActiveHttpClient := nil;
  LeaveCriticalSection(ActiveHttpClientCriticalSection);
end;

procedure TerminateActiveHttpClient;
begin
  EnterCriticalSection(ActiveHttpClientCriticalSection);
  if Assigned(ActiveHttpClient) then
    ActiveHttpClient.Terminate;
  LeaveCriticalSection(ActiveHttpClientCriticalSection);
end;

function GetAvailableFiles(out Response: TStringList): Boolean;
begin
  Result := False;
  Response := TStringList.Create;
  try
    try
      CreateActiveHttpClient.Get(URL, Response);
      Result := True;
    except
      Response.Free;
      Response := nil;
    end;
  finally
    FreeActiveHttpClient;
  end;
end;

function GetMetaData(const FileName: String; out MetaData: TBundleMeta): Boolean;
var
  Response: TStream;
begin
  Result := False;
  try
    try
      Response := TMemoryStream.Create;
      CreateActiveHttpClient.Get(URL + FileName, Response);
      Response.Position := 0;
      MetaData := ReadMetaFile(Response);
      Result := True;
    except
      MetaData.BundleFileSize := 0;
      MetaData.BundleType := TBundleType.TUnknownBundle;
      MetaData.ContentVersion := 0;
      FillByte(MetaData.BundleFileChecksum, SizeOf(MetaData.BundleFileChecksum), 0);
      MetaData.FileEntries := nil;
    end;
  finally
    FreeActiveHttpClient;
    Response.Free;
  end;
end;

type
  THttpClientProcess = object
    Stream: TStream;
    LastSaveTime: Double;
    ProgressCallback: TDownloadProgressCallback;
    procedure DownloadingData(Sender: TObject; const ContentLength, CurrentPos: Int64);
  end;

procedure THttpClientProcess.DownloadingData(Sender: TObject; const ContentLength, CurrentPos: Int64);
var
  OldPosition: Int64;
  CurrentMoment: Double;
begin
  // ContentLength can be less if we resume a download. Always use the stream size itself.
  ProgressCallback(Stream.Size - SizeOf(Int64), Stream.Position);
  CurrentMoment := Now;
  // Save every some seconds where we currently are. For slow and fast internet this will either way be an okayish loss of time in case of cancellation.
  if MilliSecondsBetween(LastSaveTime, CurrentMoment) > 2000 then
  begin
    LastSaveTime := CurrentMoment;
    OldPosition := Stream.Position;
    Stream.Position := Stream.Size - SizeOf(Int64);
    Stream.WriteQWord(OldPosition);
    Stream.Position := OldPosition;
  end;
end;

function DownloadModData(const Meta: TBundleMeta; const FileName: String; const Aborted: PLongBool; const ProgressCallback: TDownloadProgressCallback): TDownloadResult;
const
  SourceFileName: String = 'release.flsr';
var
  ServerFiles: TStringList = nil;
  Stream: TFileStream = nil;
  StartAtBytes: Int64 = 0;
  HttpClient: TFPHTTPClient = nil;
  DownloadProcess: THttpClientProcess;
begin
  if not GetAvailableFiles(ServerFiles) then
  begin
    Assert(not Assigned(ServerFiles));
    Exit(TDownloadResult.NoAccess);
  end;

  if Aborted^ then
    Exit(TDownloadResult.Aborted);

  if ServerFiles.IndexOf(SourceFileName) < 0 then
  begin
    ServerFiles.Free;
    Exit(TDownloadResult.NotFound);
  end;
  ServerFiles.Free;

  if FileExists(FileName) then
  begin
    try
      Stream := TFileStream.Create(FileName, fmOpenReadWrite);
      Stream.Position := 0;
      if IsMatchingBundleVersion(Stream, Meta.ContentVersion, Meta.BundleType) then
      begin
        if (Stream.Size = Meta.BundleFileSize) and MDMatch(MDFile(FileName, TMDVersion.MD_VERSION_5), Meta.BundleFileChecksum) then
        begin
          ProgressCallback(1, 1);       
          FreeAndNil(Stream);
          Exit(TDownloadResult.Success);
        end
        else if Stream.Size = Meta.BundleFileSize + SizeOf(StartAtBytes) then
        begin
          Stream.Position := Stream.Size - SizeOf(StartAtBytes);
          StartAtBytes := Stream.ReadQWord;
        end;
      end;
    except
      StartAtBytes := 0;
      if Assigned(Stream) then
        FreeAndNil(Stream)
    end;
  end;

  if not Assigned(Stream) then
  begin
    try
      Stream := TFileStream.Create(FileName, fmCreate);
      Stream.Size := Meta.BundleFileSize + SizeOf(StartAtBytes);
      Stream.Position := Stream.Size - SizeOf(StartAtBytes);
      StartAtBytes := 0;
      Stream.WriteQWord(StartAtBytes);
    except
      if Assigned(Stream) then
        Stream.Free;
      Exit(TDownloadResult.WritingFailed);
    end;
  end;

  try
    try
      DownloadProcess.LastSaveTime := Now;
      DownloadProcess.Stream := Stream;
      DownloadProcess.ProgressCallback := ProgressCallback;
      HttpClient := CreateActiveHttpClient;                           
      HttpClient.OnDataReceived := @DownloadProcess.DownloadingData;
      if StartAtBytes > 0 then
        HttpClient.AddHeader('Range', 'bytes=' + IntToStr(StartAtBytes) + '-' + IntToStr(Meta.BundleFileSize));

      if Aborted^ then
        Exit(TDownloadResult.Aborted);

      Stream.Position := StartAtBytes;
      HttpClient.HTTPMethod('GET', URL + SourceFileName, Stream, [200, 206]);

      EnterCriticalSection(ActiveHttpClientCriticalSection);
      if not HttpClient.Terminated then
      begin
        Stream.Size := Meta.BundleFileSize;
        if MDMatch(MDFile(FileName, TMDVersion.MD_VERSION_5), Meta.BundleFileChecksum) then
          Result := TDownloadResult.Success
        else
          Result := TDownloadResult.ChecksumMismatch;
      end
      else
        Result := TDownloadResult.Aborted;
      LeaveCriticalSection(ActiveHttpClientCriticalSection);
    except       
      Result := TDownloadResult.DownloadFailed;
    end;
  finally
    FreeAndNil(Stream);
    FreeActiveHttpClient;
  end;                     
  Assert(Result <> TDownloadResult.Unknown);
end;

function DownloadMetaData(out Meta: TBundleMeta; const Aborted: PLongBool; const ProgressCallback: TDownloadProgressCallback): TDownloadResult;        
const
  SourceFileName: String = 'release.flsr.meta';
var
  ServerFiles: TStringList = nil;
begin
  Result := TDownloadResult.Unknown;
  if not GetAvailableFiles(ServerFiles) then
  begin
    Assert(not Assigned(ServerFiles));
    Exit(TDownloadResult.NoAccess);
  end;

  if Assigned(ProgressCallback) then
    ProgressCallback(2, 1);

  if Aborted^ then
    Exit(TDownloadResult.Aborted);

  if ServerFiles.IndexOf(SourceFileName) >= 0 then
  begin
    if GetMetaData(SourceFileName, Meta) then
      Result := TDownloadResult.Success
    else
      Result := TDownloadResult.DownloadFailed;
    if Assigned(ProgressCallback) then
      ProgressCallback(2, 2);
  end
  else
    Result := TDownloadResult.NotFound;
  Assert(Result <> TDownloadResult.Unknown);
  ServerFiles.Free;
end;

initialization
  InitCriticalSection(ActiveHttpClientCriticalSection);

finalization
  Assert(not Assigned(ActiveHttpClient));
  DoneCriticalSection(ActiveHttpClientCriticalSection);

end.
