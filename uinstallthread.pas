unit UInstallThread;

{$mode ObjFPC}
{$H+}
{$WriteableConst OFF}
{$ScopedEnums ON}
{$PACKENUM 4}

interface

uses
  Classes,
  SysUtils,
  UMeta,
  UDownloading,
  UDecoding;

type
  TTask = (None, DownloadMeta, VerifyFlsrInstallation, VerifyFreelancerAndFlsrPath, CopyFreelancer, DownloadMod, DecodeMod);
  TTaskError = record
    case Task: TTask of
      TTask.DownloadMeta,
      TTask.DownloadMod: (DownloadResult: TDownloadResult);
      TTask.VerifyFlsrInstallation: (NoVersionFile: Boolean;
        WrongVersion: Boolean;);
      TTask.VerifyFreelancerAndFlsrPath: (FreelancerInvalid: Boolean;
        FlsrPathInvalid: Boolean;
        MissingBytes: Int64);
      TTask.CopyFreelancer: (InvalidPath: Pchar);
      TTask.DecodeMod: (DecoderErrorType: TDecoderErrorType;
        DecoderErrorReason: Pchar);
  end;
  TTaskDoneCallback = procedure(const Task: TTask; const Result: Boolean; const Errors: TTaskError) of object;
  TProgressCallback = procedure(const MaxProgress, CurrentProgress: Int64) of object;
  TSetBundleMeta = procedure(const Meta: TBundleMeta) of object;

procedure CreateInstallThread;
procedure TerminateInstallThread;
procedure AbortCurrentInstallTask;
procedure DownloadMeta(const SetBundleMeta: TSetBundleMeta; const TaskDoneCallback: TTaskDoneCallback; const ProgressCallback: TProgressCallback);
procedure VerifyFlsrInstallation(const FlsrPath: String; const Meta: TBundleMeta; const TaskDoneCallback: TTaskDoneCallback; const ProgressCallback: TProgressCallback);
procedure VerifyFreelancerAndFlsrPath(const FreelancerPath, FlsrPath: String; const Meta: TBundleMeta; const TaskDoneCallback: TTaskDoneCallback; const ProgressCallback: TProgressCallback);
procedure CopyFreelancer(const FreelancerPath, TargetPath: String; const Meta: TBundleMeta; const TaskDoneCallback: TTaskDoneCallback; const ProgressCallback: TProgressCallback);
procedure DownloadMod(const Meta: TBundleMeta; const FileName: String; const TaskDoneCallback: TTaskDoneCallback; const ProgressCallback: TProgressCallback);
procedure DecodeMod(const Meta: TBundleMeta; const BundleFileName, TargetDirectory: String; const TaskDoneCallback: TTaskDoneCallback; const ProgressCallback: TProgressCallback);

implementation

uses
  UBundle,
  UCopyFiles,
  UInstallSteps,
  USettings;

type      
  TDownloadMetaData = record
    SetBundleMeta: TSetBundleMeta;
  end;                           
  PDownloadMetaData = ^TDownloadMetaData;

  TVerifyFlsrInstallationData = record
    FlsrPath: String;
    Meta: TBundleMeta;
  end;
  PVerifyFlsrInstallationData = ^TVerifyFlsrInstallationData;

  TVerifyFreelancerAndFlsrPathData = record
    FreelancerPath: String;
    FlsrPath: String;
    Meta: TBundleMeta;
  end;
  PVerifyFreelancerAndFlsrPathData = ^TVerifyFreelancerAndFlsrPathData;

  TCopyFreelancerData = record
    FreelancerPath: String;
    TargetPath: String;
    Meta: TBundleMeta;
  end;
  PCopyFreelancerData = ^TCopyFreelancerData;

  TDownloadModData = record
    Meta: TBundleMeta;
    FileName: String;
  end;
  PDownloadModData = ^TDownloadModData;

  TDecodeData = record
    Meta: TBundleMeta;
    BundleFileName: String;
    TargetDirectory: String;
  end;
  PDecodeData = ^TDecodeData;

  TInstallThread = class(TThread)
  private
  type
    TTaskPayload = record
      Task: TTask;
      Data: Pointer;
      DoneCallback: TTaskDoneCallback;
      ProgressCallback: TProgressCallback;
    end;
  var
    FResumeEvent: PRTLEvent;
    FTaskAborted: Longbool;
    FTask: TTaskPayload;
    procedure CopyFreelancer(out Result: Boolean; out Errors: TTaskError);
    procedure CreateProgressCallbackCall(const MaxProgress, CurrentProgress: Int64);
    procedure CreateTaskDoneCallbackCall(const Task: TTask; const Result: Boolean; const Errors: TTaskError);
    procedure DecodeMod(out Result: Boolean; out Errors: TTaskError);
    procedure DownloadMeta(out Result: Boolean; out Errors: TTaskError);
    procedure DownloadMod(out Result: Boolean; out Errors: TTaskError);
    procedure VerifyFlsrInstallation(out Result: Boolean; out Errors: TTaskError);
    procedure VerifyFreelancerAndFlsrPath(out Result: Boolean; out Errors: TTaskError);
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure StartTaskWithData(const NewTask: TTask; const Data: Pointer; const DoneCallback: TTaskDoneCallback; const ProgressCallback: TProgressCallback);
    procedure SetTaskAborted;
  end;

var
  Thread: TInstallThread = nil;

constructor TInstallThread.Create;
begin
  inherited Create(False);
  FTaskAborted := False;
  FTask.Task := TTask.None;
  FTask.Data := nil;
  FTask.DoneCallback := nil;
  FTask.ProgressCallback := nil;
  FResumeEvent := RTLEventCreate;
end;

destructor TInstallThread.Destroy;
begin
  RTLEventDestroy(FResumeEvent);
  inherited Destroy;
end;

procedure TInstallThread.StartTaskWithData(const NewTask: TTask; const Data: Pointer; const DoneCallback: TTaskDoneCallback; const ProgressCallback: TProgressCallback);
begin
  Assert(FTask.Task = TTask.None);
  FTask.Task := NewTask;
  FTask.Data := Data;
  FTask.DoneCallback := DoneCallback;
  FTask.ProgressCallback := ProgressCallback;
  FTaskAborted := False;
  RTLEventSetEvent(FResumeEvent);
end;

procedure TInstallThread.SetTaskAborted;
begin
  InterlockedExchange(Int32(FTaskAborted), Int32(True));
  TerminateActiveHttpClient;
end;

type
  TTaskDoneCallbackProxy = object
    Task: TTask;
    Result: Boolean;
    Errors: TTaskError;
    Callback: TTaskDoneCallback;
    procedure Invoke;
  end;
  PTaskDoneCallbackProxy = ^TTaskDoneCallbackProxy;

procedure TTaskDoneCallbackProxy.Invoke;
begin
  Callback(Task, Result, Errors);
  Dispose(PTaskDoneCallbackProxy(@Self));
end;

type
  TProgressCallbackProxy = object
    MaxProgress: Int64;
    CurrentProgress: Int64;
    Callback: TProgressCallback;
    procedure Invoke;
  end;
  PProgressCallbackProxy = ^TProgressCallbackProxy;

procedure TProgressCallbackProxy.Invoke;
begin
  Callback(MaxProgress, CurrentProgress);
  Dispose(PProgressCallbackProxy(@Self));
end;

procedure TInstallThread.CreateProgressCallbackCall(const MaxProgress, CurrentProgress: Int64);
var
  Proxy: PProgressCallbackProxy;
begin
  if Assigned(FTask.ProgressCallback) then
  begin
    New(Proxy);
    Proxy^.MaxProgress := MaxProgress;
    Proxy^.CurrentProgress := CurrentProgress;
    Proxy^.Callback := FTask.ProgressCallback;
    Queue(@Proxy^.Invoke);
  end;
end;

procedure TInstallThread.CreateTaskDoneCallbackCall(const Task: TTask; const Result: Boolean; const Errors: TTaskError);
var
  Proxy: PTaskDoneCallbackProxy;
begin
  if Assigned(FTask.DoneCallback) then
  begin
    New(Proxy);
    Proxy^.Task := Task;
    Proxy^.Result := Result;
    Proxy^.Errors := Errors;
    Proxy^.Callback := FTask.DoneCallback;
    Queue(@Proxy^.Invoke);
  end;
end;

procedure TInstallThread.DownloadMeta(out Result: Boolean; out Errors: TTaskError);
var
  MetaResult: TBundleMeta;
begin
  Errors.DownloadResult := DownloadMetaData(MetaResult, @FTaskAborted, @CreateProgressCallbackCall);
  Assert(Assigned(FTask.Data));
  PDownloadMetaData(FTask.Data)^.SetBundleMeta(MetaResult);
  Result := Errors.DownloadResult = TDownloadResult.Success;

  Dispose(PDownloadMetaData(FTask.Data));
  {$IfOpt C+}
  FTask.Data := nil;
  {$EndIf}
end;

procedure TInstallThread.VerifyFlsrInstallation(out Result: Boolean; out Errors: TTaskError);
begin
  Assert(Assigned(FTask.Data));
  with PVerifyFlsrInstallationData(FTask.Data)^ do
  begin
    Errors.NoVersionFile := not IsFlsrInstalled(FlsrPath);
    if not Errors.NoVersionFile then
      Errors.WrongVersion := not IsLatestFlsrVersion(FlsrPath, Meta, @CreateProgressCallbackCall)
    else
      Errors.WrongVersion := False;
  end; 
  Result := not Errors.NoVersionFile and not Errors.WrongVersion;

  Dispose(PVerifyFlsrInstallationData(FTask.Data));
  {$IfOpt C+}
  FTask.Data := nil;
  {$EndIf}
end;

procedure TInstallThread.VerifyFreelancerAndFlsrPath(out Result: Boolean; out Errors: TTaskError);
var
  FileList: TStrings = nil;
begin
  Assert(Assigned(FTask.Data));
  FTask.ProgressCallback(4, 0);
  Errors.FreelancerInvalid := not IsOriginalFreelancer(PVerifyFreelancerAndFlsrPathData(FTask.Data)^.FreelancerPath);
  FTask.ProgressCallback(4, 1);
  Errors.FlsrPathInvalid := not CanCreateFiles(PVerifyFreelancerAndFlsrPathData(FTask.Data)^.FlsrPath);
  FTask.ProgressCallback(4, 2);
  Result := not Errors.FreelancerInvalid and not Errors.FlsrPathInvalid;
  if Result then
  begin
    with PVerifyFreelancerAndFlsrPathData(FTask.Data)^ do
      FileList := FindOriginalFilesToCopy(FreelancerPath, Meta.FileEntries);
    FTask.ProgressCallback(4, 3);
    with PVerifyFreelancerAndFlsrPathData(FTask.Data)^ do
      Errors.MissingBytes := EvaluateMissingDiskSpace(FileList, FlsrPath, Meta.FileEntries, Meta.BundleFileSize);
    FTask.ProgressCallback(4, 4);
    FreeAndNil(FileList);
  end
  else
    Errors.MissingBytes := 0;
  Result := Result and (Errors.MissingBytes = 0);

  Assert(not Assigned(FileList));
  Dispose(PVerifyFreelancerAndFlsrPathData(FTask.Data));
  {$IfOpt C+}
  FTask.Data := nil;
  {$EndIf}
end;

procedure TInstallThread.CopyFreelancer(out Result: Boolean; out Errors: TTaskError);
var
  FileList: TStrings = nil;
  ErrorOut: String;
begin
  Assert(Assigned(FTask.Data));
  with PCopyFreelancerData(FTask.Data)^ do
  begin
    FileList := FindOriginalFilesToCopy(FreelancerPath, Meta.FileEntries);
    Result := CopyFiles(FreelancerPath, TargetPath, FileList, @FTaskAborted, @CreateProgressCallbackCall, ErrorOut);
  end;
  FreeAndNil(FileList);
  if not ErrorOut.IsEmpty then
    Errors.InvalidPath := PChar(ErrorOut.ToCharArray)
  else
    Errors.InvalidPath := PChar('');

  Dispose(PCopyFreelancerData(FTask.Data));
  {$IfOpt C+}
  FTask.Data := nil;
  {$EndIf}
end;

procedure TInstallThread.DownloadMod(out Result: Boolean; out Errors: TTaskError);
begin
  Assert(Assigned(FTask.Data));
  with PDownloadModData(FTask.Data)^ do
    Errors.DownloadResult := DownloadModData(Meta, FileName, @FTaskAborted, @CreateProgressCallbackCall);
  Result := Errors.DownloadResult = TDownloadResult.Success;

  Dispose(PDownloadModData(FTask.Data));
  {$IfOpt C+}
  FTask.Data := nil;
  {$EndIf}
end;

procedure TInstallThread.DecodeMod(out Result: Boolean; out Errors: TTaskError);
var
  DecoderErrors: TDecoderErrorArray;
  Stream: TStream;
  Bundle: TBundle;
begin
  Assert(Assigned(FTask.Data));
  Errors.DecoderErrorType := TDecoderErrorType.None;
  Errors.DecoderErrorReason := nil;
  try
    try
      Stream := TFileStream.Create(PDecodeData(FTask.Data)^.BundleFileName, fmOpenRead or fmShareDenyWrite);
      Bundle := ReadBundleMetaData(Stream);
      with PDownloadModData(FTask.Data)^ do
        Result := DecodeFilesChunks(Bundle.FilesChunks, Stream, PDecodeData(FTask.Data)^.TargetDirectory, @FTaskAborted, @CreateProgressCallbackCall, DecoderErrors);
      Bundle.FilesChunks := nil;
      if Length(DecoderErrors) > 0 then
      begin
        Errors.DecoderErrorType := DecoderErrors[0].ErrorType;
        Errors.DecoderErrorReason := PChar(DecoderErrors[0].Reason.ToCharArray);
      end;
    except
    end;
  finally
    if Assigned(Stream) then
      Stream.Free;
  end;

  if Result then
  begin
    DeleteFile(PDecodeData(FTask.Data)^.BundleFileName);
    try
      try
        Stream := TFileStream.Create(PDecodeData(FTask.Data)^.TargetDirectory + DirectorySeparator + FlsrVersionFileName, fmCreate);
        Stream.WriteBuffer(PDecodeData(FTask.Data)^.Meta.ContentVersion, SizeOf(TBundleMeta.ContentVersion));
      except
      end;
    finally
      if Assigned(Stream) then
        Stream.Free;
    end;
  end;

  Dispose(PDecodeData(FTask.Data));
  {$IfOpt C+}
  FTask.Data := nil;
  {$EndIf}
end;

procedure TInstallThread.Execute;
var
  Result: Boolean;
  Errors: TTaskError;
begin
  repeat
    RTLEventResetEvent(FResumeEvent);
    Result := False;
    Errors.Task := FTask.Task;
    case FTask.Task of
      TTask.DownloadMeta: DownloadMeta(Result, Errors);
      TTask.VerifyFlsrInstallation: VerifyFlsrInstallation(Result, Errors);
      TTask.VerifyFreelancerAndFlsrPath: VerifyFreelancerAndFlsrPath(Result, Errors);
      TTask.CopyFreelancer: CopyFreelancer(Result, Errors);
      TTask.DownloadMod: DownloadMod(Result, Errors);
      TTask.DecodeMod: DecodeMod(Result, Errors);
    end;
    if FTask.Task <> TTask.None then
      CreateTaskDoneCallbackCall(FTask.Task, Result, Errors);
    Assert(not Assigned(FTask.Data));
    {$IfOpt C+}
    InterlockedExchange(Int32(FTask.Task), Int32(TTask.None));
    {$EndIf}
    RTLEventWaitFor(FResumeEvent);
  until Terminated;
end;

procedure CreateInstallThread;
begin
  if not Assigned(Thread) then
    Thread := TInstallThread.Create;
end;

procedure TerminateInstallThread;
begin
  if Assigned(Thread) then
  begin
    AbortCurrentInstallTask;
    Thread.Terminate;
    RTLEventSetEvent(Thread.FResumeEvent);
    Thread.WaitFor;
    Thread.Free;
  end;
end;

procedure AbortCurrentInstallTask;
begin
  Assert(Assigned(Thread));
  Thread.SetTaskAborted;
end;

procedure DownloadMeta(const SetBundleMeta: TSetBundleMeta; const TaskDoneCallback: TTaskDoneCallback; const ProgressCallback: TProgressCallback);
var
  Data: PDownloadMetaData;
begin
  Assert(Assigned(Thread));
  New(Data);
  Data^.SetBundleMeta := SetBundleMeta;
  Thread.StartTaskWithData(TTask.DownloadMeta, Data, TaskDoneCallback, ProgressCallback);
end;

procedure VerifyFlsrInstallation(const FlsrPath: String; const Meta: TBundleMeta; const TaskDoneCallback: TTaskDoneCallback; const ProgressCallback: TProgressCallback);
var
  Data: PVerifyFlsrInstallationData;
begin
  Assert(Assigned(Thread));
  New(Data);
  Data^.FlsrPath := FlsrPath;
  Data^.Meta := Meta;
  Thread.StartTaskWithData(TTask.VerifyFlsrInstallation, Data, TaskDoneCallback, ProgressCallback);
end;

procedure VerifyFreelancerAndFlsrPath(const FreelancerPath, FlsrPath: String; const Meta: TBundleMeta; const TaskDoneCallback: TTaskDoneCallback; const ProgressCallback: TProgressCallback);
var
  Data: PVerifyFreelancerAndFlsrPathData;
begin
  Assert(Assigned(Thread));
  New(Data);
  Data^.FreelancerPath := FreelancerPath;
  Data^.FlsrPath := FlsrPath;
  Data^.Meta := Meta;
  Thread.StartTaskWithData(TTask.VerifyFreelancerAndFlsrPath, Data, TaskDoneCallback, ProgressCallback);
end;

procedure CopyFreelancer(const FreelancerPath, TargetPath: String; const Meta: TBundleMeta; const TaskDoneCallback: TTaskDoneCallback; const ProgressCallback: TProgressCallback);
var
  Data: PCopyFreelancerData;
begin
  Assert(Assigned(Thread));
  New(Data);
  Data^.FreelancerPath := FreelancerPath;
  Data^.TargetPath := TargetPath;
  Data^.Meta := Meta;
  Thread.StartTaskWithData(TTask.CopyFreelancer, Data, TaskDoneCallback, ProgressCallback);
end;

procedure DownloadMod(const Meta: TBundleMeta; const FileName: String; const TaskDoneCallback: TTaskDoneCallback; const ProgressCallback: TProgressCallback);
var
  Data: PDownloadModData;
begin
  Assert(Assigned(Thread));
  New(Data);
  Data^.FileName := FileName;
  Data^.Meta := Meta;
  Thread.StartTaskWithData(TTask.DownloadMod, Data, TaskDoneCallback, ProgressCallback);
end;

procedure DecodeMod(const Meta: TBundleMeta; const BundleFileName, TargetDirectory: String; const TaskDoneCallback: TTaskDoneCallback; const ProgressCallback: TProgressCallback);
var
  Data: PDecodeData;
begin
  Assert(Assigned(Thread));
  New(Data);
  Data^.BundleFileName := BundleFileName;
  Data^.TargetDirectory := TargetDirectory;
  Data^.Meta := Meta;
  Thread.StartTaskWithData(TTask.DecodeMod, Data, TaskDoneCallback, ProgressCallback);
end;

end.
