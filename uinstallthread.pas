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
  TTask = (None, DownloadMeta, VerifyFreelancerAndFlsr, CopyFreelancer, DownloadMod, DecodeMod);
  TTaskError = record
    case Task: TTask of
      TTask.DownloadMeta,
      TTask.DownloadMod: (DownloadResult: TDownloadResult);
      TTask.VerifyFreelancerAndFlsr: (FreelancerInvalid: Boolean;
        FlsrInvalid: Boolean;
        MissingBytes: Int64);
      TTask.CopyFreelancer: (InvalidPath: Pchar);
      TTask.DecodeMod: (DecoderErrorType: TDecoderErrorType;
        DecoderErrorReason: Pchar;);
  end;
  TTaskDoneCallback = procedure(const Task: TTask; const Result: Boolean; const Errors: TTaskError) of object;
  TProgressCallback = procedure(const MaxProgress, CurrentProgress: Int64) of object;

procedure CreateInstallThread;
procedure TerminateInstallThread;
procedure AbortCurrentInstallTask;
function GetBundleMeta(out Meta: TBundleMeta): Boolean;
procedure DownloadMeta(const TaskDoneCallback: TTaskDoneCallback; const ProgressCallback: TProgressCallback);
procedure VerifyFreelancerAndFlsr(const FreelancerPath, FlsrPath: String; const TaskDoneCallback: TTaskDoneCallback; const ProgressCallback: TProgressCallback);
procedure CopyFreelancer(const FreelancerPath, TargetPath: String; const TaskDoneCallback: TTaskDoneCallback; const ProgressCallback: TProgressCallback);
procedure DownloadMod(const Meta: TBundleMeta; const FileName: String; const TaskDoneCallback: TTaskDoneCallback; const ProgressCallback: TProgressCallback);
procedure DecodeMod(const Meta: TBundleMeta; const BundleFileName, TargetDirectory: String; const TaskDoneCallback: TTaskDoneCallback; const ProgressCallback: TProgressCallback);

implementation

uses
  UBundle,
  UCopyFiles,
  UInstallSteps,
  USettings;

type
  TVerifyFreelancerAndFLsrData = record
    FreelancerPath: String;
    FlsrPath: String;
  end;
  PVerifyFreelancerAndFlsrData = ^TVerifyFreelancerAndFLsrData;

  TCopyFreelancerData = record
    FreelancerPath: String;
    TargetPath: String;
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
    FMeta: TBundleMeta;
    procedure CreateProgressCallbackCall(const MaxProgress, CurrentProgress: Int64);
    procedure CreateTaskDoneCallbackCall(const Task: TTask; const Result: Boolean; const Errors: TTaskError);
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
  FMeta.InitEmpty;
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

procedure TInstallThread.Execute;
var
  Result: Boolean;
  Errors: TTaskError;
  ErrorOut: String;
  DecoderErrors: TDecoderErrorArray;
  FileList: TStrings;
  Stream: TStream;
  Bundle: TBundle;
begin
  repeat
    RTLEventResetEvent(FResumeEvent);
    Result := False;
    FileList := nil;
    Errors.Task := FTask.Task;
    case FTask.Task of
      TTask.DownloadMeta:
      begin
        Errors.DownloadResult := DownloadMetaData(FMeta, @FTaskAborted, @CreateProgressCallbackCall);
        Result := Errors.DownloadResult = TDownloadResult.Success;
      end;

      TTask.VerifyFreelancerAndFlsr:
      begin
        Assert(Assigned(FTask.Data));
        Errors.FreelancerInvalid := not ValidateOriginalFreelancer(PVerifyFreelancerAndFlsrData(FTask.Data)^.FreelancerPath);
        Errors.FlsrInvalid := not ValidateCanCreateFiles(PVerifyFreelancerAndFlsrData(FTask.Data)^.FlsrPath);
        Result := not Errors.FreelancerInvalid and not Errors.FlsrInvalid;
        if Result then
        begin
          FileList := FindOriginalFilesToCopy(PVerifyFreelancerAndFlsrData(FTask.Data)^.FreelancerPath, FMeta.FileEntries);
          Errors.MissingBytes := EvaluateMissingDiskSpace(FileList, PVerifyFreelancerAndFlsrData(FTask.Data)^.FlsrPath, FMeta.FileEntries, FMeta.BundleFileSize);
          FreeAndNil(FileList);
        end
        else
          Errors.MissingBytes := 0;
        Result := Result and (Errors.MissingBytes = 0);
        if Result then
          WriteFlsrPathToConfig(PVerifyFreelancerAndFlsrData(FTask.Data)^.FlsrPath);

        Assert(not Assigned(FileList));
        Dispose(PVerifyFreelancerAndFlsrData(FTask.Data));
        {$IfOpt C+}
        FTask.Data := nil;        
        {$EndIf}
      end;

      TTask.CopyFreelancer:
      begin
        Assert(Assigned(FTask.Data));
        with PCopyFreelancerData(FTask.Data)^ do
        begin
          FileList := FindOriginalFilesToCopy(FreelancerPath, FMeta.FileEntries);
          Result := CopyFiles(FreelancerPath, TargetPath, FileList, @FTaskAborted, @CreateProgressCallbackCall, ErrorOut);
        end;
        FreeAndNil(FileList);
        if not ErrorOut.IsEmpty then
          Errors.InvalidPath := PChar(ErrorOut.ToCharArray);

        Dispose(PCopyFreelancerData(FTask.Data));
        {$IfOpt C+}
        FTask.Data := nil;    
        {$EndIf}
      end;

      TTask.DownloadMod:
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

      TTask.DecodeMod:
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

        try
          try
            Stream := TFileStream.Create(PDecodeData(FTask.Data)^.TargetDirectory + DirectorySeparator + FlsrVersionFileName, fmCreate);
            Stream.WriteBuffer(FMeta.ContentVersion, SizeOf(TBundleMeta.ContentVersion));
          except
          end;
        finally
          if Assigned(Stream) then
            Stream.Free;
        end;
        Dispose(PDecodeData(FTask.Data));
        {$IfOpt C+}
        FTask.Data := nil;
        {$EndIf}
      end;
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
  if not Assigned(Thread) then
    Exit;
  Thread.SetTaskAborted;
end;

function GetBundleMeta(out Meta: TBundleMeta): Boolean;
begin
  if not Assigned(Thread) or (Thread.FMeta.BundleType = TBundleType.TUnknownBundle) then
    Exit(False);

  Meta := Thread.FMeta;
  Result := True;
end;

procedure DownloadMeta(const TaskDoneCallback: TTaskDoneCallback; const ProgressCallback: TProgressCallback);
begin
  if not Assigned(Thread) then
    Exit;
  Thread.StartTaskWithData(TTask.DownloadMeta, nil, TaskDoneCallback, ProgressCallback);
end;

procedure VerifyFreelancerAndFlsr(const FreelancerPath, FlsrPath: String; const TaskDoneCallback: TTaskDoneCallback; const ProgressCallback: TProgressCallback);
var
  Data: PVerifyFreelancerAndFlsrData;
begin
  if not Assigned(Thread) then
    Exit;
  New(Data);
  Data^.FreelancerPath := FreelancerPath;
  Data^.FlsrPath := FlsrPath;
  Thread.StartTaskWithData(TTask.VerifyFreelancerAndFlsr, Data, TaskDoneCallback, ProgressCallback);
end;

procedure CopyFreelancer(const FreelancerPath, TargetPath: String; const TaskDoneCallback: TTaskDoneCallback; const ProgressCallback: TProgressCallback);
var
  Data: PCopyFreelancerData;
begin
  if not Assigned(Thread) then
    Exit;
  New(Data);
  Data^.FreelancerPath := FreelancerPath;
  Data^.TargetPath := TargetPath;
  Thread.StartTaskWithData(TTask.CopyFreelancer, Data, TaskDoneCallback, ProgressCallback);
end;

procedure DownloadMod(const Meta: TBundleMeta; const FileName: String; const TaskDoneCallback: TTaskDoneCallback; const ProgressCallback: TProgressCallback);
var
  Data: PDownloadModData;
begin
  if not Assigned(Thread) then
    Exit;
  New(Data);
  Data^.FileName := FileName;
  Data^.Meta := Meta;
  Thread.StartTaskWithData(TTask.DownloadMod, Data, TaskDoneCallback, ProgressCallback);
end;

procedure DecodeMod(const Meta: TBundleMeta; const BundleFileName, TargetDirectory: String; const TaskDoneCallback: TTaskDoneCallback; const ProgressCallback: TProgressCallback);
var
  Data: PDecodeData;
begin
  if not Assigned(Thread) then
    Exit;
  New(Data);
  Data^.BundleFileName := BundleFileName;
  Data^.TargetDirectory := TargetDirectory;
  Data^.Meta := Meta;
  Thread.StartTaskWithData(TTask.DecodeMod, Data, TaskDoneCallback, ProgressCallback);
end;

end.
