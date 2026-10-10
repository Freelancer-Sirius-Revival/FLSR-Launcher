unit UInstallFrame;

{$mode ObjFPC}
{$H+}
{$WriteableConst OFF}
{$ScopedEnums ON}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  ExtCtrls,
  Buttons,
  StdCtrls,
  ComCtrls,
  UMeta,
  UInstallThread;

type
  TInstallFrame = class(TFrame)
  published
    ContinueButton: TButton;
    CancelButton: TButton;
    FlsrPathError: TLabel;
    InstallHeadingLabel: TLabel;
    FreelancerPathError: TLabel;
    PathsPanel: TPanel;
    FreelancerPathLabel: TLabel;
    FreelancerPathInput: TEdit;
    FreelancerPathButton: TButton;
    FreelancerPathDialog: TSelectDirectoryDialog;
    FlsrPathLabel: TLabel;
    FlsrPathInput: TEdit;
    FlsrPathButton: TButton;
    FlsrPathDialog: TSelectDirectoryDialog;
    ProgressError: TLabel;
    ProgressPanel: TPanel;
    ProgressStepLabel: TLabel;
    ProgressBar: TProgressBar;
    ProgressLabel: TLabel;
    MainControlsPanel: TPanel;
    procedure CancelButtonClick(Sender: TObject);
    procedure ContinueButtonClick(Sender: TObject);
    procedure FreelancerPathButtonClick(Sender: TObject);
    procedure FlsrPathButtonClick(Sender: TObject);
  private
    FLastProgressUpdate: Double;
    FLastBundleMeta: TBundleMeta;
    procedure DisplayDownloadMetaErrors(const Errors: TTaskError);
    procedure SetBundleMeta(const Meta: TBundleMeta);
    procedure SetUpInstallStep(const Task: TTask);
    procedure SetUpdateTaskDone(const Task: TTask; const Result: Boolean; const Errors: TTaskError);
    procedure SetInstallTaskDone(const Task: TTask; const Result: Boolean; const Errors: TTaskError);
    procedure SetInstallProgress(const MaxProgress, CurrentProgress: Int64);
  public
    constructor Create(TheOwner: TComponent); override;
    procedure BeginInstallWorkflow;
    procedure BeginUpdateWorkflow;
  end;

implementation

uses
  UMainForm,
  LCLIntf,
  FileUtil,
  DateUtils,
  Math,
  USettings,
  UDecoding,
  UDownloading;

  {$R *.lfm}

procedure TInstallFrame.CancelButtonClick(Sender: TObject);
begin
  MainForm.InstallFrame.Visible := False;
end;

procedure TInstallFrame.ContinueButtonClick(Sender: TObject);
begin
  ContinueButton.Enabled := False;
  FreelancerPathError.Visible := False;
  FlsrPathError.Visible := False;
  PathsPanel.Enabled := False;
  VerifyFreelancerAndFlsrPath(FreelancerPathInput.Text, FlsrPathInput.Text, FLastBundleMeta, @SetInstallTaskDone, @SetInstallProgress);
end;

procedure TInstallFrame.FreelancerPathButtonClick(Sender: TObject);
begin
  if DirectoryExists(FreelancerPathInput.Text) then
    FreelancerPathDialog.FileName := FreelancerPathInput.Text
  else
    FreelancerPathDialog.FileName := GetCurrentDir;
  if FreelancerPathDialog.Execute then
    FreelancerPathInput.Text := FreelancerPathDialog.FileName;
end;

procedure TInstallFrame.FlsrPathButtonClick(Sender: TObject);
begin
  if DirectoryExists(FlsrPathInput.Text) then
    FlsrPathDialog.FileName := FlsrPathInput.Text
  else
    FlsrPathDialog.FileName := GetCurrentDir;
  if FlsrPathDialog.Execute then
    FlsrPathInput.Text := FlsrPathDialog.FileName;
end;

procedure TInstallFrame.SetBundleMeta(const Meta: TBundleMeta);
begin
  FLastBundleMeta := Meta;
end;

procedure TInstallFrame.DisplayDownloadMetaErrors(const Errors: TTaskError);
begin
  ProgressError.Caption := '';
  case Errors.DownloadResult of
    TDownloadResult.Aborted: ProgressError.Caption := '';
    TDownloadResult.NoAccess: ProgressError.Caption := 'No access to download server to fetch mod information!';
    TDownloadResult.NotFound: ProgressError.Caption := 'Mod information not found on download server!';
    TDownloadResult.DownloadFailed: ProgressError.Caption := 'Downloading mod information failed!';
    TDownloadResult.WritingFailed,
    TDownloadResult.Success,
    TDownloadResult.Unknown: Assert(False);
  end;
  if ProgressError.Caption <> '' then
    ProgressError.Visible := True;
end;

procedure TInstallFrame.SetUpdateTaskDone(const Task: TTask; const Result: Boolean; const Errors: TTaskError);
begin
  case Task of
    TTask.DownloadMeta:
    begin
      if Result then
      begin
        SetUpInstallStep(TTask.VerifyFlsrInstallation);
        VerifyFlsrInstallation(GetSettings.LivePath, FLastBundleMeta, @SetUpdateTaskDone, @SetInstallProgress);
      end
      else
        DisplayDownloadMetaErrors(Errors);
    end;

    TTask.VerifyFlsrInstallation:
    begin
      if Result then
      begin
        MainForm.SetModStatus(TModStatus.Installed);
        SetUpInstallStep(TTask.None);
      end
      else
      begin
        ProgressError.Caption := '';
        if Errors.NoVersionFile then
        begin
          ProgressError.Caption := 'The mod is not installed at ' + GetSettings.LivePath;
          MainForm.SetModStatus(TModStatus.NotInstalled);
        end
        else if Errors.WrongVersion then
        begin
          ProgressError.Caption := 'The mod needs an update!';
          MainForm.SetModStatus(TModStatus.Outdated);
        end;
        if ProgressError.Caption <> '' then
          ProgressError.Visible := True;
      end;
    end;
  end;
end;

procedure TInstallFrame.SetInstallTaskDone(const Task: TTask; const Result: Boolean; const Errors: TTaskError);
const
  DownloadTempFile: String = 'flsr.temp';
var
  ValidFreelancerPath: String;
begin
  case Task of
    TTask.DownloadMeta:
    begin
      if Result then
        SetUpInstallStep(TTask.VerifyFreelancerAndFlsrPath)
      else
        DisplayDownloadMetaErrors(Errors);
    end;

    TTask.VerifyFreelancerAndFlsrPath:
    begin
      if Result then
      begin
        ValidFreelancerPath := FreelancerPathInput.Text;
        WriteFlsrPathToConfig(FlsrPathInput.Text);
        SetUpInstallStep(TTask.CopyFreelancer);
        CopyFreelancer(ValidFreelancerPath, GetSettings.LivePath, FLastBundleMeta, @SetInstallTaskDone, @SetInstallProgress);
      end
      else
      begin
        FreelancerPathError.Caption := '';
        FlsrPathError.Caption := '';
        if Errors.FreelancerInvalid then
          FreelancerPathError.Caption := 'Invalid installation. Make sure it contains an unmodified Freelancer installation!';
        if Errors.FlsrPathInvalid then
          FlsrPathError.Caption := 'Cannot write data. Make sure you are allowed to write files there, or chose another location!'
        else if Errors.MissingBytes > 0 then
          FlsrPathError.Caption := 'You need more space on your drive! Required space: ' + IntToStr(Math.Ceil(Errors.MissingBytes / (1024 * 1024 * 1024))) + ' GiBytes';

        if FreelancerPathError.Caption <> '' then
          FreelancerPathError.Visible := True;
        if FlsrPathError.Caption <> '' then
          FlsrPathError.Visible := True;

      end;
      PathsPanel.Enabled := True;
    end;

    TTask.CopyFreelancer:
    begin
      if Result then
      begin
        SetUpInstallStep(TTask.DownloadMod);
        DownloadMod(FLastBundleMeta, GetSettings.LivePath.TrimRight('\').TrimRight('/').Trim + DirectorySeparator + DownloadTempFile, @SetInstallTaskDone, @SetInstallProgress);
      end
      else
        ProgressError.Caption := 'Unable to copy file ' + Errors.InvalidPath + 'ake sure you can read the file and write it to the FL:SR directory!';
    end;

    TTask.DownloadMod:
    begin
      if Result then
      begin
        SetUpInstallStep(TTask.DecodeMod);
        DecodeMod(FLastBundleMeta, GetSettings.LivePath.TrimRight('\').TrimRight('/').Trim + DirectorySeparator + DownloadTempFile, GetSettings.LivePath.TrimRight('\').TrimRight('/').Trim, @SetInstallTaskDone, @SetInstallProgress);
      end
      else
      begin
        ProgressError.Caption := '';
        case Errors.DownloadResult of
          TDownloadResult.Aborted: ProgressError.Caption := '';
          TDownloadResult.NoAccess: ProgressError.Caption := 'No access to download server to fetch mod data!';
          TDownloadResult.NotFound: ProgressError.Caption := 'Mod data not found on download server!';
          TDownloadResult.DownloadFailed: ProgressError.Caption := 'Downloading mod contents failed!';
          TDownloadResult.WritingFailed: ProgressError.Caption := 'Writing mod contents failed! Make sure you can write files to the FL:SR directory!';
          TDownloadResult.ChecksumMismatch: ProgressError.Caption := 'Downloaded file contains errors. Please re-download!';
          TDownloadResult.Success,
          TDownloadResult.Unknown: Assert(False);
        end;
        if ProgressError.Caption <> '' then
          ProgressError.Visible := True;
      end;
    end;

    TTask.DecodeMod:
    begin
      if Result then
      begin
        MainForm.SetModStatus(TModStatus.Installed);
        SetUpInstallStep(TTask.None);
      end
      else
      begin
        ProgressError.Caption := '';
        case Errors.DecoderErrorType of
          TDecoderErrorType.Decoder: ProgressError.Caption := 'Decompressing file ' + Errors.DecoderErrorReason + ' failed!';
          TDecoderErrorType.WritingOutputPath: ProgressError.Caption := 'Writing file ' + Errors.DecoderErrorReason + ' failed! Make sure you can write files to the FL:SR directory!';
        end;
        if ProgressError.Caption <> '' then
          ProgressError.Visible := True;
      end;
    end;
  end;
end;

procedure TInstallFrame.SetInstallProgress(const MaxProgress, CurrentProgress: Int64);
var
  NormalizedProgress: Int16;
  CurrentMoment: Double;
begin
  if not ProgressPanel.Visible then
    Exit;
  CurrentMoment := Now;
  if MilliSecondsBetween(FLastProgressUpdate, CurrentMoment) > 50 then
  begin
    FLastProgressUpdate := CurrentMoment;
    NormalizedProgress := Math.Ceil(CurrentProgress / MaxProgress * 1000);
    ProgressBar.Position := NormalizedProgress;
    ProgressLabel.Caption := IntToStr(NormalizedProgress div 10) + '%';
  end;
end;

procedure TInstallFrame.SetUpInstallStep(const Task: TTask);
begin
  ProgressError.Visible := False;
  ProgressError.Caption := '';
  ProgressLabel.Caption := '0%';
  ProgressBar.Position := 0;

  case Task of
    TTask.None:
    begin
      ContinueButton.Enabled := False;
      PathsPanel.Visible := False;
      ProgressPanel.Visible := False;
    end;

    TTask.DownloadMeta:
    begin
      ContinueButton.Enabled := False;
      InstallHeadingLabel.Caption := 'Fetching Mod Information';
      PathsPanel.Visible := False;
      ProgressStepLabel.Caption := 'Downloading mod information…';
      ProgressPanel.Visible := True;
    end;

    TTask.VerifyFlsrInstallation:
    begin
      ContinueButton.Enabled := False;
      InstallHeadingLabel.Caption := 'Verifying Installation';
      PathsPanel.Visible := False;
      ProgressStepLabel.Caption := 'Checking installed mod contents…';
      ProgressPanel.Visible := True;
    end;

    TTask.VerifyFreelancerAndFlsrPath:
    begin
      ContinueButton.Enabled := True;
      InstallHeadingLabel.Caption := 'Preparing Installation';
      FreelancerPathInput.Text := '';
      FlsrPathInput.Text := GetSettings.LivePath;
      PathsPanel.Enabled := True;
      PathsPanel.Visible := True;
      ProgressPanel.Visible := False;
    end;

    TTask.CopyFreelancer:
    begin
      ContinueButton.Enabled := False;
      InstallHeadingLabel.Caption := 'Installing';
      PathsPanel.Visible := False;
      ProgressStepLabel.Caption := 'Copying Freelancer files to FL:SR directory…';
      ProgressPanel.Visible := True;
    end;

    TTask.DownloadMod:
    begin
      ContinueButton.Enabled := False;
      InstallHeadingLabel.Caption := 'Installing';
      PathsPanel.Visible := False;
      ProgressStepLabel.Caption := 'Downloading mod contents…';
      ProgressPanel.Visible := True;
    end;

    TTask.DecodeMod:
    begin
      ContinueButton.Enabled := False;
      InstallHeadingLabel.Caption := 'Installing';
      PathsPanel.Visible := False;
      ProgressStepLabel.Caption := 'Decompressing mod contents…';
      ProgressPanel.Visible := True;
    end;
  end;
end;

constructor TInstallFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FLastBundleMeta.InitEmpty;
  FLastProgressUpdate := Now;
end;

procedure TInstallFrame.BeginInstallWorkflow;
begin
  SetUpInstallStep(TTask.DownloadMeta);
  DownloadMeta(@SetBundleMeta, @SetInstallTaskDone, @SetInstallProgress);
end;

procedure TInstallFrame.BeginUpdateWorkflow;
begin
  SetUpInstallStep(TTask.DownloadMeta);
  DownloadMeta(@SetBundleMeta, @SetUpdateTaskDone, @SetInstallProgress);
end;

end.
