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
    ValidFreelancerPath: String;
    ValidFlsrPath: String;
    BundleMeta: TBundleMeta;
    procedure SetInstallTaskDone(const Task: TTask; const Result: Boolean; const Errors: TTaskError);
    procedure SetInstallProgress(const MaxProgress, CurrentProgress: Int64);
    procedure SetUpInstallStep(const FinishedTask: TTask);
  public
    constructor Create(TheOwner: TComponent); override;
    procedure SetUp;
  end;

implementation

uses
  UMainForm,
  LCLIntf,
  FileUtil,
  DateUtils,
  Math,
  UDecoding,
  UDownloading;

  {$R *.lfm}

procedure TInstallFrame.CancelButtonClick(Sender: TObject);
begin
  MainForm.InstallFrame.Visible := False;
  //InstallButton.Visible := True;
  //ProgressPanel.Visible := False;
end;

procedure TInstallFrame.ContinueButtonClick(Sender: TObject);
begin
  ContinueButton.Enabled := False;
  FreelancerPathError.Visible := False;
  FlsrPathError.Visible := False;
  PathsPanel.Enabled := False;
  VerifyFreelancerAndFlsr(FreelancerPathInput.Text, FlsrPathInput.Text, @SetInstallTaskDone, @SetInstallProgress);
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

procedure TInstallFrame.SetInstallTaskDone(const Task: TTask; const Result: Boolean; const Errors: TTaskError);
const
  DownloadTempFile: String = 'flsr.temp';
begin
  case Task of
    TTask.DownloadMeta:
    begin
      if Result and GetBundleMeta(BundleMeta) then
        SetUpInstallStep(TTask.VerifyFreelancerAndFlsr)
      else
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
    end;

    TTask.VerifyFreelancerAndFlsr:
    begin
      if Result then
      begin
        ValidFreelancerPath := FreelancerPathInput.Text;
        ValidFlsrPath := FlsrPathInput.Text;
        SetUpInstallStep(TTask.CopyFreelancer);
        CopyFreelancer(ValidFreelancerPath, ValidFlsrPath, @SetInstallTaskDone, @SetInstallProgress);
      end
      else
      begin
        FreelancerPathError.Visible := True;
        if Errors.FreelancerInvalid then
          FreelancerPathError.Caption := 'Invalid installation. Make sure it contains an unmodified english Freelancer installation!';
        if Errors.FlsrInvalid then
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
        DownloadMod(BundleMeta, ValidFlsrPath.TrimRight('\').TrimRight('/').TrimRight + DirectorySeparator + DownloadTempFile, @SetInstallTaskDone, @SetInstallProgress);
      end
      else
      begin
         ProgressError.Caption := 'Unable to copy file ' + Errors.InvalidPath + 'ake sure you can read the file and write it to the FL:SR directory!';
      end;
    end;

    TTask.DownloadMod:
    begin
      if Result then
      begin
        SetUpInstallStep(TTask.DecodeMod);
        DecodeMod(BundleMeta, ValidFlsrPath.TrimRight('\').TrimRight('/').TrimRight + DirectorySeparator + DownloadTempFile, ValidFlsrPath.TrimRight('\').TrimRight('/').TrimRight, @SetInstallTaskDone, @SetInstallProgress);
      end
      else
      begin
        ProgressError.Caption := '';
        case Errors.DownloadResult of
          TDownloadResult.Aborted: ProgressError.Caption := '';
          TDownloadResult.NoAccess: ProgressError.Caption := 'No access to download server to fetch mod data!';
          TDownloadResult.NotFound: ProgressError.Caption := 'Mod data not found on download server!';
          TDownloadResult.DownloadFailed: ProgressError.Caption := 'Downloading mod data failed!';
          TDownloadResult.WritingFailed: ProgressError.Caption := 'Writing mod data failed! Make sure you can write files to the FL:SR directory!';
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
        MainForm.SetModInstalled;
        MainForm.InstallFrame.Visible := False;
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
  ContinueButton.Enabled := True;
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

procedure TInstallFrame.SetUpInstallStep(const FinishedTask: TTask);
begin
  case FinishedTask of
    TTask.None:
    begin
      PathsPanel.Visible := False;
      ProgressPanel.Visible := False;
    end;

    TTask.DownloadMeta:
    begin
      InstallHeadingLabel.Caption := 'Preparing Installation';
      PathsPanel.Visible := False;
      ProgressStepLabel.Caption := 'Downloading Mod Information…';
      ProgressPanel.Visible := True;
    end;

    TTask.VerifyFreelancerAndFlsr:
    begin
      InstallHeadingLabel.Caption := 'Preparing Installation';
      FreelancerPathInput.Text := '';
      FlsrPathInput.Text := '';
      PathsPanel.Enabled := True;
      PathsPanel.Visible := True;
      ProgressPanel.Visible := False;
    end;

    TTask.CopyFreelancer:
    begin
      InstallHeadingLabel.Caption := 'Installing';
      PathsPanel.Visible := False;
      ProgressStepLabel.Caption := 'Copying Freelancer files to FL:SR directory…';
      ProgressBar.Position := 0;
      ProgressLabel.Caption := '0%';
      ProgressPanel.Visible := True;
    end;

    TTask.DownloadMod:
    begin
      InstallHeadingLabel.Caption := 'Installing';
      PathsPanel.Visible := False;
      ProgressStepLabel.Caption := 'Downloading Mod Content…';
      ProgressBar.Position := 0;
      ProgressLabel.Caption := '0%';
      ProgressPanel.Visible := True;
    end;

    TTask.DecodeMod:
    begin
      InstallHeadingLabel.Caption := 'Installing';
      PathsPanel.Visible := False;
      ProgressStepLabel.Caption := 'Decompressing Mod Content…';
      ProgressBar.Position := 0;
      ProgressLabel.Caption := '0%';
      ProgressPanel.Visible := True;
    end;
  end;
end;

constructor TInstallFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FLastProgressUpdate := Now;
end;

procedure TInstallFrame.SetUp;
begin
  SetUpInstallStep(TTask.DownloadMeta);
  DownloadMeta(@SetInstallTaskDone, @SetInstallProgress);
end;

end.
