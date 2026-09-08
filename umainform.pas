unit UMainForm;

{$mode objfpc}
{$H+}
{$ScopedEnums on}

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
  StdCtrls, ComCtrls;

type
  TInstallationStep = (None, SelectingVanillaFL, SelectingFLSRTargetDir, Downloading, Unpacking);

  TMainForm = class(TForm)
    ProgressErrorLabel: TLabel;
    ProgressInputButton: TButton;
    ProgressContinueButton: TButton;
    ProgressCancelInstallButton: TButton;
    ProgressInputEdit: TEdit;
    DiscordButton: TBitBtn;
    ProgressHeadingLabel: TLabel;
    ProgressInstructionsLabel: TLabel;
    ProgressPanel: TPanel;
    SelectFLSRTargetDirectoryDialog: TSelectDirectoryDialog;
    SelectVanillaFLDirectoryDialog: TSelectDirectoryDialog;
    ServerStatusLabel: TLabel;
    LogoImage: TImage;
    InstallButton: TSpeedButton;
    procedure DiscordButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LogoImageClick(Sender: TObject);
    procedure InstallButtonClick(Sender: TObject);
    procedure InstallButtonPaint(Sender: TObject);
    procedure ProgressCancelInstallButtonClick(Sender: TObject);
    procedure ProgressContinueButtonClick(Sender: TObject);
    procedure ProgressInputButtonClick(Sender: TObject);
  private
    procedure ShowPlayersOnline(const Count: Int32);
    procedure PrepareInstallationStep;
  var
    CurrentInstallationStep: TInstallationStep;
    FreelancerVanillaPath: String;
    FlsrPath: String;
  public

  end;

var
  MainForm: TMainForm;

implementation

uses
  LCLIntf,
  FileUtil,
  UPlayersOnline,
  UProgress,
  UMeta,
  UBundle,
  UBundleDownload,
  UDecoding,
  UInstallSteps;

  {$R *.lfm}

procedure TMainForm.LogoImageClick(Sender: TObject);
begin
  OpenURL('http://fl-sr.eu');
end;

procedure TMainForm.PrepareInstallationStep;
begin
  ProgressErrorLabel.Visible := False;
  ProgressErrorLabel.Caption := '';

  case CurrentInstallationStep of
    TInstallationStep.SelectingVanillaFL:
    begin
      ProgressHeadingLabel.Caption := 'Preparing Installation';
      ProgressInstructionsLabel.Caption := 'Select an unmodified english original installation of Freelancer.';
      ProgressInputEdit.Text := '';
      ProgressInputEdit.Visible := True;
    end;

    TInstallationStep.SelectingFLSRTargetDir:
    begin
      ProgressHeadingLabel.Caption := 'Preparing Installation';
      ProgressInstructionsLabel.Caption := 'Select the directory to install Freelancer: Sirius Revival into.';
      ProgressInputEdit.Text := GetCurrentDir;
      ProgressInputEdit.Visible := True;
    end;

    TInstallationStep.Downloading:
    begin
      ProgressHeadingLabel.Caption := 'Downloading Mod Files';
      ProgressInstructionsLabel.Caption := 'Please wait while the mod files are being downloaded.';
      ProgressInputEdit.Visible := False;
      ProgressInputEdit.Text := '';
    end;

    TInstallationStep.Unpacking:
    begin
      ProgressHeadingLabel.Caption := 'Unpacking Mod Files';
      ProgressInstructionsLabel.Caption := 'Please wait while the mod files are being installed.';
      ProgressInputEdit.Visible := False;
      ProgressInputEdit.Text := '';
    end;
  end;

  ProgressPanel.Visible := True;
end;

procedure TMainForm.InstallButtonClick(Sender: TObject);
begin
  InstallButton.Visible := False;
  CurrentInstallationStep := TInstallationStep.SelectingVanillaFL;
  PrepareInstallationStep;
end;

procedure TMainForm.InstallButtonPaint(Sender: TObject);
var
  Area: TRect;
  TextStyle: TTextStyle;
begin
  Area := Rect(0, 0, InstallButton.Width, InstallButton.Height);
  TextStyle := InstallButton.Canvas.TextStyle;
  TextStyle.Alignment := taCenter;
  TextStyle.Layout := tlCenter;
  InstallButton.Canvas.Font.Size := 28;
  InstallButton.Canvas.TextRect(Area, 0, 0, 'Install', TextStyle);
end;

procedure TMainForm.ProgressCancelInstallButtonClick(Sender: TObject);
begin
  CurrentInstallationStep := TInstallationStep.None;
  InstallButton.Visible := True;
  ProgressPanel.Visible := False;
end;

procedure TMainForm.ProgressContinueButtonClick(Sender: TObject);
var
  Process: TProcessProgress;
  Meta: TBundleMeta;
  Bundle: TBundle;
  Index: ValSInt;
  FreelancerFiles: TStringList;
  TempFlsrPath: String;
  MissingDiskSpace: Int64;
  DirectoryAlreadyExisted: Boolean;
  BundleStream: TStream;
begin
  case CurrentInstallationStep of
    TInstallationStep.SelectingVanillaFL:
    begin
      if ValidateOriginalFreelancer(ProgressInputEdit.Text) then
      begin
        FreelancerVanillaPath := ProgressInputEdit.Text;
        CurrentInstallationStep := TInstallationStep.SelectingFLSRTargetDir;
        PrepareInstallationStep;
      end
      else
      begin
        ProgressErrorLabel.Visible := True;
        ProgressErrorLabel.Caption := 'No valid Freelancer installation was found in the directory!';
      end;
    end;

    TInstallationStep.SelectingFLSRTargetDir:
    begin
      TempFlsrPath := String(ProgressInputEdit.Text).Trim.Replace('/', DirectorySeparator, [rfReplaceAll]).Replace('\', DirectorySeparator, [rfReplaceAll]);

      DirectoryAlreadyExisted := DirectoryExists(TempFlsrPath);

      if not DirectoryAlreadyExisted and not ForceDirectories(TempFlsrPath) then
      begin
        ProgressErrorLabel.Visible := True;
        ProgressErrorLabel.Caption := 'The path could not be created. Check your permissions, or try another location to install to.';
      end;

      if not ValidateCanCreateFiles(TempFlsrPath) then
      begin
        ProgressErrorLabel.Visible := True;
        ProgressErrorLabel.Caption := 'No files can be modified under that path. Check your permissions, or try another location to install to.';

        if not DirectoryAlreadyExisted then
          DeleteDirectory(TempFlsrPath, False);

        Exit;
      end;

      FlsrPath := TempFlsrPath;

      Process := DownloadMetaData(Meta);
      while not Process.Done do ;
      Process.Free;

      if (Meta.BundleFileSize = 0) and (Meta.FileEntries = nil) then
      begin
        ProgressErrorLabel.Caption := 'Information about the mod could not be downloaded.';
        Exit;
      end;

      FreelancerFiles := FindOriginalFilesToCopy(FreelancerVanillaPath, Meta.FileEntries);
      MissingDiskSpace := EvaluateMissingDiskSpace(FreelancerFiles, TempFlsrPath, Meta.FileEntries);
      if MissingDiskSpace > 0 then
      begin
        ProgressErrorLabel.Caption := 'You do not have enough space on your drive. Required: ' + IntToStr(MissingDiskSpace div 1024 div 1024) + ' MiB';
        ProgressErrorLabel.Visible := True;
        Exit;
      end;

      for Index := 0 to FreelancerFiles.Count - 1 do
        if not CopyFile(FreelancerVanillaPath + FreelancerFiles.Strings[Index], TempFlsrPath + FreelancerFiles.Strings[Index], [cffOverwriteFile, cffCreateDestDirectory, cffPreserveTime]) then
        begin
          ProgressErrorLabel.Caption := 'Original Freelancer files could not be copied to the FL:SR directory.';
          ProgressErrorLabel.Visible := True;
          Exit;
        end;

      FreelancerFiles.Free;

      CurrentInstallationStep := TInstallationStep.Downloading;
      PrepareInstallationStep;
    end;

    TInstallationStep.Downloading:
    begin
      WriteLn('Downloading now');
      Process := DownloadModData(FlsrPath);
      while not Process.Done do ;
      Process.Free;
                                
      CurrentInstallationStep := TInstallationStep.Unpacking;
      PrepareInstallationStep;
    end;

    TInstallationStep.Unpacking:
    begin
      WriteLn('Unpacking now');
      BundleStream := TFileStream.Create(FlsrPath + '/temp.flsr', fmOpenRead or fmShareDenyWrite);
      Bundle := ReadBundleMetaData(BundleStream);
      DecodeFilesChunks(Bundle.FilesChunks, BundleStream, FlsrPath + '/decoded');
      BundleStream.Free;
      WriteLn('Done unpacking');
    end;
  end;
end;

procedure TMainForm.ProgressInputButtonClick(Sender: TObject);
begin
  case CurrentInstallationStep of
    TInstallationStep.SelectingVanillaFL:
    begin
      if DirectoryExists(ProgressInputEdit.Text) then
        SelectVanillaFLDirectoryDialog.FileName := ProgressInputEdit.Text
      else
        SelectVanillaFLDirectoryDialog.FileName := GetCurrentDir;
      if SelectVanillaFLDirectoryDialog.Execute then
        ProgressInputEdit.Text := SelectVanillaFLDirectoryDialog.FileName;
    end;

    TInstallationStep.SelectingFLSRTargetDir:
    begin
      if DirectoryExists(ProgressInputEdit.Text) then
        SelectFLSRTargetDirectoryDialog.FileName := ProgressInputEdit.Text
      else
        SelectFLSRTargetDirectoryDialog.FileName := GetCurrentDir;
      if SelectFLSRTargetDirectoryDialog.Execute then
        ProgressInputEdit.Text := SelectVanillaFLDirectoryDialog.FileName;
    end;
  end;
end;

procedure TMainForm.DiscordButtonClick(Sender: TObject);
begin
  OpenURL('https://discord.gg/k6e4yFrKDU');
end;

procedure TMainForm.ShowPlayersOnline(const Count: Int32);
begin
  if not Assigned(ServerStatusLabel) then
    Exit;
  if Count < 0 then
    ServerStatusLabel.Caption := 'Game server offline.'
  else if Count = 0 then
    ServerStatusLabel.Caption := 'No freelancers playing.'
  else if Count = 1 then
    ServerStatusLabel.Caption := Count.ToString + ' freelancer playing.'
  else
    ServerStatusLabel.Caption := Count.ToString + ' freelancers playing.';
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  ListenForPlayersOnline(@ShowPlayersOnline);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  StopListeningForPlayersOnline;
end;

end.
