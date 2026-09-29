unit UMainForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  ExtCtrls,
  Buttons,
  StdCtrls,
  ComCtrls,
  UInstallThread,
  UInstallFrame,
  UMeta;

type
  TMainForm = class(TForm)
  published
    DiscordButton: TBitBtn;
    FetchingDataInfoLabel: TLabel;
    ProgressPanel: TPanel;
    ServerStatusLabel: TLabel;
    LogoImage: TImage;
    MainButton: TSpeedButton;
    InstallFrame: TInstallFrame;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MainButtonClick(Sender: TObject);
    procedure MainButtonPaint(Sender: TObject);
    procedure DiscordButtonClick(Sender: TObject);
    procedure LogoImageClick(Sender: TObject);
  public
    procedure SetModInstalled;
  private
    FModInstalled: Boolean;
    FModMeta: TBundleMeta;
    procedure SetInstallTaskDone(const Task: TTask; const Result: Boolean; const Errors: TTaskError);
    procedure ShowPlayersOnline(const Count: Int32);
  public

  end;

var
  MainForm: TMainForm;

implementation

uses
  LCLIntf,
  FileUtil,
  DateUtils,
  UPlayersOnline,
  UDownloading;

  {$R *.lfm}

procedure TMainForm.MainButtonPaint(Sender: TObject);
var
  Area: TRect;
  TextStyle: TTextStyle;
  ButtonText: String;
begin
  Area := Rect(0, 0, MainButton.Width, MainButton.Height);
  TextStyle := MainButton.Canvas.TextStyle;
  TextStyle.Alignment := taCenter;
  TextStyle.Layout := tlCenter;
  MainButton.Canvas.Font.Size := 28;
  MainButton.Canvas.Font.Color := clWhite;
  if FModInstalled then
    ButtonText := 'Launch Game'
  else
    ButtonText := 'Install';
  MainButton.Canvas.TextRect(Area, 0, 0, ButtonText, TextStyle);
end;

procedure TMainForm.MainButtonClick(Sender: TObject);
begin
  InstallFrame.Visible := True;
  MainButton.Visible := False;
  InstallFrame.SetUp;
end;

procedure TMainForm.LogoImageClick(Sender: TObject);
begin
  OpenURL('http://fl-sr.eu');
end;

procedure TMainForm.SetModInstalled;
begin
  FModInstalled := True;
  MainButton.Visible := True;
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

procedure TMainForm.SetInstallTaskDone(const Task: TTask; const Result: Boolean; const Errors: TTaskError);
begin
  case Task of
    TTask.DownloadMeta:
    begin
      if Result and GetBundleMeta(FModMeta) then
      begin
        FetchingDataInfoLabel.Visible := False;
        MainButton.Visible := True;
      end
      else
      begin
        FModMeta.InitEmpty;
        FetchingDataInfoLabel.Caption := '';
        case Errors.DownloadResult of
          TDownloadResult.NoAccess: FetchingDataInfoLabel.Caption := 'No access to download server to fetch mod information!';
          TDownloadResult.NotFound: FetchingDataInfoLabel.Caption := 'Mod information not found on download server!';
          TDownloadResult.DownloadFailed: FetchingDataInfoLabel.Caption := 'Downloading mod information failed!';
        end;
        if FetchingDataInfoLabel.Caption <> '' then
          FetchingDataInfoLabel.Visible := True;
      end;
    end;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FModInstalled := False;
  FModMeta.InitEmpty;
  ListenForPlayersOnline(@ShowPlayersOnline);
  CreateInstallThread;
  FetchingDataInfoLabel.Caption := 'Fetching Mod Information…';
  FetchingDataInfoLabel.Visible := True;
  DownloadMeta(@SetInstallTaskDone, nil);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  StopListeningForPlayersOnline;
  TerminateInstallThread;
end;

end.
