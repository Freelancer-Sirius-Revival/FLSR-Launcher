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
  UInstallFrame;

type
  TModStatus = (NotInstalled, Installed, Outdated);

  TMainForm = class(TForm)
  published
    DiscordButton: TBitBtn;
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
  private
    FModStatus: TModStatus;
    procedure ShowPlayersOnline(const Count: Int32);
  public
    procedure SetModStatus(const ModStatus: TModStatus);
  end;

var
  MainForm: TMainForm;

implementation

uses
  LCLIntf,
  FileUtil,
  DateUtils,
  UPlayersOnline,
  UDownloading,
  USettings;

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
  case FModStatus of
    TModStatus.NotInstalled: ButtonText := 'Install';
    TModStatus.Installed: ButtonText := 'Launch';
    TModStatus.Outdated: ButtonText := 'Update';
  end;
  MainButton.Canvas.TextRect(Area, 0, 0, ButtonText, TextStyle);
end;

procedure TMainForm.MainButtonClick(Sender: TObject);
begin                      
  MainButton.Enabled := False;
  case FModStatus of
    TModStatus.NotInstalled:
    begin
      InstallFrame.BeginInstallWorkflow;       
      MainButton.Visible := False;
      InstallFrame.Visible := True;
    end;
    TModStatus.Installed: ;
    TModStatus.Outdated:
    begin
      InstallFrame.BeginInstallWorkflow;
      MainButton.Visible := False;
      InstallFrame.Visible := True;
    end;
  end;
end;

procedure TMainForm.LogoImageClick(Sender: TObject);
begin
  OpenURL('http://fl-sr.eu');
end;

procedure TMainForm.SetModStatus(const ModStatus: TModStatus);
begin
  FModStatus := ModStatus;
  MainButton.Enabled := True;
  MainButton.Visible := True;
  InstallFrame.Visible := False;
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
  ReadSettings;
  FModStatus := NotInstalled;
  ListenForPlayersOnline(@ShowPlayersOnline);
  CreateInstallThread;
  InstallFrame.BeginUpdateWorkflow;
  InstallFrame.Visible := True;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  StopListeningForPlayersOnline;
  TerminateInstallThread;
end;

end.
