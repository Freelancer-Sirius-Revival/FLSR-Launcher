unit UMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  StdCtrls,
  UProcessing,
  BGRAVirtualScreen,
  BCImageButton,
  BGRABitmap,
  BGRABitmapTypes,
  BGRATextFX,
  BGRAFreeType,
  LazFreeTypeFontCollection;

type
  TMainForm = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private                                          
    BackgroundPanel: TBGRAVirtualScreen;     
    BackgroundImage: TBGRABitmap;
    FontCollection: TFreeTypeFontCollection;
    procedure BackgroundPanelRedraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure CloseButtonClick(Sender: TObject);
    procedure MinimizeButtonClick(Sender: TObject);
    procedure MoveButtonMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MoveButtonMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure MoveButtonMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);       
    procedure TspWebsiteButtonClick(Sender: TObject);
    procedure FlgcDiscordButtonClick(Sender: TObject);
    procedure FlsrDiscordButtonClick(Sender: TObject);
    procedure TitleButtonClick(Sender: TObject);
  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  {$IFDEF WINDOWS}
  windows,
  {$ENDIF}
  LCLIntf,
  fphttpclient,
  opensslsockets,
  UFormHeader;

function CreateMainFontRenderer: TBGRAFreeTypeFontRenderer;
begin
  Result := TBGRAFreeTypeFontRenderer.Create;
  Result.ShadowOffset.SetLocation(1, 1);
  Result.ShadowRadius := 2;
  Result.ShadowColor := BGRABlack;
  Result.ShadowVisible := True;
end;

function RenderText(const Width: Int32; const Height: Int32; const Size: Int32; const Text: String): TBGRABitmap;
var
  TextSize: TSize;
begin
  Result := TBGRABitmap.Create(Width, Height);
  Result.FontRenderer := CreateMainFontRenderer; // Font renderer is freed by bitmap.
  Result.FontName := 'Vibrocentric';
  Result.FontFullHeight := Size;
  Result.FontQuality := fqFineClearTypeRGB;
  TextSize := Result.TextSize(Text);
  Result.TextOut(
    Round(Result.Width / 2) - Round(TextSize.cx / 2),
    Round(Result.Height / 2) - Round(TextSize.cy / 2),
    Text,
    BGRAWhite
    );
  Result.FontRenderer := nil;
end;

procedure TMainForm.BackgroundPanelRedraw(Sender: TObject; Bitmap: TBGRABitmap);
var
  RenderedText: TBGRABitmap;
begin
  Bitmap.StretchPutImage(TRect.Create(0, 0, ScaleDesignToForm((Sender as TBGRAVirtualScreen).Width), ScaleDesignToForm((Sender as TBGRAVirtualScreen).Height)), BackgroundImage, TDrawMode.dmSet);
  //Bitmap.PutImage(0, 0, BackgroundImage, TDrawMode.dmSet);
  //Bitmap.PutImage(0, 0, TitleImage, TDrawMode.dmLinearBlend);
  RenderedText := RenderText(ScaleDesignToForm(BackgroundImage.Width), ScaleDesignToForm(BackgroundImage.Height), ScaleDesignToForm(64), 'Push him out of the airlock!');
  Bitmap.PutImage(0, 0, RenderedText, TDrawMode.dmLinearBlend);
  RenderedText.Free;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  Stream: TResourceStream;
begin
  BackgroundPanel := TBGRAVirtualScreen.Create(Self);
  BackgroundPanel.Parent := Self;
  BackgroundPanel.Width := Self.Width;
  BackgroundPanel.Height := Self.Height;
  BackgroundPanel.Align := TAlign.alClient;
  BackgroundPanel.OnRedraw := @BackgroundPanelRedraw;


  FontCollection := TFreeTypeFontCollection.Create;
  Stream := TResourceStream.Create(HInstance, 'VIBROCEN', RT_RCDATA);
  FontCollection.AddStream(Stream, True);
  SetDefaultFreeTypeFontCollection(FontCollection);

  Stream := TResourceStream.Create(HInstance, 'BACKGROUND', RT_RCDATA);
  BackgroundImage := TBGRABitmap.Create(Stream);
  Stream.Free;

  with CreateFormHeader(BackgroundPanel) do
  begin
    CloseButton.OnClick := @CloseButtonClick;
    MinimizeButton.OnClick := @MinimizeButtonClick;
    MoveButton.OnMouseDown := @MoveButtonMouseDown;
    MoveButton.OnMouseMove := @MoveButtonMouseMove;
    MoveButton.OnMouseUp := @MoveButtonMouseUp;
    TitleButton.OnClick := @TitleButtonClick;
    TspWebsiteButton.OnClick := @TspWebsiteButtonClick;
    FlgcDiscordButton.OnClick := @FlgcDiscordButtonClick;
    FlsrDiscordButton.OnClick := @FlsrDiscordButtonClick;
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  BackgroundImage.Free;
  FontCollection.Free;
  SetDefaultFreeTypeFontCollection(nil);
end;

procedure TMainForm.CloseButtonClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TMainForm.MinimizeButtonClick(Sender: TObject);
begin
  Self.WindowState := wsMinimized; // Makes the window disappear forever under Windows. TODO
  Application.Minimize; // Does not work on Linux because of BorderStyle=bsNone
end;

var
  OldPos: TPoint;
  Repositioning: Boolean;

procedure TMainForm.MoveButtonMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  OldPos.SetLocation(X, Y);
  Repositioning := True;
end;

procedure TMainForm.MoveButtonMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if Repositioning then
    Self.SetBounds(Left + X - OldPos.X, Top + Y - OldPos.Y, Width, Height);
end;

procedure TMainForm.MoveButtonMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Repositioning := False;
end;

procedure TMainForm.TspWebsiteButtonClick(Sender: TObject);
begin
  OpenURL('https://the-starport.net/');
end;

procedure TMainForm.FlgcDiscordButtonClick(Sender: TObject);
begin
  OpenURL('https://discord.com/invite/c6wtsBk');
end;

procedure TMainForm.FlsrDiscordButtonClick(Sender: TObject);
begin
  OpenURL('https://discord.gg/GMtyM57egU');
end;

procedure TMainForm.TitleButtonClick(Sender: TObject);
begin
  OpenURL('http://flsr.erikszeug.de');
end;

//procedure TMainForm.Button1Click(Sender: TObject);
////var
////  Stream: TStream;
//begin
//  //try
//  //  Stream:=TFileStream.Create('main.flsr', fmCreate);
//  //  TFPHttpClient.SimpleGet('http://flsr.erikszeug.de/files/main.flsr', Stream); // Throws Errors with the HTTP codes
//  //finally        
//  //  Stream.Free;
//  //end;


//  ProcessBundling('/home/eroe/Arbeit/Freelancer Sirius Revival/Update-Bundler/', '/home/eroe/Arbeit/Freelancer Sirius Revival/FLSR-Launcher/output');
//end;

end.
