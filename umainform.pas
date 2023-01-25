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
  LazFreeTypeFontCollection,
  UFormHeader;

type
  TMainForm = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    BackgroundPanel: TBGRAVirtualScreen;
    BackgroundImage: TBGRABitmap;
    FrameTopImage: TBGRABitmap;
    FrameRightImage: TBGRABitmap;
    FrameBottomImage: TBGRABitmap;
    FrameLeftImage: TBGRABitmap;
    TopRightFrameImage: TBGRABitmap;
    TopLeftFrameImage: TBGRABitmap;
    BottomRightFrameImage: TBGRABitmap;
    BottomLeftFrameImage: TBGRABitmap;
    FontCollection: TFreeTypeFontCollection;
    FormHeader: TFormHeader;
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
    procedure SetUpFonts;
    procedure SetUpBackground;
    procedure ShowPlayersOnline(const Count: Int32);
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
  UResourceLoading,
  UPlayersOnline,
  URenderText;

//function CreateMainFontRenderer: TBGRAFreeTypeFontRenderer;
//begin
//  Result := TBGRAFreeTypeFontRenderer.Create;
//  Result.ShadowOffset.SetLocation(1, 1);
//  Result.ShadowRadius := 2;
//  Result.ShadowColor := BGRABlack;
//  Result.ShadowVisible := True;
//end;

//function RenderText(const Width: Int32; const Height: Int32; const Size: Int32; const Text: String): TBGRABitmap;
//begin
//  Result := TBGRABitmap.Create(Width, Height);
//  Result.FontRenderer := CreateMainFontRenderer; // Font renderer is freed by bitmap.
//  Result.FontName := 'Vibrocentric';
//  Result.FontFullHeight := Size;
//  Result.FontQuality := fqFineClearTypeRGB;
//  Result.TextMultiline(0, 0, 200, Text, BGRAWhite);
//  Result.FontRenderer := nil;
//end;

procedure TMainForm.BackgroundPanelRedraw(Sender: TObject; Bitmap: TBGRABitmap);
//var
//  RenderedText: TBGRABitmap;
var
  ScaledBackgroundWidth: Int32;
  ScaledBackgroundHeight: Int32;
begin
  ScaledBackgroundWidth := ScaleDesignToForm(BackgroundPanel.Width);       
  ScaledBackgroundHeight := ScaleDesignToForm(BackgroundPanel.Height);

  Bitmap.StretchPutImage(TRect.Create(0, 0, ScaledBackgroundWidth, ScaledBackgroundHeight), BackgroundImage, TDrawMode.dmSet);

  Bitmap.StretchPutImage(TRect.Create(0, 0, ScaleDesignToForm(FrameLeftImage.Width), ScaledBackgroundHeight), FrameLeftImage, TDrawMode.dmLinearBlend);
  Bitmap.StretchPutImage(TRect.Create(0, 0, ScaledBackgroundWidth, ScaleDesignToForm(FrameTopImage.Height)), FrameTopImage, TDrawMode.dmLinearBlend);
  Bitmap.StretchPutImage(TRect.Create(ScaledBackgroundWidth - ScaleDesignToForm(FrameRightImage.Width), 0, ScaledBackgroundWidth, ScaledBackgroundHeight), FrameRightImage, TDrawMode.dmLinearBlend);
  Bitmap.StretchPutImage(TRect.Create(0, ScaledBackgroundHeight - ScaleDesignToForm(FrameBottomImage.Height), ScaledBackgroundWidth, ScaledBackgroundHeight), FrameBottomImage, TDrawMode.dmLinearBlend);

  Bitmap.StretchPutImage(TRect.Create(0, 0, ScaleDesignToForm(TopLeftFrameImage.Width), ScaleDesignToForm(TopLeftFrameImage.Height)), TopLeftFrameImage, TDrawMode.dmDrawWithTransparency);
  Bitmap.StretchPutImage(TRect.Create(ScaledBackgroundWidth - ScaleDesignToForm(TopRightFrameImage.Width), 0, ScaledBackgroundWidth, ScaleDesignToForm(TopRightFrameImage.Height)), TopRightFrameImage, TDrawMode.dmDrawWithTransparency);

  Bitmap.StretchPutImage(TRect.Create(0, ScaledBackgroundHeight - ScaleDesignToForm(BottomLeftFrameImage.Height), ScaleDesignToForm(BottomLeftFrameImage.Width), ScaledBackgroundHeight), BottomLeftFrameImage, TDrawMode.dmDrawWithTransparency);
  Bitmap.StretchPutImage(TRect.Create(ScaledBackgroundWidth - ScaleDesignToForm(BottomRightFrameImage.Width), ScaledBackgroundHeight - ScaleDesignToForm(BottomRightFrameImage.Height), ScaledBackgroundWidth, ScaledBackgroundHeight), BottomRightFrameImage, TDrawMode.dmDrawWithTransparency);

  //RenderedText := RenderText(ScaleDesignToForm(BackgroundImage.Width), ScaleDesignToForm(BackgroundImage.Height), ScaleDesignToForm(32), 'Push him out of the airlock!');
  //RenderedText := RenderText(ScaleDesignToForm(BackgroundImage.Width), ScaleDesignToForm(BackgroundImage.Height), ScaleDesignToForm(32), (Sender as TControl).Caption);
  //Bitmap.PutImage(0, 0, RenderedText, TDrawMode.dmLinearBlend);
  //RenderedText.Free;
end;

procedure TMainForm.SetUpFonts;
//var
//  Stream: TStream;
begin
  //FontCollection := TFreeTypeFontCollection.Create;
  //Stream := LoadResource('VIBROCEN');
  //if Assigned(Stream) then
  //begin
  //  FontCollection.AddStream(Stream, True);
  //  SetDefaultFreeTypeFontCollection(FontCollection);
  //end;
end;

procedure TMainForm.SetUpBackground;
var
  Stream: TStream;
begin
  Stream := LoadResource('BACKGROUND');
  if Assigned(Stream) then
  begin
    BackgroundImage := TBGRABitmap.Create(Stream);
    Stream.Free;
  end;

  Stream := LoadResource('FRAME');
  if Assigned(Stream) then
  begin
    FrameRightImage := TBGRABitmap.Create(Stream);
    Stream.Free;
    FrameTopImage := FrameRightImage.RotateCCW;
    FrameBottomImage := FrameRightImage.RotateCW;
    FrameLeftImage := TBGRABitmap.Create(FrameRightImage);
    FrameLeftImage.HorizontalFlip;
  end;

  Stream := LoadResource('FRAME_TOPRIGHT');
  if Assigned(Stream) then
  begin
    TopRightFrameImage := TBGRABitmap.Create(Stream);
    Stream.Free;
    TopLeftFrameImage := TBGRABitmap.Create(TopRightFrameImage);
    TopLeftFrameImage.HorizontalFlip;
  end;

  Stream := LoadResource('FRAME_BOTTOMRIGHT');
  if Assigned(Stream) then
  begin
    BottomRightFrameImage := TBGRABitmap.Create(Stream);
    Stream.Free;
    BottomLeftFrameImage := TBGRABitmap.Create(BottomRightFrameImage);
    BottomLeftFrameImage.HorizontalFlip;
  end;

  BackgroundPanel := TBGRAVirtualScreen.Create(Self);
  BackgroundPanel.Parent := Self;
  BackgroundPanel.Width := Self.Width;
  BackgroundPanel.Height := Self.Height;
  BackgroundPanel.Align := TAlign.alClient;
  BackgroundPanel.OnRedraw := @BackgroundPanelRedraw;
  //BackgroundPanel.Color := cl
end;

procedure TMainForm.ShowPlayersOnline(const Count: Int32);
begin
  if not Assigned(FormHeader.OnlinePlayersPanel) then
    Exit;
  if Count < 0 then
    FormHeader.OnlinePlayersPanel.Caption := 'Server currently offline.'
  else if Count = 0 then
    FormHeader.OnlinePlayersPanel.Caption := 'No players on the server.'
  else if Count = 1 then
    FormHeader.OnlinePlayersPanel.Caption := Count.ToString + ' player on the server.'
  else
    FormHeader.OnlinePlayersPanel.Caption := Count.ToString + ' players on the server.';
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  SetUpFonts;
  SetUpBackground;

  FormHeader := CreateFormHeader(BackgroundPanel);
  with FormHeader do
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
  ListenForPlayersOnline(@ShowPlayersOnline);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  StopListeningForPlayersOnline;
  BackgroundImage.Free;
  FrameTopImage.Free;
  FrameRightImage.Free;
  FrameBottomImage.Free;
  FrameLeftImage.Free;
  TopRightFrameImage.Free;
  TopLeftFrameImage.Free;
  BottomRightFrameImage.Free;
  BottomLeftFrameImage.Free;
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
