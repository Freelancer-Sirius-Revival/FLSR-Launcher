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
  BGRABitmap,
  BGRABitmapTypes,
  BGRATextFX,
  BGRAFreeType,
  LazFreeTypeFontCollection;

type
  TMainForm = class(TForm)
    BGRAVirtualScreen1: TBGRAVirtualScreen;
    procedure BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FontCollection: TFreeTypeFontCollection;
    BackgroundImage: TBGRABitmap;
    TitleImage: TBGRABitmap;

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  fphttpclient,
  opensslsockets;

function CreateMainFontRenderer: TBGRAFreeTypeFontRenderer;
begin
  Result := TBGRAFreeTypeFontRenderer.Create;
  Result.ShadowOffset := Point(1, 1);
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

procedure TMainForm.BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
var
  RenderedText: TBGRABitmap;
begin
  Bitmap.PutImage(0, 0, BackgroundImage, TDrawMode.dmSet);
  Bitmap.PutImage(0, 0, TitleImage, TDrawMode.dmLinearBlend);
  RenderedText := RenderText(BackgroundImage.Width, BackgroundImage.Height, 64, 'Push him out of the airlock!');
  Bitmap.PutImage(0, 0, RenderedText, TDrawMode.dmLinearBlend);
  RenderedText.Free;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  Stream: TResourceStream;
begin
  FontCollection := TFreeTypeFontCollection.Create;
  Stream := TResourceStream.Create(HInstance, 'VIBROCEN', RT_RCDATA);
  FontCollection.AddStream(Stream, True);
  SetDefaultFreeTypeFontCollection(FontCollection);

  Stream := TResourceStream.Create(HInstance, 'BACKGROUND', RT_RCDATA);
  BackgroundImage := TBGRABitmap.Create(Stream);
  Stream.Free;

  Stream := TResourceStream.Create(HInstance, 'TITLE', RT_RCDATA);
  TitleImage := TBGRABitmap.Create(Stream);
  Stream.Free;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  BackgroundImage.Free;
  TitleImage.Free;
  FontCollection.Free;
  SetDefaultFreeTypeFontCollection(nil);
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
