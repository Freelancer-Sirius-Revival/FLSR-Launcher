unit UBitmapButton;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  Controls,
  BGRABitmap,
  BCImageButton;

type
  TBitmapButton = class(TBCXButton)
  private
    FCaptionSize: Int32;
    FDefaultImage: TBGRABitmap;
    FActiveImage: TBGRABitmap;
    procedure SetDefaultImage(const NewDefaultImageResourceName: String);
    procedure SetActiveImage(const NewActiveImageResourceName: String);
    procedure DoRenderControl(Sender: TObject; Bitmap: TBGRABitmap; State: TBCGraphicButtonState);
  public
    property DefaultImageResourceName: String write SetDefaultImage;
    property ActiveImageResourceName: String write SetActiveImage;
    property CaptionSize: Int32 read FCaptionSize write FCaptionSize;
    constructor Create(const AOwner: TWinControl);
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  UResourceLoading,
  URenderText,
  BGRAGraphics,
  BGRABitmapTypes;

procedure TBitmapButton.SetDefaultImage(const NewDefaultImageResourceName: String);
var
  Stream: TStream;
begin
  if Assigned(FDefaultImage) then
    FDefaultImage.Free;
  Stream := LoadResource(NewDefaultImageResourceName);
  if Assigned(Stream) then
  begin
    FDefaultImage := TBGRABitmap.Create(Stream);
    Stream.Free;
  end;
end;

procedure TBitmapButton.SetActiveImage(const NewActiveImageResourceName: String);
var
  Stream: TStream;
begin
  if Assigned(FActiveImage) then
    FActiveImage.Free;
  Stream := LoadResource(NewActiveImageResourceName);
  if Assigned(Stream) then
  begin
    FActiveImage := TBGRABitmap.Create(Stream);
    Stream.Free;
  end;
end;

procedure TBitmapButton.DoRenderControl(Sender: TObject; Bitmap: TBGRABitmap; State: TBCGraphicButtonState);
var
  CaptionBitmap: TBGRABitmap;
  CaptionColor: TBGRAPixel;
begin
  CaptionColor.FromRGB(135, 195, 224);
  case State of
    gbsNormal:
    begin
      if Assigned(FDefaultImage) then
        Bitmap.StretchPutImage(TRect.Create(0, 0, ScaleDesignToForm(Width), ScaleDesignToForm(Height)), FDefaultImage, TDrawMode.dmLinearBlend);
    end;
    gbsActive, gbsHover:
    begin
      CaptionColor.FromRGB(245, 234, 82);
      if Assigned(FActiveImage) then
        Bitmap.StretchPutImage(TRect.Create(0, 0, ScaleDesignToForm(Width), ScaleDesignToForm(Height)), FActiveImage, TDrawMode.dmLinearBlend)
      else if Assigned(FDefaultImage) then
        Bitmap.StretchPutImage(TRect.Create(0, 0, ScaleDesignToForm(Width), ScaleDesignToForm(Height)), FDefaultImage, TDrawMode.dmLinearBlend);
    end;
  end;
  if Caption <> '' then
  begin
    CaptionBitmap := RenderText(ScaleDesignToForm(Width), ScaleDesignToForm(Height), ScaleDesignToForm(FCaptionSize), 'Agency FB', [], CaptionColor, UpperCase(Caption));
    Bitmap.PutImage(0, 0, CaptionBitmap, TDrawMode.dmLinearBlend);
    CaptionBitmap.Free;
  end;
end;

constructor TBitmapButton.Create(const AOwner: TWinControl);
begin
  inherited Create(AOwner);
  Self.Parent := AOwner;
  FDefaultImage := nil;
  FActiveImage := nil;
  OnRenderControl := @DoRenderControl;
end;

destructor TBitmapButton.Destroy;
begin
  if Assigned(FDefaultImage) then
    FDefaultImage.Free;
  if Assigned(FActiveImage) then
    FActiveImage.Free;
  inherited Destroy;
end;

end.
