unit URenderText;

{$mode ObjFPC}{$H+}

interface

uses
  BGRABitmap,
  BGRABitmapTypes,
  BGRAGraphics;

function RenderText(const Width: Int32; const Height: Int32; const Size: Int32; const FontName: String; const FontStyle: TFontStyles; Color: TBGRAPixel; const Text: String): TBGRABitmap;

implementation

uses
  BGRATextFX,
  BGRAFreeType;

function CreateMainFontRenderer: TBGRAFreeTypeFontRenderer;
begin
  Result := TBGRAFreeTypeFontRenderer.Create;
  Result.ShadowOffset.SetLocation(1, 1);
  Result.ShadowRadius := 2;
  Result.ShadowColor := BGRABlack;
  Result.ShadowVisible := True;
end;

function RenderText(const Width: Int32; const Height: Int32; const Size: Int32; const FontName: String; const FontStyle: TFontStyles; Color: TBGRAPixel; const Text: String): TBGRABitmap;
begin
  Result := TBGRABitmap.Create(Width, Height);
  Result.FontRenderer := CreateMainFontRenderer; // Font renderer is freed by bitmap.
  Result.FontName := FontName;
  Result.FontStyle := FontStyle;
  Result.FontFullHeight := Size;
  Result.FontQuality := fqFineClearTypeRGB;
  Result.TextMultiline(0, Height div 2, Width, Text, Color, TBidiTextAlignment.btaCenter, TTextLayout.tlCenter);
  Result.FontRenderer := nil;
end;

end.
