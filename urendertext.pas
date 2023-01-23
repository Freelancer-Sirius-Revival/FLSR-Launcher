unit URenderText;

{$mode ObjFPC}{$H+}

interface

uses
  BGRABitmap,
  BGRABitmapTypes,
  BGRATextFX,
  BGRAFreeType;

function RenderText(const Width: Int32; const Height: Int32; const Size: Int32; const Text: String): TBGRABitmap;

implementation

function CreateMainFontRenderer: TBGRAFreeTypeFontRenderer;
begin
  Result := TBGRAFreeTypeFontRenderer.Create;
  Result.ShadowOffset.SetLocation(1, 1);
  Result.ShadowRadius := 2;
  Result.ShadowColor := BGRABlack;
  Result.ShadowVisible := True;
end;

function RenderText(const Width: Int32; const Height: Int32; const Size: Int32; const Text: String): TBGRABitmap;
begin
  Result := TBGRABitmap.Create(Width, Height);
  Result.FontRenderer := CreateMainFontRenderer; // Font renderer is freed by bitmap.
  Result.FontName := 'Vibrocentric';
  Result.FontFullHeight := Size;
  Result.FontQuality := fqFineClearTypeRGB;
  Result.TextMultiline(0, 0, Width, Text, BGRAWhite);
  Result.FontRenderer := nil;
end;

end.
