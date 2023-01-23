unit UFormHeader;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  Controls,
  BGRAText,
  BCImageButton,
  BCLabel;

type
  TFormHeader = record
    TitleButton: TBCXButton;
    CloseButton: TBCXButton;
    MinimizeButton: TBCXButton;
    MoveButton: TBCXButton;
    FlsrDiscordButton: TBCXButton;
    FlgcDiscordButton: TBCXButton;
    TspWebsiteButton: TBCXButton;
    OnlinePlayersPanel: TBCLabel;
  end;

function CreateFormHeader(const Owner: TWinControl): TFormHeader;

implementation

uses
  UResourceLoading,
  BGRABitmap,
  BGRABitmapTypes,
  BCTypes,
  Graphics;

const
  IconButtonMargin = 4;
  IconButtonSize = 24;

type
  TBitmapButton = class(TBCXButton)
  private
    FImage: TBGRABitmap;
    procedure SetImage(const NewImageResourceName: String);
    procedure DoRenderControl(Sender: TObject; Bitmap: TBGRABitmap; State: TBCGraphicButtonState);
  public
    property ImageResourceName: String write SetImage;
    constructor Create(const AOwner: TWinControl);
    destructor Destroy; override;
  end;

procedure TBitmapButton.SetImage(const NewImageResourceName: String);
var
  Stream: TStream;
begin
  if Assigned(FImage) then
    FImage.Free;
  Stream := LoadResource(NewImageResourceName);
  if Assigned(Stream) then
  begin
    FImage := TBGRABitmap.Create(Stream);
    Stream.Free;
  end;
end;

procedure TBitmapButton.DoRenderControl(Sender: TObject; Bitmap: TBGRABitmap; State: TBCGraphicButtonState);
begin
  if Assigned(FImage) then
    Bitmap.StretchPutImage(TRect.Create(0, 0, ScaleDesignToForm(Width), ScaleDesignToForm(Height)), FImage, TDrawMode.dmLinearBlend);
end;

constructor TBitmapButton.Create(const AOwner: TWinControl);
begin
  inherited Create(AOwner);
  Self.Parent := AOwner;
  FImage := nil;
  OnRenderControl := @DoRenderControl;
end;

destructor TBitmapButton.Destroy;
begin
  if Assigned(FImage) then
    FImage.Free;
  inherited Destroy;
end;

function CreateCloseButton(const Owner: TWinControl): TBitmapButton;
begin
  Result := TBitmapButton.Create(Owner);
  Result.Width := IconButtonSize;
  Result.Height := IconButtonSize;
  Result.Top := IconButtonMargin;
  Result.Left := Owner.Width - Result.Width - IconButtonMargin;
  Result.Cursor := crHandPoint;
  Result.ImageResourceName := 'CANCEL_04_32';
  Result.ShowHint := True;
  Result.Hint := 'Close';
end;

function CreateMinimizeButton(const Owner: TWinControl; const LeftOffset: Int32): TBitmapButton;
begin
  Result := TBitmapButton.Create(Owner);
  Result.Width := IconButtonSize;
  Result.Height := IconButtonSize;
  Result.Top := IconButtonMargin;
  Result.Left := LeftOffset;
  Result.Cursor := crHandPoint;
  Result.ImageResourceName := 'ARROW_35_32';
  Result.ShowHint := True;
  Result.Hint := 'Minimize';
end;

function CreateMoveButton(const Owner: TWinControl; const LeftOffset: Int32): TBitmapButton;
begin
  Result := TBitmapButton.Create(Owner);
  Result.Width := IconButtonSize;
  Result.Height := IconButtonSize;
  Result.Top := IconButtonMargin;
  Result.Left := LeftOffset;
  Result.Cursor := crSize;
  Result.ImageResourceName := 'ARROW_29_32';
  Result.ShowHint := True;
  Result.Hint := 'Move';
end;

function CreateTitleButton(const Owner: TWinControl): TBitmapButton;
begin
  Result := TBitmapButton.Create(Owner);
  Result.Width := 920 div 3 * 2;
  Result.Height := 210 div 3 * 2;
  Result.Top := IconButtonMargin;
  Result.Left := IconButtonMargin;
  Result.Cursor := crHandPoint;
  Result.ImageResourceName := 'TITLE';
  Result.ShowHint := True;
  Result.Hint := 'Open Freelancer: Sirius Revival website';
end;

function CreateFlsrDiscordButton(const Owner: TWinControl; const LeftOffset: Int32): TBitmapButton;
begin
  Result := TBitmapButton.Create(Owner);
  Result.Width := IconButtonSize;
  Result.Height := IconButtonSize;
  Result.Top := IconButtonMargin;
  Result.Left := LeftOffset;
  Result.Cursor := crHandPoint;
  Result.ImageResourceName := 'USER_01_32';
  Result.ShowHint := True;
  Result.Hint := 'Join the Freelancer: Sirius Revival Discord';
end;

function CreateFlgcDiscordButton(const Owner: TWinControl; const LeftOffset: Int32): TBitmapButton;
begin
  Result := TBitmapButton.Create(Owner);
  Result.Width := IconButtonSize;
  Result.Height := IconButtonSize;
  Result.Top := IconButtonMargin;
  Result.Left := LeftOffset;
  Result.Cursor := crHandPoint;
  Result.ImageResourceName := 'USER_01_32';
  Result.ShowHint := True;
  Result.Hint := 'Join the Freelancer Galactic Community Discord';
end;

function CreateTspWebsiteButton(const Owner: TWinControl; const LeftOffset: Int32): TBitmapButton;
begin
  Result := TBitmapButton.Create(Owner);
  Result.Width := IconButtonSize;
  Result.Height := IconButtonSize;
  Result.Top := IconButtonMargin;
  Result.Left := LeftOffset;
  Result.Cursor := crHandPoint;
  Result.ImageResourceName := 'USER_01_32';
  Result.ShowHint := True;
  Result.Hint := 'Open The Starport website';
end;

function CreatePlayersOnlineLabel(const Owner: TWinControl; const TopOffset: Int32; const LeftOffset: Int32): TBCLabel;
begin
  Result := TBCLabel.Create(Owner);
  Result.Parent := Owner;
  Result.Caption := '';
  Result.Top := TopOffset;
  Result.Left := LeftOffset;
  Result.FontEx.Name := 'Vibrocentric';
  Result.FontEx.Color := clWhite;
  Result.FontEx.Height := 24;
  Result.FontEx.Shadow := True;
  Result.FontEx.ShadowColor := clBlack;
  Result.FontEx.ShadowOffsetX := 2;
  Result.FontEx.ShadowOffsetY := 2;
  Result.FontEx.ShadowRadius := 4;
  Result.FontEx.SingleLine := True;
end;

function CreateFormHeader(const Owner: TWinControl): TFormHeader;
begin
  Result.CloseButton := CreateCloseButton(Owner);
  Result.MinimizeButton := CreateMinimizeButton(Owner, Result.CloseButton.Left - IconButtonMargin - IconButtonSize);
  Result.MoveButton := CreateMoveButton(Owner, Result.MinimizeButton.Left - IconButtonMargin - IconButtonSize);
  Result.TspWebsiteButton := CreateTspWebsiteButton(Owner, Result.MoveButton.Left - IconButtonMargin * 8 - IconButtonSize);
  Result.FlgcDiscordButton := CreateFlgcDiscordButton(Owner, Result.TspWebsiteButton.Left - IconButtonMargin - IconButtonSize);
  Result.FlsrDiscordButton := CreateFlsrDiscordButton(Owner, Result.FlgcDiscordButton.Left - IconButtonMargin - IconButtonSize);
  Result.TitleButton := CreateTitleButton(Owner);
  Result.OnlinePlayersPanel := CreatePlayersOnlineLabel(Owner, Result.TitleButton.Top + Result.TitleButton.Height - 48, Result.TitleButton.Left * 2 + Result.TitleButton.Width);
end;

end.
