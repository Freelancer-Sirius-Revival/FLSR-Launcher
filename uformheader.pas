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
  TopBorderDragHeight = 20;
  BorderPadding = 24;
  SystemIconButtonMargin = 12;
  SystemIconButtonSize = 16;
  CommunityIconButtonSize = 24;

type
  TBitmapButton = class(TBCXButton)
  private
    FDefaultImage: TBGRABitmap;
    FActiveImage: TBGRABitmap;
    procedure SetDefaultImage(const NewDefaultImageResourceName: String);
    procedure SetActiveImage(const NewActiveImageResourceName: String);
    procedure DoRenderControl(Sender: TObject; Bitmap: TBGRABitmap; State: TBCGraphicButtonState);
  public
    property DefaultImageResourceName: String write SetDefaultImage;
    property ActiveImageResourceName: String write SetActiveImage;
    constructor Create(const AOwner: TWinControl);
    destructor Destroy; override;
  end;

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
begin
  case State of
    gbsNormal:
    begin
      if Assigned(FDefaultImage) then
        Bitmap.StretchPutImage(TRect.Create(0, 0, ScaleDesignToForm(Width), ScaleDesignToForm(Height)), FDefaultImage, TDrawMode.dmDrawWithTransparency);
    end;
    gbsActive, gbsHover:
    begin
      if Assigned(FActiveImage) then
        Bitmap.StretchPutImage(TRect.Create(0, 0, ScaleDesignToForm(Width), ScaleDesignToForm(Height)), FActiveImage, TDrawMode.dmDrawWithTransparency)
      else if Assigned(FDefaultImage) then
        Bitmap.StretchPutImage(TRect.Create(0, 0, ScaleDesignToForm(Width), ScaleDesignToForm(Height)), FDefaultImage, TDrawMode.dmDrawWithTransparency);
    end;
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

function CreateCloseButton(const Owner: TWinControl): TBitmapButton;
begin
  Result := TBitmapButton.Create(Owner);
  Result.Width := SystemIconButtonSize;
  Result.Height := SystemIconButtonSize;
  Result.Top := BorderPadding;
  Result.Left := Owner.Width - Result.Width - BorderPadding;
  Result.Cursor := crHandPoint;
  Result.DefaultImageResourceName := 'CLOSE';        
  Result.ActiveImageResourceName := 'CLOSE_SELECTED';
  Result.ShowHint := True;
  Result.Hint := 'Close';
end;

function CreateMinimizeButton(const Owner: TWinControl; const LeftOffset: Int32): TBitmapButton;
begin
  Result := TBitmapButton.Create(Owner);
  Result.Width := SystemIconButtonSize;
  Result.Height := SystemIconButtonSize;
  Result.Top := BorderPadding;
  Result.Left := LeftOffset;
  Result.Cursor := crHandPoint;
  Result.DefaultImageResourceName := 'MINIMIZE';    
  Result.ActiveImageResourceName := 'MINIMIZE_SELECTED';
  Result.ShowHint := True;
  Result.Hint := 'Minimize';
end;

function CreateMoveButton(const Owner: TWinControl): TBitmapButton;
begin
  Result := TBitmapButton.Create(Owner);
  Result.Width := Owner.Width;
  Result.Height := TopBorderDragHeight;
  Result.Top := 0;
  Result.Left := 0;
  Result.Cursor := crSize;
  Result.ShowHint := True;
  Result.Hint := 'Move window';
end;

function CreateTitleButton(const Owner: TWinControl): TBitmapButton;
begin
  Result := TBitmapButton.Create(Owner);
  Result.Width := 920 div 4 * 3;
  Result.Height := 210 div 4 * 3;
  Result.Top := BorderPadding;
  Result.Left := 0;
  Result.Cursor := crHandPoint;
  Result.DefaultImageResourceName := 'TITLE';
  Result.ShowHint := True;
  Result.Hint := 'Open Freelancer: Sirius Revival website';
end;

function CreateFlsrDiscordButton(const Owner: TWinControl; const LeftOffset: Int32): TBitmapButton;
begin
  Result := TBitmapButton.Create(Owner);
  Result.Width := CommunityIconButtonSize;
  Result.Height := CommunityIconButtonSize;
  Result.Top := BorderPadding;
  Result.Left := LeftOffset;
  Result.Cursor := crHandPoint;
  Result.DefaultImageResourceName := 'ACCOUNTS';        
  Result.ActiveImageResourceName := 'ACCOUNTS_SELECTED';
  Result.ShowHint := True;
  Result.Hint := 'Join the Freelancer: Sirius Revival Discord';
end;

function CreateFlgcDiscordButton(const Owner: TWinControl; const LeftOffset: Int32): TBitmapButton;
begin
  Result := TBitmapButton.Create(Owner);
  Result.Width := CommunityIconButtonSize;
  Result.Height := CommunityIconButtonSize;
  Result.Top := BorderPadding;
  Result.Left := LeftOffset;
  Result.Cursor := crHandPoint;
  Result.DefaultImageResourceName := 'ACCOUNTS';    
  Result.ActiveImageResourceName := 'ACCOUNTS_SELECTED';
  Result.ShowHint := True;
  Result.Hint := 'Join the Freelancer Galactic Community Discord';
end;

function CreateTspWebsiteButton(const Owner: TWinControl; const LeftOffset: Int32): TBitmapButton;
begin
  Result := TBitmapButton.Create(Owner);
  Result.Width := CommunityIconButtonSize;
  Result.Height := CommunityIconButtonSize;
  Result.Top := BorderPadding;
  Result.Left := LeftOffset;
  Result.Cursor := crHandPoint;
  Result.DefaultImageResourceName := 'ACCOUNTS';       
  Result.ActiveImageResourceName := 'ACCOUNTS_SELECTED';
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
  Result.FontEx.Color := clWhite;
  Result.FontEx.Height := 20;
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
  Result.MinimizeButton := CreateMinimizeButton(Owner, Result.CloseButton.Left - SystemIconButtonMargin - SystemIconButtonSize);
  Result.MoveButton := CreateMoveButton(Owner);
  Result.TspWebsiteButton := CreateTspWebsiteButton(Owner, Result.MinimizeButton.Left - CommunityIconButtonSize - SystemIconButtonMargin);
  Result.FlgcDiscordButton := CreateFlgcDiscordButton(Owner, Result.TspWebsiteButton.Left - SystemIconButtonMargin - CommunityIconButtonSize);
  Result.FlsrDiscordButton := CreateFlsrDiscordButton(Owner, Result.FlgcDiscordButton.Left - SystemIconButtonMargin - CommunityIconButtonSize);
  Result.TitleButton := CreateTitleButton(Owner);
  Result.OnlinePlayersPanel := CreatePlayersOnlineLabel(Owner, Result.TitleButton.Top + Result.TitleButton.Height - 48, Result.TitleButton.Left + Result.TitleButton.Width);
end;

end.
