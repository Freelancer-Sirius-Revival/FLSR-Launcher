unit UFormHeader;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  Controls,
  BCLabel,
  UBitmapButton;

type
  TFormHeader = record
    TitleButton: TBitmapButton;
    CloseButton: TBitmapButton;
    MinimizeButton: TBitmapButton;
    MoveButton: TBitmapButton;
    FlsrDiscordButton: TBitmapButton;
    FlgcDiscordButton: TBitmapButton;
    TspWebsiteButton: TBitmapButton;
    OnlinePlayersPanel: TBCLabel;
  end;

function CreateFormHeader(const Owner: TWinControl): TFormHeader;

implementation

uses
  Math,
  Graphics;

const
  TopBorderDragHeight = 24;
  BorderPadding = 48;
  SystemIconButtonBorderPadding = 7;
  SystemIconButtonMargin = 12;
  SystemIconButtonSize = 12;
  CommunityIconButtonSize = 72;       
  CommunityIconButtonMargin = 12;

function CreateCloseButton(const Owner: TWinControl): TBitmapButton;
begin
  Result := TBitmapButton.Create(Owner);
  Result.Width := SystemIconButtonSize;
  Result.Height := SystemIconButtonSize;
  Result.Top := SystemIconButtonBorderPadding;
  Result.Left := Owner.Width - Result.Width - SystemIconButtonBorderPadding;
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
  Result.Top := SystemIconButtonBorderPadding;
  Result.Left := LeftOffset;
  Result.Cursor := crHandPoint;
  Result.DefaultImageResourceName := 'MINIMIZE';
  Result.ActiveImageResourceName := 'MINIMIZE_SELECTED';
  Result.ShowHint := True;
  Result.Hint := 'Minimize';
end;

function CreateMoveButton(const Owner: TWinControl; const Right: Int32): TBitmapButton;
begin
  Result := TBitmapButton.Create(Owner);
  Result.Width := Right;
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
  Result.Width := 815 div 4 * 3;
  Result.Height := 200 div 4 * 3;
  Result.Top := BorderPadding;
  Result.Left := BorderPadding;
  Result.Cursor := crHandPoint;
  Result.DefaultImageResourceName := 'TITLE3';
  Result.ShowHint := True;
  Result.Hint := 'Open Freelancer: Sirius Revival website';
end;

function CreateFlsrDiscordButton(const Owner: TWinControl; const LeftOffset: Int32): TBitmapButton;
begin
  Result := TBitmapButton.Create(Owner);
  Result.Width := CommunityIconButtonSize;
  Result.Height := Math.Floor(CommunityIconButtonSize * 0.875);
  Result.Top := BorderPadding;
  Result.Left := LeftOffset;
  Result.Cursor := crHandPoint;
  Result.DefaultImageResourceName := 'FLSR';
  Result.ActiveImageResourceName := 'ACCOUNTS_SELECTED';
  Result.ShowHint := True;
  Result.Hint := 'Join the Freelancer: Sirius Revival Discord';
end;

function CreateFlgcDiscordButton(const Owner: TWinControl; const LeftOffset: Int32): TBitmapButton;
begin
  Result := TBitmapButton.Create(Owner);
  Result.Width := CommunityIconButtonSize;
  Result.Height := Math.Floor(CommunityIconButtonSize * 0.875);
  Result.Top := BorderPadding;
  Result.Left := LeftOffset;
  Result.Cursor := crHandPoint;
  Result.DefaultImageResourceName := 'FGC';
  Result.ActiveImageResourceName := 'ACCOUNTS_SELECTED';
  Result.ShowHint := True;
  Result.Hint := 'Join the Freelancer Galactic Community Discord';
end;

function CreateTspWebsiteButton(const Owner: TWinControl; const LeftOffset: Int32): TBitmapButton;
begin
  Result := TBitmapButton.Create(Owner);
  Result.Width := CommunityIconButtonSize;
  Result.Height := Math.Floor(CommunityIconButtonSize * 0.875);
  Result.Top := BorderPadding;
  Result.Left := LeftOffset;
  Result.Cursor := crHandPoint;
  Result.DefaultImageResourceName := 'TSP';
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
  Result.MoveButton := CreateMoveButton(Owner, Result.MinimizeButton.Left);
  Result.TspWebsiteButton := CreateTspWebsiteButton(Owner, Owner.Width - BorderPadding - CommunityIconButtonSize);
  Result.FlgcDiscordButton := CreateFlgcDiscordButton(Owner, Result.TspWebsiteButton.Left - CommunityIconButtonMargin - CommunityIconButtonSize);
  Result.FlsrDiscordButton := CreateFlsrDiscordButton(Owner, Result.FlgcDiscordButton.Left - CommunityIconButtonMargin - CommunityIconButtonSize);
  Result.TitleButton := CreateTitleButton(Owner);
  Result.OnlinePlayersPanel := CreatePlayersOnlineLabel(Owner, Result.FlsrDiscordButton.Top + Result.FlsrDiscordButton.Height + CommunityIconButtonMargin, Result.FlsrDiscordButton.Left);
end;

end.
