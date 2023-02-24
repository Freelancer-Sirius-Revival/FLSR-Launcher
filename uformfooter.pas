unit UFormFooter;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  Controls,
  UBitmapButton;

type
  TFormFooter = record
    MainButton: TBitmapButton;
  end;

function CreateFormFooter(const Owner: TWinControl): TFormFooter;

implementation
       
function CreateMainButton(const Owner: TWinControl): TBitmapButton;
begin
  Result := TBitmapButton.Create(Owner);
  Result.Width := 384;
  Result.Height := 96;
  Result.Top := Owner.Height - 192;
  Result.Left := Owner.Width div 2 - 192;
  Result.DefaultImageResourceName := 'MAIN_BUTTON';
  Result.ActiveImageResourceName := 'MAIN_BUTTON_SELECTED';
  Result.ShowHint := True;
  Result.Hint := 'Install Freelancer: Sirius Revival';
  Result.Caption := 'Install ›Sirius Revival‹';
  Result.CaptionSize := 48;
end;

function CreateFormFooter(const Owner: TWinControl): TFormFooter;
begin
  Result.MainButton := CreateMainButton(Owner);
end;

end.

