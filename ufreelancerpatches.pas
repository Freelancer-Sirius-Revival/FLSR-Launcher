unit UFreelancerPatches;

{$mode ObjFPC}{$H+}

interface

procedure PatchFreelancerMoving3dTargetView(const Moving: Boolean);

implementation

uses
  Classes,
  SysUtils;

procedure PatchFreelancerMoving3dTargetView(const FreelancerExePath: string; const Moving: Boolean);
const
  TopDown3dView: array [0..2] of Byte = ($B1, $01, $90);
  Moving3dView: array [0..2] of Byte = ($0F, $95, $C1);
var
  FileStream: TFileStream;
begin
  if FileExists(FreelancerExePath) then
  begin
    FileStream := TFileStream.Create(FreelancerExePath, fmOpenWrite);
    FileStream.Position := $E3D09;
    if Moving then
      FileStream.WriteBuffer(Moving3dView[0], 3)
    else
      FileStream.WriteBuffer(TopDown3dView[0], 3);
    FileStream.Free;
  end;
end;

end.
