unit UProgress;

{$mode ObjFPC}{$H+}

interface

type
  TProcessProgress = class
  private
    FTotalBytes: Int64;
    FPercentage: Single;
    FDone: Int32;
    function GetDone: Boolean;
    procedure SetDone(const Done: Boolean);
    procedure SetPercentage(const NewPercentage: Single);
    procedure OnEncodingProgress(const BytesWritten: Int64);
  public
    constructor Create;
    property Percentage: Single read FPercentage write SetPercentage;
    property Done: Boolean read GetDone write SetDone;
  end;

implementation
                 
uses
  SysUtils;

function TProcessProgress.GetDone: Boolean;
begin
  Result := Boolean(FDone);
end;

procedure TProcessProgress.SetDone(const Done: Boolean);
begin
  InterlockedExchange(FDone, Int32(Done));
end;

procedure TProcessProgress.SetPercentage(const NewPercentage: Single);
begin
  InterlockedExchange(Int32(FPercentage), Int32(NewPercentage));
end;

procedure TProcessProgress.OnEncodingProgress(const BytesWritten: Int64);
begin
  SetPercentage(BytesWritten / FTotalBytes);
end;

constructor TProcessProgress.Create;
begin
  inherited Create;
  FTotalBytes := 0;
  FPercentage := 0;
  Done := False;
end;

end.
