unit ULog;

{$mode ObjFPC}{$H+}

interface

procedure AddToLog(const Message: String);

implementation

uses
  Classes, SysUtils;

var
  CriticalSection: TRTLCriticalSection;
  Log: TStringList;

procedure AddToLog(const Message: String);
begin
  EnterCriticalSection(CriticalSection);
  Log.Append(Message);
  LeaveCriticalSection(CriticalSection);
end;

initialization
  InitCriticalSection(CriticalSection);
  Log := TStringList.Create;

finalization
  DoneCriticalSection(CriticalSection);
  Log.Free;

end.
