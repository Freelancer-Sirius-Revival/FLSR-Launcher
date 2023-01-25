unit UResourceLoading;

{$mode ObjFPC}{$H+}

interface

uses
  Classes;

function LoadResource(const Name: String): TResourceStream;

implementation

uses
  {$IFDEF WINDOWS}
  windows,
  {$ENDIF}
  LCLIntf;

function LoadResource(const Name: String): TResourceStream;
begin
  try
    Result := TResourceStream.Create(HInstance, Name, RT_RCDATA);
  except
    Result := nil;
  end;
end;

end.
