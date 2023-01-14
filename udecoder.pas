unit UDecoder;

{$mode ObjFPC}{$H+}

interface

uses
  Classes;

function Decode(const InputStream, OutputStream: TStream): Int64;

implementation

uses
  ULZMADecoder,
  ULZMACommon;

function Decode(const InputStream, OutputStream: TStream): Int64;
var
  Decoder: TLZMADecoder;
  Properties: array[0..4] of Byte; // From Encoder.WriteCoderProperties
  OutputSize: Int64;       
  i, v: Byte;
begin
  Result := -1;
  if InputStream.Read(Properties, SizeOf(Properties)) <> SizeOf(Properties) then
    Exit;
  Decoder := TLZMADecoder.Create;
  try
    if not Decoder.SetDecoderProperties(Properties) then
      Exit;
    OutputSize := 0;
    for i := 0 to 7 do
    begin
      v := ReadByte(InputStream);
      OutputSize := OutputSize or v shl (8 * i);
    end;
    if Decoder.Code(InputStream, OutputStream, OutputSize) then
      Result := OutputSize;
  finally
    Decoder.Free;
  end;
end;

end.
