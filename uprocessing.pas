unit UProcessing;

{$mode ObjFPC}{$H+}

interface

procedure ProcessBundling(const InputPath: String; const OutputPath: String);

implementation

uses
  Classes,
  SysUtils,
  UDecoding,
  UBundle,
  FileUtil,
  md5;

procedure Process(InputPath: String; OutputPath: String);
var
  CompleteBundlePath: String;
  Stream: TStream;
  BundleMetaData: TBundle;
begin
  InputPath := InputPath.Replace('\', '/');
  if not InputPath.EndsWith('/') then
    InputPath := InputPath + '/';

  OutputPath := OutputPath.Replace('\', '/');
  if OutputPath.EndsWith('/') then
    OutputPath := OutputPath.Substring(0, Length(OutputPath) - 1);

  CompleteBundlePath := InputPath + FullBundleFileName + BundleFileExtension;

  if FileExists(CompleteBundlePath) then
  begin
    try
      Stream := TFileStream.Create(CompleteBundlePath, fmOpenRead);
      BundleMetaData := ReadBundleMetaData(Stream);
      if BundleMetaData.BundleType <> TUnknownBundle then
        DecodeFilesChunks(BundleMetaData.FilesChunks, Stream, OutputPath);
    finally
      Stream.Free;
    end;



    //PreviousFullBundleMetaData := GetBundleMetaData(CompleteBundlePath);
    //if PreviousFullBundleMetaData.BundleType = TFullBundle then
    //begin
    //  UpdateFileList := RemoveEqualFileEntriesFromFilesChunks(CompleteFileList, PreviousFullBundleMetaData.FileEntries, InputPath);
    //  if UpdateFileList.Count <> 0 then
    //  begin
    //    NextContentVersion := PreviousFullBundleMetaData.ContentVersion + 1;
    //    UpdateFilesChunks := ComputeChunkedFiles(UpdateFileList, ['.ini']);
    //    UpdateFileName := UpdateBundleFileName + '.' + IntToStr(NextContentVersion) + BundleFileExtension;
    //    CreateBundle(NextContentVersion, TUpdateBundle, UpdateFilesChunks, InputPath, OutputPath + UpdateFileName);
    //    CreateChecksumFileForFile(UpdateFileName);
    //  end;
    //end;
  end;
end;

type
  TProcessThread = class(TThread)
  private
    FInputPath: String;
    FOutputPath: String;
  protected
    procedure Execute; override;
  public
    constructor Create(const InputPath: String; const OutputPath: String);
  end;

constructor TProcessThread.Create(const InputPath: String; const OutputPath: String);
begin
  inherited Create(False);
  FInputPath := InputPath;
  FOutputPath := OutputPath;
end;

procedure TProcessThread.Execute;
begin
  Process(FInputPath, FOutputPath);
end;

procedure ProcessBundling(const InputPath: String; const OutputPath: String);
var
  ProcessThread: TProcessThread;
begin
  ProcessThread := TProcessThread.Create(InputPath, OutputPath);
  ProcessThread.FreeOnTerminate := True;
end;

end.
