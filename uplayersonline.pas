unit UPlayersOnline;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils;

type
  TPlayersFetchedProcedure = procedure(const PlayersOnline: Int32) of object;

procedure ListenForPlayersOnline(const OnPlayersFetched: TPlayersFetchedProcedure);
procedure StopListeningForPlayersOnline;

implementation

uses
  Classes,
  fphttpclient,
  opensslsockets,
  IniFiles,
  DateUtils;

type
  TPlayersOnlineThread = class(TThread)
  private
    FPlayersOnline: Int32;
    procedure CallCallback;
  protected
    procedure Execute; override;
  public
    PlayersOnlineCallback: TPlayersFetchedProcedure;
  end;

var
  Thread: TPlayersOnlineThread = nil;

procedure TPlayersOnlineThread.CallCallback;
begin
  if Assigned(PlayersOnlineCallback) then
    PlayersOnlineCallback(FPlayersOnline);
end;

procedure TPlayersOnlineThread.Execute;
var
  Response: TStream = nil;
  Ini: TIniFile = nil;
  Data: TStrings = nil;
  PlayerOnline: Int32;
  LastFetch: Double;
begin
  LastFetch := 0;
  FPlayersOnline := -1;
  while not Terminated do
  begin
    if MilliSecondsBetween(LastFetch, Now) < 30000 then
    begin
      Sleep(100); // Sleep must not be too big or closing the application may be blocked by it.
      Continue;
    end;

    LastFetch := Now;
    if Assigned(PlayersOnlineCallback) then
    begin
      try      
        try
          Response := TMemoryStream.Create;
          TFPHttpClient.SimpleGet('http://srv.fl-sr.eu:4040/Server.ini', Response); // Throws Errors with the HTTP codes
          Response.Position := 0;
          Ini := TMemIniFile.Create(Response);
          FreeAndNil(Response);
          Data := TStringList.Create;
          Ini.ReadSectionValues('data', Data);
          if TryStrToInt(Data.Values['playeronline'], PlayerOnline) then
          begin
            FPlayersOnline := PlayerOnline;
            Queue(@CallCallback);
          end;
        except
          FPlayersOnline := -1;
          Queue(@CallCallback);
        end;
      finally
        if Assigned(Response) then
          Response.Free;
        if Assigned(Ini) then
          Ini.Free;
        if Assigned(Data) then
          Data.Free;
      end;
    end;
  end;
end;

procedure ListenForPlayersOnline(const OnPlayersFetched: TPlayersFetchedProcedure);
begin
  Thread := TPlayersOnlineThread.Create(False);
  Thread.PlayersOnlineCallback := OnPlayersFetched;
end;

procedure StopListeningForPlayersOnline;
begin
  if Assigned(Thread) then
  begin
    Thread.Terminate;
    Thread.WaitFor;
    Thread.Free;
  end;
end;

end.
