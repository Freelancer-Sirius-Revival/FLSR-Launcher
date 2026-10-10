program launcher;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  sysutils,
  Interfaces, // this includes the LCL widgetset
  Forms,
  UMainForm;

{$R *.res}

// Set the FL:SR specific vendor name for "GetAppConfigDir".
function GetVendorName: String;
begin
  Result := 'freelancer-sirius-revival'
end;

// Set the Laucher's specific application name for "GetAppConfigDir".
function GetApplicationName: String;
begin
  Result := 'launcher'
end;

begin  
  OnGetVendorName := @GetVendorName;
  OnGetApplicationName := @GetApplicationName;
  RequireDerivedFormResource := True;
  Application.Title := 'Freelancer: Sirius Revival – Launcher';
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

