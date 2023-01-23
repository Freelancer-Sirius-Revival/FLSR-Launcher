program Launcher;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  UMainForm;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Title := 'Freelancer: Sirius Revival – Launcher';
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

