program FileManagerTester;

uses
  Vcl.Forms,
  uMain in 'uMain.pas' {fMain},
  DirectoryManager in '..\DirectoryManager.pas',
  FileManager in '..\FileManager.pas',
  Heroes3LOD in '..\Heroes 3 Resources\Heroes3LOD.pas',
  StreamExtensions in '..\StreamExtensions.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfMain, fMain);
  Application.Run;
end.
