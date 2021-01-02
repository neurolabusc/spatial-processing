program smoothdemo;

uses
  Forms,
  smooth in 'smooth.pas' {Form1},
  otsu in 'otsu.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
