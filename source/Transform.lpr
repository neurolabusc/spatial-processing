program Transform;

{$MODE Delphi}

uses
    Interfaces, // this includes the LCL widgetset
  Forms,MatrixForm;

{ $R LTransform.res}

begin
  Application.Title:='MatrixDemo';
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
