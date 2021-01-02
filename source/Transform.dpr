program Transform;

uses
  Forms,
  MatrixForm in 'MatrixForm.pas' {Form1},
  matrices in 'matrices.pas';

//{ $R DTRANSFORM.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
