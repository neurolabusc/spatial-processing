unit matrices;
{$IFDEF FPC}{$MODE Delphi}{$ENDIF}


interface
uses Math;

const
 kMatSz = 3;
type

    TIndex      = 1..kMatSz;      // index of 'TMatrix' and 'TVector' TYPEs
    TMatrix2D     = ARRAY[TIndex,TIndex] OF single; //azx DOUBLE;

procedure EyeMat (var lMat: TMatrix2D);
procedure RotateMat (var lMat: TMatrix2D;Cx,Cy,Alpha: single);
procedure ScaleMat (var lMat: TMatrix2D;lX,lY: single);
function Mult(const M1, M2: TMatrix2D): TMatrix2D;
procedure TranslateMat (var lMat: TMatrix2D;lX,lY: single);
procedure SkewMat (var lMat: TMatrix2D;lX,lY: single);
procedure Invert(var M: TMatrix2D);
implementation



procedure TranslateMat (var lMat: TMatrix2D;lX,lY: single);
begin
          lMat[kMatSz,1]:=lMat[kMatSz,1]+lX;
          lMat[kMatSz,2]:=lMat[kMatSz,2]+lY;
end;

function Mult(const M1, M2: TMatrix2D): TMatrix2D;
var
  i, j: Integer;
begin
  for i := 1 to kMatSz do
    for j := 1 to kMatSz do
      Result[i, j] :=
        M1[1, j] * M2[i, 1] +
        M1[2, j] * M2[i, 2] +
        M1[3, j] * M2[i, 3];
end;

procedure ScaleMat (var lMat: TMatrix2D;lX,lY: single);
var
  M: TMatrix2D;
begin
  EyeMat(M);
  M[1,1] := lX;
  M[2,2] := lY;
  lMat := Mult(M, lMat);
end;

procedure RotateMat (var lMat: TMatrix2D;Cx,Cy,Alpha: single);
//X,Y are pivots -
var
  S, C: Single;
  M: TMatrix2D;
begin
  if (Cx <> 0) or (Cy <> 0) then TranslateMat(lMat,-Cx, -Cy);
  Alpha := DegToRad(Alpha);
  S := Sin(Alpha); C := Cos(Alpha);
  EyeMat(M);
  M[1,1] := C;   M[2,1] := S;
  M[1,2] := -S;  M[2,2] := C;
  lMat := Mult(M, lMat);
  if (Cx <> 0) or (Cy <> 0) then TranslateMat(lMat,Cx, Cy);
end;

procedure SkewMat (var lMat: TMatrix2D;lX,lY: single);
var
  M: TMatrix2D;
begin
  EyeMat(M);
  M[2,1] := lX;
  M[1,2] := lY;
  lMat := Mult(M, lMat);
end;

procedure EyeMat (var lMat: TMatrix2D);
var
   i,j: integer;
begin
    for i := 1 to kMatSz do
        for j := 1 to kMatSz do
            lMat[i,j] := 0;
    for i := 1 to kMatSz do
        lMat[i,i] := 1;
end;


function _DET(a1, a2, b1, b2: single): single; overload;
begin
  Result := a1 * b2 - a2 * b1;
end;

function _DET(a1, a2, a3, b1, b2, b3, c1, c2, c3: single): single; overload;
begin
  Result :=
    a1 * (b2 * c3 - b3 * c2) -
    b1 * (a2 * c3 - a3 * c2) +
    c1 * (a2 * b3 - a3 * b2);
end;

procedure Adjoint(var M: TMatrix2D);
var
  a1, a2, a3,
  b1, b2, b3,
  c1, c2, c3: single;
begin
  a1 := M[1,1]; a2:= M[1,2]; a3 := M[1,3];
  b1 := M[2,1]; b2:= M[2,2]; b3 := M[2,3];
  c1 := M[3,1]; c2:= M[3,2]; c3 := M[3,3];

  M[1,1]:= _DET(b2, b3, c2, c3);
  M[1,2]:=-_DET(a2, a3, c2, c3);
  M[1,3]:= _DET(a2, a3, b2, b3);

  M[2,1]:=-_DET(b1, b3, c1, c3);
  M[2,2]:= _DET(a1, a3, c1, c3);
  M[2,3]:=-_DET(a1, a3, b1, b3);

  M[3,1]:= _DET(b1, b2, c1, c2);
  M[3,2]:=-_DET(a1, a2, c1, c2);
  M[3,3]:= _DET(a1, a2, b1, b2);
end;

function Determinant(const M: TMatrix2D): single;
begin
  Result := _DET(M[1,1], M[2,1], M[3,1],
                 M[1,2], M[2,2], M[3,2],
                 M[1,3], M[2,3], M[3,3]);
end;

procedure Scale(var M: TMatrix2D; Factor: single);
var
  i, j: Integer;
begin
  for i := 1 to 3 do
    for j := 1 to 3 do
      M[i,j] := M[i,j] * Factor;
end;

procedure Invert(var M: TMatrix2D);
var
  Det: single;
begin
  Det := Determinant(M);
  if Abs(Det) < 1E-5 then EyeMat(M)
  else
  begin
    Adjoint(M);
    Scale(M, 1 / Det);
  end;
end;
end.
