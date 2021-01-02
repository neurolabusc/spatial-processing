unit fsmooth;

{$mode objfpc}

interface

uses
  Classes, SysUtils,imgfunc;

procedure FastSmooth (var lImg: Bytep; lXi,lYi,lZi: integer);


implementation

procedure FastSmooth (var lImg: Bytep; lXi,lYi,lZi: integer);
const
  k0=240;//weight of center voxel
  k1=120;//weight of nearest neighbors
  k2=15;//weight of subsequent neighbors
  kTot=k0+k1+k1+k2+k2; //weight of center plus all neighbors within 2 voxels
  kWid = 2; //we will look +/- 2 voxels from center
var
  lyPos,lPos,lWSum,lX,lY,lZ,lXi2,lXY,lXY2: integer;
  lTemp: bytep;
begin
   if (lXi < 5) or (lYi < 5) then exit;
   lXY := lXi*lYi; //offset one slice
   lXY2 := lXY * 2; //offset two slices
   lXi2 := lXi*2;//offset to voxel two lines above or below
   getmem(lTemp,lXi*lYi*lZi*sizeof(byte));
   for lPos := 1 to (lXi*lYi*lZi) do
       lTemp^[lPos] := lImg^[lPos];
   //smooth horizontally
   for lZ := 1 to lZi do begin
     for lY := (1) to (lYi) do begin
       lyPos := ((lY-1)*lXi) + ((lZ-1)*lXY) ;
       for lX := (1+kWid) to (lXi-kWid) do begin
           lPos := lyPos + lX;
           lWSum := lImg^[lPos-2]*k2+lImg^[lPos-1]*k1
                 +lImg^[lPos]*k0
                 +lImg^[lPos+1]*k1+lImg^[lPos+2]*k2;
           lTemp^[lPos] := lWSum div kTot;
       end; {lX}
     end; {lY}
   end; //lZi
   //smooth vertically

   for lPos := 1 to (lXi*lYi*lZi) do
       lImg^[lPos] := lTemp^[lPos];//fill in sides
   for lZ := 1 to lZi do begin
     for lX := (1) to (lXi) do begin
       for lY := (1+kWid) to (lYi-kWid) do begin
           lPos := ((lY-1)*lXi) + lX + ((lZ-1)*lXY) ;
           lWSum := lTemp^[lPos-lXi2]*k2+lTemp^[lPos-lXi]*k1
                 +lTemp^[lPos]*k0
                 +lTemp^[lPos+lXi]*k1+lTemp^[lPos+lXi2]*k2;
           lImg^[lPos] := lWSum div kTot;
       end; {lX}
     end; //lY
   end; //lZ
   //if 3rd dimension....
   if lZi >= 5 then begin
     //smooth across slices
     for lPos := 1 to (lXi*lYi*lZi) do
         lTemp^[lPos] := lImg^[lPos]; //fill in sides
     for lZ := (1+kWid) to (lZi-kWid) do begin
       for lY := (1) to (lYi) do begin
         lyPos := ((lY-1)*lXi) + ((lZ-1)*lXY) ;
         for lX := (1) to (lXi) do begin
             lPos := lyPos + lX;
             lWSum := lImg^[lPos-lXY2]*k2+lImg^[lPos-lXY]*k1
                   +lImg^[lPos]*k0
                   +lImg^[lPos+lXY]*k1+lImg^[lPos+lXY2]*k2;
             lTemp^[lPos] := lWSum div kTot;
         end; {lX}
       end; {lY}
     end; //lZi
     for lPos := 1 to (lXi*lYi*lZi) do
         lImg^[lPos] := lTemp^[lPos];
   end; //at least 5 slices...
   //free memory
   freemem(lTemp);
end;

end.

