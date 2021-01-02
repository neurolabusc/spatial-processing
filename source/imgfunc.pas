unit imgfunc;
{$ifdef fpc}{$mode objfpc}{$endif}
interface
uses
//  {$IFDEF FPC} GraphType,// {888 for gettickcount->}LCLIntf,

  {$ifndef UNIX}Windows,{$ELSE}  LCLIntf,LResources,{$ENDIF}
  Classes, SysUtils, dialogs, graphics;
const
 kNearestFilter=0;
 kLinearFilter=1;
 kLanczosFilter= 2;
 kCatRomFilter=3;
 kMitchellFilter = 4;
 kCubicFilter = 5; //approximates a Gaussian with 1.44 pixel fwhm
 kBackground8 = 0;
 {$IFDEF Darwin}
 kAlphaOpaque = 255;
 {$ELSE}
 kAlphaOpaque = 128;
 {$ENDIF}
 type
 {$IFDEF UNIX}
   TRGBquad = PACKED RECORD
      {$IFDEF ENDIAN_BIG} //OSX PPC
           rgbreserved,rgbRed,rgbGreen,rgbBlue: byte;
      {$ELSE}
           {$IFDEF UNIX}
              {$IFDEF DARWIN}
             rgbreserved,rgbRed,rgbGreen,rgbBlue: byte;
             //rgbBlue,rgbGreen,rgbRed,rgbreserved: byte;
              {$ELSE}
                     rgbRed,rgbGreen,rgbBlue,rgbreserved: byte;
              {$ENDIF}
           {$ELSE} //not unix - windows
           rgbBlue,rgbGreen,rgbRed,rgbreserved: byte;
           {$ENDIF}
      {$ENDIF}
      end;
  {$ENDIF}
      RGBQuadRA = array [1..1] of TRGBQuad;
      RGBQuadp = ^RGBQuadRA;
    ByteRA = array [1..1] of byte;
    Bytep = ^ByteRA;
    TImg = record
          PixelFormat32,Ht,Wid: integer;
          Img: ByteP;
          Img32: RGBQuadp;
      end;
    LongIntRA = array [1..1] of longint;
    LongIntp = ^LongIntRA;
    SingleRA = array [1..1] of single;
    Singlep = ^SingleRA;
    SingleRA0 = array [0..0] of single;
    Singlep0 = ^SingleRA0;
 var
   kBackGround32 : TRGBQuad;
procedure ForceGrayScale(var lImg: TImg);
procedure ForceRGB(var lImg: TImg);
procedure CreateImg(var lBuff: TImg);
procedure FreeImg(var lBuff: TImg);
function FilterHt (X: single; lFilt: integer): single;
procedure ResizeImg( lHt,lWid: integer; var lBuff: TImg; l32bit: boolean);
procedure GenerateTestImg( lHt,lWid: integer; var lBuff: TImg);
procedure CopyImg( lSrc: TImg; var lDest: TImg);
procedure CopyImg8(  lSrc: TImg; var lDest: TImg);
function Intensity (lX,lY: single; lSrc: TImg): byte; overload; //bilinear interpolation
function Intensity (lX,lY: single; lSrc: TImg; lBilinear: boolean): byte; overload;
function Intensity (lX,lY: single; lSrc: TImg; lInterp: integer): byte; overload;
function Intensity32 (lX,lY: single; lSrc: TImg): TRGBQuad; overload; //bilinear interpolation
function Intensity32 (lX,lY: single; lSrc: TImg; lBilinear: boolean): TRGBQuad; overload;
function Intensity32 (lX,lY: single; lSrc: TImg; lInterp: integer): TRGBQuad; overload;
 FUNCTION  ColorToRGB32(CONST Color:  TColor):  TRGBquad;
 function TRGBQuad2Gray (lRGB: TRGBQuad):byte;
implementation


 function TRGBQuad2Gray (lRGB: TRGBQuad):byte;
begin
     result := round( 0.3*lRGB.rgbRed + 0.59*lRGB.rgbGreen + 0.11*lRGB.rgbBlue);
end;

FUNCTION  ColorToRGB32(CONST Color:  TColor):  TRGBquad;
  BEGIN
    WITH RESULT DO BEGIN
      rgbRed   := GetRValue(Color);
      rgbGreen := GetGValue(Color);
      rgbBlue  := GetBValue(Color);
      rgbReserved := kAlphaOpaque;
    END
  END ;

function Intensity (lX,lY: single; lSrc: TImg): byte;   overload;
begin
     result := Intensity (lX,lY, lSrc, kLinearFilter)
end;

function Intensity (lX,lY: single; lSrc: TImg; lBilinear: boolean): byte; overload;
begin
     if lBilinear then
        result := Intensity (lX,lY, lSrc, kLinearFilter)
     else
         result := Intensity (lX,lY, lSrc, kNearestFilter)//nearest
end;

function NearestHt(Value: single): single;
begin
  if (Value >= -0.5) and (Value <= 0.5) then Result := 1.0
  else Result := 0;
end;

function LinearHt(X: single): single;
begin
     x := abs(x);
     if X < 1 then
        result := 1-X
     else
         result := 0;
end;

function KeysHt(x: single; B,C: single): single;
//This function creates Keys Filters.
// You can make the code a tiny bit faster for cubic B-Spline and Catmull-Rom by removing all the factors multiplied by C or B respectively
//  However, the speed up is small and this provides a universal source
// 'B' (as used for 'B-spline' curves) and 'C' ('Cardinal' curves, referred to as 'A' in descriptions of 'Keys' filter a value)
//http://http.developer.nvidia.com/GPUGems/gpugems_ch24.html
   // B = 1,   C = 0   - cubic B-spline
   // B = 1/3, C = 1/3 - recommended  Mitchell Netravali  Filter
   // B = 0,   C = 1/2 - Catmull-Rom spline
//http://www.imagemagick.org/Usage/img_diagrams/cubic_survey.gif
//http://www.imagemagick.org/Usage/resize/#filter_cubics
//http://www.imagemagick.org/Usage/resize/#mitchell
//http://www.imagemagick.org/Usage/resize/#catrom
var
   ax: single;
begin
  ax := abs(x);
  if (ax < 1) then
     result := ((12 - 9 * B - 6 * C) * ax * ax * ax +
            (-18 + 12 * B + 6 * C) * ax * ax + (6 - 2 * B)) / 6
  else if  (ax < 2) then
      result :=  ((-B - 6 * C) * ax * ax * ax +
              (6 * B + 30 * C) * ax * ax + (-12 * B - 48 * C) *
              ax + (8 * B + 24 * C)) / 6
  else
      result := 0;
end;

function CatRomHt(x: single): single;
begin
     result := KeysHt(x,0,1/2);
end;

function MitchellHt(x: single): single;
begin
     result := KeysHt(x,1/3,1/3);
end;

function CubicHt(x: single): single;
begin
     result := KeysHt(x,1,0);
end;

//Lanczos filter - approximation of the Sinc filter. Excellent for high frequencies with kLanczosA = 3
//  kLanczosA = 2 is a bit blurrier (and has less ringing artifacts).
// However, kLanczosA = 2 is very similar to the faster faster Mitchell filters, as noted by Nicolas Robidoux
//http://www.imagemagick.org/Usage/resize/#robidoux
const
 kLanczosA = 3;
 kLanczosANeg = -kLanczosA+1;
 pisqr = pi * pi;
function LanczosHt(x: Single): Single;
// http://en.wikipedia.org/wiki/Lanczos_resampling
begin
     x := abs(x);
     if x = 0 then
       result := 1
     else if x >= kLanczosA then
          result := 0
     else
         result :=  (kLanczosA*sin(pi*x)*sin(pi*x/kLanczosA)) /  (pisqr*sqr(x));
end;

//WARNING: These computations are exceptionally inefficient.
//For actual interpolation you would compute 2D plane and use lookup tables rather than getting one voxel at a time...
function Intensity (lX,lY: single; lSrc: TImg; lInterp: integer): byte; overload;
function SrcVal (lX,lY: integer): byte; //nearest neighbor
var
   ix,iy: integer;
begin
     ix:= lX;
     iy:= lY;
     if (ix > lSrc.Wid) then
        ix := lSrc.Wid;
     if ix < 1 then
        ix := 1;
     if (iy > lSrc.Ht) then
        iy := lSrc.Ht;
     if iy < 1 then
        iy := 1;
     result := lSrc.Img^[(iy-1)*lSrc.Wid+ ix];
end;
function LinearVal (lX,lY: single): byte; //bilinear interpolation - weights four nearest neighbors
var
   ix,iy: integer;
begin
     ix := trunc(lX);
     iy := trunc(lY);
     result :=round (
            SrcVal(ix,iy)*( (1-frac(lX)) * (1-frac(lY)))+
            SrcVal(ix+1,iy)*( (frac(lX)) * (1-frac(lY)))+
            SrcVal(ix,iy+1)*( (1-frac(lX)) * (frac(lY)))+
            SrcVal(ix+1,iy+1)*( (frac(lX)) * (frac(lY))));
end;
function LanczosVal (lX,lY: single): byte;
var
   ix,iy,dx,dy: integer;
   wt,f,fy: single;
   fx: array [kLanczosANeg..kLanczosA] of single;
begin
     ix := trunc(lX);
     iy := trunc(lY);
     f := 0;
     for dx := kLanczosANeg to kLanczosA do
         fx[dx] := LanczosHt(lX- dx-ix);
     wt := 0;
     for dy := kLanczosANeg to kLanczosA do begin
          fy := LanczosHt(lY-dy-iy);
          for dx := kLanczosANeg to kLanczosA do begin
              wt := wt + fx[dx]*fy;
              f := f + (SrcVal (dx+ix,dy+iy)*fx[dx]*fy);
          end;
     end;
     if wt <> 0 then
        f := f/wt;
     if f < 0 then
        f := 0;
     if f > 255 then
        f := 255;
     result := round(f);
end;
function KeysVal (lX,lY,B,C: single): byte;
var
   ix,iy,dx,dy: integer;
   f,fy: single;
   fx: array [-1..2] of single;
begin
     ix := trunc(lX);
     iy := trunc(lY);
     f := 0;
     for dx := -1 to +2 do
         fx[dx] := MitchellHt(lX- dx-ix);
     for dy := (iy-1) to (iy+2) do begin
          fy := KeysHt(lY-dy,B,C);
          for dx := -1 to +2 do begin
              f := f + (SrcVal (dx+ix,dy)*fx[dx]*fy);
          end;
     end;
     if f < 0 then
        f := 0;
     if f > 255 then
        f := 255;
     result := round(f);
end;

begin
     result := 0;
     if (lSrc.Ht < 1) or (lSrc.Wid< 1) then
        exit;
     if (lX < 0) or (lY < 0) or (lX > lSrc.Wid) or (lY > lSrc.Ht) then
        exit;
     if lInterp = kCubicFilter then
        result := KeysVal(lX,lY,1,0)
     else if lInterp = kMitchellFilter then
        result := KeysVal(lX,lY,1/3,1/3)
     else if lInterp = kCatRomFilter then
        result := KeysVal(lX,lY,0,1/2)
     else if lInterp = kLanczosFilter then
        result := LanczosVal(lX,lY)
     else if lInterp = kNearestFilter then
        result := SrcVal(round(lX),round(lY))
     else  //default = 1 = linear
        result :=  LinearVal(lX,lY);
end; //funct Intensity

function FilterHt (X: single; lFilt: integer): single;
begin
     case lFilt of
          kNearestFilter: result := NearestHt(X);
          //1: result := Linear(X);
          kLanczosFilter: result := LanczosHt(X);
          kCatRomFilter: result := CatRomHt(X);
          kMitchellFilter: result := MitchellHt(X);
          kCubicFilter: result := CubicHt(X);
          else result := LinearHt(X);
     end;
end;
procedure Bar (lL,lT,lR,lB,lInten: integer; var lBuff: TImg);
var
   lLineOffset,lY,lX: integer;
begin
     for lY := lT to lB do begin
         lLineOffset := ((lY-1)*lBuff.Wid);
         for lX := lL to lR do
             lBuff.Img^[lX+lLineOffset] := lInten;
     end;
end;

procedure GradientBar (lL,lT,lR,lB: integer; var lBuff: TImg);
var
   lLineOffset,lY,lX: integer;
begin
     for lY := lT to lB do begin
         lLineOffset := ((lY-1)*lBuff.Wid);
         for lX := lL to lR do
             lBuff.Img^[lX+lLineOffset] := lY-lB mod 256;
     end;
end;

procedure CreateImg(var lBuff: TImg);
begin
     lBuff.Ht := 0;
     lBuff.Wid := 0;
     lBuff.Img := nil;
     lBuff.Img32 := nil;
     lBuff.PixelFormat32 := 8;
end;

procedure FreeImg(var lBuff: TImg);
begin
     if lBuff.Img <> nil then
        freemem(lBuff.Img);
     if lBuff.Img32 <> nil then
        freemem(lBuff.Img32);
     CreateImg( lBuff); //set to nil, 0 ht, 0 wid
end;

procedure ResizeImg( lHt,lWid: integer; var lBuff: TImg; l32bit: boolean);
var
   lPos: integer;
   BG: TRGBQuad;
begin
     FreeImg(lBuff);
     lBuff.Ht := lHt;
     lBuff.Wid := lWid;
     lBuff.PixelFormat32 := 8;
     getmem(lBuff.Img, lHt*lWid);
     for lPos := 1 to (lHt*lWid) do
             lBuff.Img^[lPos] := 0;
     if not l32bit then
        exit;
     lBuff.PixelFormat32 := 32;
     getmem(lBuff.Img32, lHt*lWid*sizeof(TRGBQuad));
     BG :=  ColorToRGB32(clGray);
     for lPos := 1 to (lHt*lWid) do
             lBuff.Img32^[lPos] := BG;
end;

procedure GenerateTestImg( lHt,lWid: integer; var lBuff: TImg);
var
   lBorder,lThick: integer;
begin
     ResizeImg(lHt,lWid,lBuff,false);
     lBorder := 40;
     lThick := 10;

     GradientBar ((lWid div 2),1,(lWid div 2)+lThick,lHt, lBuff); //left

     Bar (2,2,lWid-2,4,255, lBuff); //top horiz
     Bar (2,lHt-4,lWid-2,lHt-2,255, lBuff); //bottom horiz

     Bar (lBorder,lBorder,lWid-lBorder,lBorder+lThick,255, lBuff); //bottom horiz
     Bar (lBorder,lHt-lBorder-lThick,lWid-lBorder,lHt-lBorder,255, lBuff); //top horiz
     Bar (lWid-lBorder-lThick,lBorder,lWid-lBorder,lHt-lBorder,255, lBuff); //right


     lThick := 2* lThick;
     Bar (lBorder,lBorder,lBorder+lThick,lHt-lBorder,150, lBuff); //left
end;

procedure ForceRGB(var lImg: TImg);
const
     kFlip = true;
var
   lPix,x,y: integer;
   lI: byte;
   lTemp: TImg;
begin
     if (lImg.PixelFormat32 = 32) then
        exit;
     lPix := lImg.Wid * lImg.Ht;
     if (lPix < 1) then
        exit;
     CreateImg(lTemp);
     CopyImg(lImg,lTemp);
     FreeImg(lImg);
     getmem(lImg.Img, lPix);
     getmem(lImg.Img32, lPix*sizeof(TRGBQuad));

     lImg.Ht := lTemp.Ht;
     lImg.Wid := lTemp.Wid;
     lImg.PixelFormat32 := 32;
     lPix := 0;
     for y :=  (lImg.Ht-1) downto 0  do
     //for y :=  0 to (lImg.Ht-1)  do
         for x := 1 to lImg.Wid do begin

             inc(lPix);
             lImg.Img^[lPix] := lTemp.Img^[lPix];
             lI := lTemp.Img^[(y*lImg.Wid)+x];
             lImg.Img32^[lPix].rgbRed := lI;
             lImg.Img32^[lPix].rgbGreen := lI;
             lImg.Img32^[lPix].rgbBlue := lI;


         end; //for each pixel

end;

procedure ForceGrayScale(var lImg: TImg);
const
     kFlip = true;
var
   lPix,x,y: integer;
   lTemp: TImg;
begin
     if (lImg.PixelFormat32 <> 32) then begin

        exit;
     end;

     lPix := lImg.Wid * lImg.Ht;
     if (lPix < 1) then
        exit;
     CreateImg(lTemp);
     CopyImg(lImg,lTemp);
     FreeImg(lImg);
     getmem(lImg.Img, lPix);
     lImg.Ht := lTemp.Ht;
     lImg.Wid := lTemp.Wid;
     lImg.PixelFormat32 := 8;
     lPix := 0;
     for y := (lImg.Ht-1) downto 0 do
         for x := 1 to lImg.Wid do begin
             inc(lPix);
             lImg.Img^[lPix] := lTemp.Img^[(y*lTemp.Wid)+x];
         end;
     FreeImg(lTemp);
end;

(*procedure CopyImg(  lSrc: TImg; var lDest: TImg; lForce8Bit: boolean); overload;
var
   i,lPix: integer;
begin
     lPix := lSrc.Wid * lSrc.Ht;
     if (lPix < 1) then
        exit;
     FreeImg(lDest);
     getmem(lDest.Img, lPix);
     lDest.Ht := lSrc.Ht;
     lDest.Wid := lSrc.Wid;
     for i := 1 to lPix do
         lDest.Img^[i] := lSrc.Img^[i];
     lDest.PixelFormat32 := lSrc.PixelFormat32;
     if lSrc.PixelFormat32 <> 32 then
        exit;
     getmem(lDest.Img32, lPix*sizeof(TRGBQuad));
     for i := 1 to lPix do
         lDest.Img32^[i] := lSrc.Img32^[i];
end;

procedure CopyImg(  lSrc: TImg; var lDest: TImg); overload;
begin
  CopyImg(lSrc, lDest, False);
end;  *)

procedure CopyImg( lSrc: TImg; var lDest: TImg);
//procedure CopyImg(  lSrc: TImg; var lDest: TImg); overload;
var
   i,lPix: integer;
begin
     lPix := lSrc.Wid * lSrc.Ht;
     if (lPix < 1) then
        exit;
     FreeImg(lDest);
     getmem(lDest.Img, lPix);
     lDest.Ht := lSrc.Ht;
     lDest.Wid := lSrc.Wid;
     for i := 1 to lPix do
         lDest.Img^[i] := lSrc.Img^[i];
     lDest.PixelFormat32 := lSrc.PixelFormat32;
     if lSrc.PixelFormat32 <> 32 then
        exit;
     getmem(lDest.Img32, lPix*sizeof(TRGBQuad));
     for i := 1 to lPix do
         lDest.Img32^[i] := lSrc.Img32^[i];
end;

{$IFDEF FPC}
procedure CopyImg8(  lSrc: TImg; var lDest: TImg);
const
     kFlip = true;
var
   lPix,x,y: integer;
begin
     if (lSrc.PixelFormat32 <> 32) or (not kFlip) then begin
        CopyImg(lSrc,lDest);
        exit;
     end;
     lPix := lSrc.Wid * lSrc.Ht;
     if (lPix < 1) then
        exit;
     FreeImg(lDest);
     getmem(lDest.Img, lPix);
     lDest.Ht := lSrc.Ht;
     lDest.Wid := lSrc.Wid;
     lDest.PixelFormat32 := 8;
     lPix := 0;
     for y := (lDest.Ht-1) downto 0 do
         for x := 1 to lDest.Wid do begin
             inc(lPix);
             lDest.Img^[lPix] := lSrc.Img^[(y*lDest.Wid)+x];
         end;
end;
{$ELSE}
procedure CopyImg8(  lSrc: TImg; var lDest: TImg);
begin
     CopyImg(lSrc,lDest);
end;
{$ENDIF}

//WARNING: These computations are exceptionally inefficient.
function Intensity32 (lX,lY: single; lSrc: TImg; lInterp: integer): TRGBQuad; overload;
function SrcVal32 (lX,lY: integer): TRGBQuad; //nearest neighbor
var
   ix,iy: integer;
begin
     ix:= lX;
     iy:= lY;
     if (ix > lSrc.Wid) then
        ix := lSrc.Wid;
     if ix < 1 then
        ix := 1;
     if (iy > lSrc.Ht) then
        iy := lSrc.Ht;
     if iy < 1 then
        iy := 1;
     result := lSrc.Img32^[(iy-1)*lSrc.Wid+ ix];
end;
function LinearVal32 (lX,lY: single): TRGBQuad; //bilinear interpolation - weights four nearest neighbors
var
   ix,iy: integer;
   i00,i10,i01,i11:TRGBQuad;
begin
     ix := trunc(lX);
     iy := trunc(lY);
     i00 := SrcVal32(ix,iy);
     i10 := SrcVal32(ix+1,iy);
     i01 := SrcVal32(ix,iy+1);
     i11 := SrcVal32(ix+1,iy+1);

     result.rgbBlue :=round (
            i00.rgbBlue*( (1-frac(lX)) * (1-frac(lY)))+
            i10.rgbBlue*( (frac(lX)) * (1-frac(lY)))+
            i01.rgbBlue*( (1-frac(lX)) * (frac(lY)))+
            i11.rgbBlue*( (frac(lX)) * (frac(lY))));
     result.rgbGreen :=round (
            i00.rgbGreen*( (1-frac(lX)) * (1-frac(lY)))+
            i10.rgbGreen*( (frac(lX)) * (1-frac(lY)))+
            i01.rgbGreen*( (1-frac(lX)) * (frac(lY)))+
            i11.rgbGreen*( (frac(lX)) * (frac(lY))));
     result.rgbRed :=round (
            i00.rgbRed*( (1-frac(lX)) * (1-frac(lY)))+
            i10.rgbRed*( (frac(lX)) * (1-frac(lY)))+
            i01.rgbRed*( (1-frac(lX)) * (frac(lY)))+
            i11.rgbRed*( (frac(lX)) * (frac(lY))));
     result.rgbreserved:= kAlphaOpaque;
end;

function AsByte (f: single): byte;
begin
     if f > 255 then
        result := 255
     else if f < 0 then
          result := 0
     else
         result := round(f);
end;
function LanczosVal32 (lX,lY: single): TRGBQuad;
var
   ix,iy,dx,dy: integer;
   px : TRGBQuad;
   w,wt,r,g,b,fy: single;
   fx: array [kLanczosANeg..kLanczosA] of single;
begin
     ix := trunc(lX);
     iy := trunc(lY);
     r := 0;
     g := 0;
     b := 0;
     for dx := kLanczosANeg to kLanczosA do
         fx[dx] := LanczosHt(lX- dx-ix);
     wt := 0;
     for dy := kLanczosANeg to kLanczosA do begin
          fy := LanczosHt(lY-dy-iy);
          for dx := kLanczosANeg to kLanczosA do begin
              w := fx[dx]*fy;
              wt := wt + w;
              px := SrcVal32 (dx+ix,dy+iy);
              r := r + (px.rgbRed *w);
              g := g + (px.rgbGreen *w);
              b := b + (px.rgbBlue *w);
          end;
     end;
     if wt <> 0 then begin
        r := r/wt;
        g := g/wt;
        b := b/wt;
     end;
     result.rgbRed := AsByte(r);
     result.rgbGreen := AsByte(g);
     result.rgbBlue := AsByte(b);
     result.rgbreserved:= kAlphaOpaque;
end;
function KeysVal32 (lX,lY,lB,lC: single): TRGBQuad;
var
   ix,iy,dx,dy: integer;
   r,g,b,fy: single;
   px : TRGBQuad;
   fx: array [-1..2] of single;
begin
     ix := trunc(lX);
     iy := trunc(lY);
     r := 0;
     g := 0;
     b := 0;
     for dx := -1 to +2 do
         fx[dx] := MitchellHt(lX- dx-ix);
     for dy := (iy-1) to (iy+2) do begin
          fy := KeysHt(lY-dy,lB,lC);
          for dx := -1 to +2 do begin
              px := SrcVal32 (dx+ix,dy);
              r := r + (px.rgbRed*fx[dx]*fy);
              g := g + (px.rgbGreen*fx[dx]*fy);
              b := b + (px.rgbBlue*fx[dx]*fy);
          end;
     end;
     result.rgbRed := AsByte(r);
     result.rgbGreen := AsByte(g);
     result.rgbBlue := AsByte(b);
     result.rgbreserved:= kAlphaOpaque;
end;
begin
     result := kBackground32;//666 make this a constant
     if (lSrc.Ht < 1) or (lSrc.Wid< 1) then
        exit;
     if (lX < -1) or (lY < -1) or (lX > (lSrc.Wid+1)) or (lY > (lSrc.Ht+1)) then
        exit;
     if lInterp = kCubicFilter then
        result := KeysVal32(lX,lY,1,0)
     else if lInterp = kMitchellFilter then
        result := KeysVal32(lX,lY,1/3,1/3)
     else if lInterp = kCatRomFilter then
        result := KeysVal32(lX,lY,0,1/2)
     else if lInterp = kLanczosFilter then
        result := LanczosVal32(lX,lY)
     else if lInterp = kNearestFilter then
        result := SrcVal32(round(lX),round(lY))
     else  //default = 1 = linear
        result :=  LinearVal32(lX,lY);
end; //funct Intensity

function Intensity32 (lX,lY: single; lSrc: TImg): TRGBQuad;   overload;
begin
     result := Intensity32 (lX,lY, lSrc, kLinearFilter)
end;

function Intensity32 (lX,lY: single; lSrc: TImg; lBilinear: boolean): TRGBQuad; overload;
begin
     if lBilinear then
        result := Intensity32 (lX,lY, lSrc, kLinearFilter)
     else
         result := Intensity32 (lX,lY, lSrc, kNearestFilter)//nearest
end;

initialization
  kBackground32 := ColorToRGB32(clBlack);

end.

