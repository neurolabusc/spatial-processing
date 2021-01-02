unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  LCLType, ComCtrls, math;

type
  TNIFTIhdr = packed record //Next: analyze Format Header structure
    dimxy: array[0..2] of smallint; //Data array dimensions
  end; //TNIFTIhdr Header Structure
  { TForm1 }

  TForm1 = class(TForm)
    AACheck: TCheckBox;
    FilterCombo: TComboBox;
    Label1: TLabel;
    LoadBtn: TButton;
    Image1: TImage;
    Image2: TImage;
    SaveBtn: TButton;
    OpenDialog1: TOpenDialog;
    AboutBtn: TButton;
    SaveDialog1: TSaveDialog;
    UpdateTimer: TTimer;
    TrackBar1: TTrackBar;
    procedure AboutBtnClick(Sender: TObject);
    procedure FilterComboChange(Sender: TObject);
    procedure LoadBmp(fnm: string);
    procedure FormShow(Sender: TObject);
    procedure LoadBtnClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
    procedure UpdateTimerTimer(Sender: TObject);
  private

  public

  end;
  TRGBquad = PACKED RECORD
   {$IFDEF DARWIN} //MacOS is ABGR
   rgbAlpha,rgbRed,rgbGreen,rgbBlue: byte;
   {$ELSE} //Linux and Windows are RGBA
   rgbBlue,rgbGreen,rgbRed,rgbAlpha: byte;
   {$ENDIF}
  end;
  TQuadRA = array [1..1] of TRGBQuad;
  RGBQuadp = ^TQuadRA;
  TByteRA = array [1..1] of byte;
  Bytep = ^TByteRA;
  TSingleRA = array [1..1] of Single;
  Singlep = ^TSingleRA;

var
  Form1: TForm1;

implementation

{$R *.lfm}
var
  gHdr: TNIFTIhdr;
  gBuffer: bytep = nil;
const
  kDT_UNSIGNED_CHAR          =2;     // unsigned char (8 bits/voxel)


{ TForm1 }

procedure DrawBMP( lx, ly: integer; var lBuff: RGBQuadp; var lImage: TImage);
var
  DestPtr: PInteger;
  row, i: integer;
begin
  lImage.Picture.Bitmap.Width:=lx;
  lImage.Picture.Bitmap.Height:=ly;
  //{$IFNDEF LCLCocoa}
  lImage.Picture.Bitmap.PixelFormat := pf32bit;
  //{$ENDIF}
  if lBuff = nil then exit;
  lImage.Picture.Bitmap.BeginUpdate(False);
  i := 1;
  for row:= 0 to (ly-1) do begin
    DestPtr := PInteger(lImage.Picture.Bitmap.RawImage.GetLineStart(row));
    Move(lBuff^[i], DestPtr^, lx * sizeof(TRGBQuad));
    inc(i, lx);
  end;
  lImage.Picture.Bitmap.EndUpdate(False);
end;

function GrayRGBA(Gray: Byte): TRGBquad;
begin
  result.rgbRed:= (Gray);
  result.rgbGreen:= (Gray);
  result.rgbBlue:= (Gray);
  {$IFDEF UNIX}
  result.rgbAlpha:= 255;
  {$ELSE}
  result.rgbAlpha:= 0;
  {$ENDIF}
end;

procedure CreateBmp(var lImage: TImage; var lHdr: TNIFTIhdr; var lBuffer: bytep);
var
  i: integer;
  lBuff: RGBQuadp;
begin
  lImage.Width := lHdr.dimxy[1];
  lImage.Height := lHdr.dimxy[2];
  GetMem(lBuff, lHdr.dimxy[1]*lHdr.dimxy[2]* sizeof(TRGBQuad));
  for i :=  1 to (lHdr.dimxy[1]*lHdr.dimxy[2]) do
      lBuff^[i] := GrayRGBA(lBuffer^[i]);
  DrawBMP(lHdr.dimxy[1],lHdr.dimxy[2], lBuff, lImage);
  FreeMem(lBuff);
end;

procedure Zoom(var lHdr: TNIFTIhdr; lScale: single);
//if we have a 256x256x256 pixel image with scale of 0.5, output is 128x128x128
//if we have a 1x1x1mm pixel image with a scale of 2.0, output is 2x2x2mm
begin
     lHdr.dimxy[1] := round(lHdr.dimxy[1] * lScale);
     lHdr.dimxy[2] := round(lHdr.dimxy[2] * lScale);

end;

// Hermite filter

function HermiteFilter(Value: Single): Single;
begin
  // f(t) = 2|t|^3 - 3|t|^2 + 1, -1 <= t <= 1
  if (Value < 0.0) then
    Value := -Value;
  if (Value < 1.0) then
    Result := (2.0 * Value - 3.0) * Sqr(Value) + 1.0
  else
    Result := 0.0;
end;

// Box filter
// a.k.a. "Nearest Neighbour" filter
// anme: I have not been able to get acceptable
//       results with this filter for subsampling.

function BoxFilter(Value: Single): Single;
begin
  if (Value > -0.5) and (Value <= 0.5) then
    Result := 1.0
  else
    Result := 0.0;
end;

// Triangle filter
// a.k.a. "Linear" or "Bilinear" filter

function TriangleFilter(Value: Single): Single;
begin
  if (Value < 0.0) then
    Value := -Value;
  if (Value < 1.0) then
    Result := 1.0 - Value
  else
    Result := 0.0;
end;

// Bell filter

function BellFilter(Value: Single): Single;
begin
  if (Value < 0.0) then
    Value := -Value;
  if (Value < 0.5) then
    Result := 0.75 - Sqr(Value)
  else if (Value < 1.5) then
  begin
    Value := Value - 1.5;
    Result := 0.5 * Sqr(Value);
  end
  else
    Result := 0.0;
end;

// B-spline filter

function SplineFilter(Value: Single): Single;
var
  tt: single;
begin
  if (Value < 0.0) then
    Value := -Value;
  if (Value < 1.0) then
  begin
    tt := Sqr(Value);
    Result := 0.5 * tt * Value - tt + 2.0 / 3.0;
  end
  else if (Value < 2.0) then
  begin
    Value := 2.0 - Value;
    Result := 1.0 / 6.0 * Sqr(Value) * Value;
  end
  else
    Result := 0.0;
end;

// Lanczos3 filter

function Lanczos3Filter(Value: Single): Single;

function SinC(Value: Single): Single;
  begin
    if (Value <> 0.0) then
    begin
      Value := Value * Pi;
      Result := sin(Value) / Value
    end
    else
      Result := 1.0;
  end;
begin
  if (Value < 0.0) then
    Value := -Value;
  if (Value < 3.0) then
    Result := SinC(Value) * SinC(Value / 3.0)
  else
    Result := 0.0;
end;

function MitchellFilter(Value: Single): Single;
const
  B = (1.0 / 3.0);
  C = (1.0 / 3.0);
var
  tt: single;
begin
  if (Value < 0.0) then
    Value := -Value;
  tt := Sqr(Value);
  if (Value < 1.0) then
  begin
    Value := (((12.0 - 9.0 * B - 6.0 * C) * (Value * tt))
      + ((-18.0 + 12.0 * B + 6.0 * C) * tt)
      + (6.0 - 2 * B));
    Result := Value / 6.0;
  end
  else if (Value < 2.0) then
  begin
    Value := (((-1.0 * B - 6.0 * C) * (Value * tt))
      + ((6.0 * B + 30.0 * C) * tt)
      + ((-12.0 * B - 48.0 * C) * Value)
      + (8.0 * B + 24 * C));
    Result := Value / 6.0;
  end
  else
    Result := 0.0;
end;

type
  // Contributor for a pixel
  TFilterProc = function(Value: Single): Single;
  TContributor = record
    pixel: integer; // Source pixel
    weight: single; // Pixel weight
  end;
  TContributorList = array[0..0] of TContributor;
  PContributorList = ^TContributorList;
  // List of source pixels contributing to a destination pixel
  TCList = record
    n: integer;
    p: PContributorList;
  end;
  TCListList = array[0..0] of TCList;
  PCListList = ^TCListList;

procedure SetContrib(out contrib: PCListList; SrcPix, DstPix, Delta: integer; xscale, fwidth: single; filter: TFilterProc; isAntiAlias: boolean);
var
  i,j,k: integer;
  width, fscale: single;
  sum, center, weight: single; // Filter calculation variables
  left, right: integer; // Filter calculation variables
begin
  if (DstPix < 1) or (xscale <= 0) then exit;
  if (xscale < 1) and (isAntiAlias) then
  	fscale := 1.0 / xscale
  else
  	fscale := 1.0;
  width := fwidth * fscale;
  GetMem(contrib, DstPix * sizeof(TCList));
  for i := 0 to DstPix - 1 do begin
      contrib^[i].n := 0;
      GetMem(contrib^[i].p, trunc(width * 2.0 + 1) * sizeof(TContributor));
      center := i / xscale;
      left := floor(center - width);
      left := max(left,0);
      right := ceil(center + width);
      right := min(right, SrcPix - 1);
      sum := 0.0;
      for j := left to right do begin
        weight := filter((center - j) / fscale) / fscale;
        if (weight = 0.0) then
          continue;
        sum := sum + weight;
        k := contrib^[i].n;
        contrib^[i].n := contrib^[i].n + 1;
        contrib^[i].p^[k].pixel := j * Delta;
        contrib^[i].p^[k].weight := weight;
      end;
      for k := 0 to contrib^[i].n - 1 do
          contrib^[i].p^[k].weight := contrib^[i].p^[k].weight/sum;
    end;
end;

procedure ShrinkLarge8(var lHdr: TNIFTIhdr; var lBuffer: bytep; xscale, fwidth: single; filter: TFilterProc; isAntiAlias: boolean);
//rescales images with any dimension larger than lMaxDim to have a maximum dimension of maxdim...
label
  666;
var
  sum, mx, mn: single;
  lineStart, x,y,bytesPerVox, lXo,lYo,lXi,lYi,inBytes, outBytes, i,j: integer;
  contrib: PCListList;
  inImg: bytep;
  finalImg, tempImgX, tempImgY, tempImgZ: Singlep;
begin
  bytesPerVox := 1;
  lXi := lHdr.dimxy[1]; //input X
  lYi := lHdr.dimxy[2]; //input Y
  lXo := lXi; lYo := lYi;  //output initially same as input
  inBytes := lHdr.dimxy[1]*lHdr.dimxy[2]*bytesPerVox;
  getmemory(inImg, inBytes);
  Move(lBuffer^,inImg^,inBytes);
  Freemem( lBuffer);
  //find min/max values
  mn := inImg^[1];
  mx := mn;
  for i := 1 to (lHdr.dimxy[1]*lHdr.dimxy[2]) do begin
      if inImg^[i] < mn then mn := inImg^[i];
      if inImg^[i] > mx then mx := inImg^[i];
  end;
  Zoom(lHdr,xscale);
  //output
  //shrink in 1st dimension : do X as these are contiguous = faster, compute slower dimensions at reduced resolution
  lXo := lHdr.dimxy[1]; //input X
  GetMem( tempImgX,lXo*lYi*sizeof(single)); //8
  SetContrib(contrib, lXi, lXo, 1, xscale, fwidth, filter, isAntiAlias);
  i := 1;
    for y := 0 to (lYi-1) do begin
        lineStart := 1+ (lXi * y);
        for x := 0 to (lXo - 1) do begin
            sum := 0.0;
            for j := 0 to contrib^[x].n - 1 do begin
              sum := sum + (contrib^[x].p^[j].weight * inImg^[lineStart +contrib^[x].p^[j].pixel]);
            end;
            tempImgX^[i] := sum;
            i := i + 1;
        end; //for X
    end; //for Y
  for i := 0 to lXo - 1 do
     FreeMem(contrib^[i].p);
  FreeMem(contrib);
  //shrink in 2nd dimension
  lYo := lHdr.dimxy[2]; //reduce Y output
  GetMem( tempImgY,lXo*lYo*sizeof(single)); //8
  SetContrib(contrib, lYi, lYo, lXo, xscale, fwidth, filter, isAntiAlias);
  i := 1;
      for y := 0 to (lYo - 1) do begin
          for x := 0 to (lXo-1) do begin
            lineStart :=  1+x;
            sum := 0.0;
            for j := 0 to contrib^[y].n - 1 do begin
              //sum := sum + (contrib^[y].p^[j].weight * sourceLine^[contrib^[y].p^[j].pixel]);
              sum := sum + (contrib^[y].p^[j].weight * tempImgX^[lineStart +contrib^[y].p^[j].pixel] );
            end;
            tempImgY^[i] := sum;
            i := i + 1;
        end; //for X
    end; //for Y
  for i := 0 to lYo - 1 do
     FreeMem(contrib^[i].p);
  FreeMem(contrib);
  Freemem( tempImgX);
  finalImg := tempImgY;
666:
  lHdr.dimxy[1] := lXo;
  lHdr.dimxy[2] := lYo;
  outBytes := lHdr.dimxy[1] * lHdr.dimxy[2] *bytesPerVox;
  GetMem( lBuffer,outBytes); //8
  for i := 1 to ((lXo*lYo)) do begin
      //check image range - some interpolation can cause ringing
      // e.g. if input range 0..1000 do not create negative values!
      if finalImg^[i] > mx then finalImg^[i] := mx;
      if finalImg^[i] < mn then finalImg^[i] := mn;
      lBuffer^[i] := round(finalImg^[i]);
  end;
  //lBuffer^[(lXo*lYo)-2] := 255;
  //lBuffer^[(lXo*lYo)-1] := 255;
  Freemem( finalImg);
end; //ShrinkLarge8()

procedure TForm1.LoadBMP(fnm: string);
const
  kSz = 64;
  kXFreq = kSz / kSz;
  kYFreq = 32 / kSz;
var
  ScanData: PRGBQuad;
  lImage, lIm: TImage;
  x,y,i: integer;
  v : single;
begin
  if (length(fnm) > 0) and (not FileExists(fnm)) then begin
   showmessage('unable to find '+fnm);
   exit;
  end;
  if length(fnm) < 1 then begin
    gHdr.dimxy[1] := kSz;
    gHdr.dimxy[2] := kSz;
    getmem(gBuffer, gHdr.dimxy[1] * gHdr.dimxy[2]);
    i := 1;
    for y := 0 to (gHdr.dimxy[2]-1) do begin
        for x := 1 to gHdr.dimxy[1] do begin
            v := (96* (1+sin(x*kXFreq)))+ (96* (1+sin(y*kYFreq)));
            if (v > 255) then v := 255;
            gBuffer^[i] :=  round(v);
          i := i + 1;
        end;
    end;
  end else begin
    lImage := TImage.Create(application);
    if (upcase(extractfileext(fnm)) ='.BMP')  then
       lImage.Picture.Bitmap.LoadFromFile(fnm)
     else begin
       lIm := TImage.Create(application);
       lIm.Picture.LoadFromFile(fnm);
       lImage.Picture.Bitmap.Height := lIm.Picture.Height;
       lImage.Picture.Bitmap.Width := lIm.Picture.Width;
       lImage.Canvas.Draw(0,0,lIm.Picture.Graphic);
       lImage.Picture.Bitmap.PaletteModified := true;
       lIm.Free;
    end;
    gHdr.dimxy[1] := lImage.Picture.Bitmap.Width;
    gHdr.dimxy[2] := lImage.Picture.Bitmap.Height;
    if (gBuffer <> nil) then freemem(gBuffer);
    getmem(gBuffer, gHdr.dimxy[1] * gHdr.dimxy[2]);
    i := 1;
    for y := 0 to (gHdr.dimxy[2]-1) do begin
        ScanData := lImage.Picture.Bitmap.ScanLine[y];
        for x := 1 to gHdr.dimxy[1] do begin
          gBuffer^[i] := ScanData^.rgbGreen;
          Inc(ScanData, 1);
          i := i + 1;
        end;
    end;
    lImage.Free;
  end;
  CreateBmp(Image1,gHdr,gBuffer);
  Image2.Left := Image1.Left + 32 + gHdr.dimxy[1];
  UpdateTimer.Enabled := true;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
     LoadBmp('');
end;


procedure TForm1.FilterComboChange(Sender: TObject);
begin
     UpdateTimer.Enabled:=true;
end;

procedure TForm1.AboutBtnClick(Sender: TObject);
begin
  Showmessage('Shrinking images can cause aliasing artifacts. The "anti-alias" option combats this as described by Dale Schumacher (1994) General Filtered Image Rescaling');
  //
end;
(*
https://blogs.mathworks.com/steve/2017/01/16/aliasing-and-image-resizing-part-3/
Matlab code to create zone plate:

[x,y] = meshgrid(-128:128);
r = hypot(x,y);
km = 0.8*pi;
rm = 200;
w = rm/10;
term1 = sin( (km * r.^2) / (2 * rm) );
term2 = 0.5*tanh((rm - r)/w) + 0.5;
g = term1 .* term2;
size(g)
imwrite(g,'fresnel_zone_plate.png');

*)

procedure TForm1.LoadBtnClick(Sender: TObject);
begin
     if not OpenDialog1.Execute then exit;
     LoadBmp(OpenDialog1.Filename);
end;

procedure TForm1.SaveBtnClick(Sender: TObject);
var
  PNG: TPortableNetworkGraphic;
begin
     if not SaveDialog1.execute then exit;

       PNG := TPortableNetworkGraphic.Create;
       try
         PNG.Assign(Image2.Picture.Bitmap);    //Convert data into png
         PNG.SaveToFile(ChangeFileExt(SaveDialog1.Filename,'.png'));
       finally
         PNG.Free;
       end;
end;

procedure TForm1.UpdateTimerTimer(Sender: TObject);
var
   fwidth: single;
   filter: TFilterProc;
   scale: double;
   lHdr: TNIFTIhdr;
   inBytes: int64;
   lBuffer: bytep;
begin
  if UpdateTimer.Tag > 0 then
      exit; //wait for next cycle;
  UpdateTimer.Enabled := false;
  UpdateTimer.Tag := 1; //busy
  scale := TrackBar1.Position / 10;
  if (scale < 0.1) then exit;
  case FilterCombo.ItemIndex of
    0: begin filter := @BoxFilter; fwidth := 0.5; end;
    1: begin filter := @TriangleFilter; fwidth := 1; end;
    2: begin filter := @HermiteFilter; fwidth := 1; end;
    3: begin filter := @BellFilter; fwidth := 1.5; end;
    4: begin filter := @SplineFilter; fwidth := 2; end;
    5: begin filter := @Lanczos3Filter; fwidth := 3; end;
    else begin filter := @MitchellFilter; fwidth := 2; end;
  end;
  lHdr := gHdr;
  inBytes := lHdr.dimxy[1]*lHdr.dimxy[2]*1;
  getmemory(lBuffer, inBytes);
  Move(gBuffer^,lBuffer^,inBytes);
  ShrinkLarge8(lHdr, lBuffer, scale, fwidth, filter, AACheck.checked);
  CreateBmp(Image2,lHdr,lBuffer);
  freememory(lBuffer);
  UpdateTimer.Tag := 0; //not busy
end;


end.

