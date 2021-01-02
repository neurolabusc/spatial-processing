unit img2image;
//converts between internal img format and TImage,TBitmap,PNG,BMP,JPEG

interface
{$DEFINE PNG} //<- set to "PNG" to support PNG images with Delphi, requires PNGImage unit


uses
{$ifndef UNIX}Windows,{$ENDIF}
{$IFDEF FPC} LResources, GraphType,// rgbgraphics,rgbroutines, // {888 for gettickcount->}LCLIntf,
{$ELSE}
   {$IFDEF PNG}PNGImage,{$ENDIF}
   ClipBrd,jpeg,mmsystem,
{$ENDIF}

  Classes, SysUtils,  Forms, Controls, Graphics, Dialogs, ExtCtrls,
   Buttons, imgfunc;
function SupportPNG: boolean;
procedure Save2BMP( lFilename: string; lImage: TImage);
function LoadBMP2Image(lFilename: string; lImage: TImage): boolean;
procedure SetDimension(var lImage: TImage; lBuff: TImg; lSelf: TForm);
procedure CopyInput(lSrc: TImage; var lDest: TImg; lForceGrayScale: boolean);

implementation

//uses Smooth;//888

function TColor2Gray (lRGB: TColor):byte;
var
   lR,lG,lB: byte;
begin
     lB := (lRGB shr 16) and 255;
     lG := (lRGB shr 8) and 255;
     lR := lRGB and 255;
     result := round( 0.3*lR + 0.59*lG + 0.11*lB);
end;


(*procedure CopyInput8(lSrc: TImage; var lDest: TImg);
const
{$IFDEF FPC}
  {$IFDEF UNIX} kFLIP = FALSE;{$ELSE}kFLIP = TRUE;{$ENDIF}
{$ELSE}
  kFLIP = FALSE;
{$ENDIF}
var
   lX,lY,lPos: integer;
   lRGB: TColor;
begin
    if (lSrc.Picture.Height < 1) or (lSrc.Picture.Width < 1) then
       exit;

    ResizeImg(lSrc.Picture.Height,lSrc.Picture.Width, lDest,false);
showmessage('tx'+inttostr(lDest.Ht)+'  '+inttostr(lDest.Wid)+'  ');
    lPos := 0;
    //lDest.PixelFormat32 := 8;
  if kFLIP then begin
    for lY := 1 to lDest.Ht do
         for lX := 1 to lDest.Wid do begin
             inc(lPos);
             lRGB := lSrc.Canvas.Pixels[lX-1,lY-1];
             lDest.Img^[lPos] := TColor2Gray(lRGB)
         end;
    exit;
  end;//flip
    for lY := lDest.Ht downto 1 do
         for lX := 1 to lDest.Wid do begin
             inc(lPos);
             lRGB := lSrc.Canvas.Pixels[lX-1,lY-1];
             lDest.Img^[lPos] := TColor2Gray(lRGB)
         end;

end; *)

{$IFDEF FPC}
Type
   TRGB = PACKED RECORD
           Blue,Green,Red: byte;
           //Red,Green,Blue: byte;
      end;
   TRGBra = array [1..1] of TRGB;
   TRGBp = ^TRGBra;

FUNCTION  RGB2RGBA(CONST Color:  TRGB):  TRGBquad;
     BEGIN
       WITH RESULT DO BEGIN
         rgbRed   := Color.Red;
         rgbGreen := Color.Green;
         rgbBlue  := Color.Blue;
         rgbReserved := 255;
       END
END ;

function RGB2Gray (lRGB: TRGB):byte;
begin
     result := round( 0.3*lRGB.Red + 0.59*lRGB.Green + 0.11*lRGB.Blue);
end;

function Byte2RGB (lA,lB,lC: byte): TRGB;
//this may be platform specific...
begin
   {$IFDEF Darwin}
    result.red := lA;
    result.green := lB;
    result.blue := lC;

   {$ELSE}
    result.red := lB;
    result.green := lA;
    result.blue := lC;
    {$ENDIF}
end;
   {$ENDIF}//FPC specific code

{$DEFINE notSLOW}

procedure CopyInput(lSrc: TImage; var lDest: TImg; lForceGrayScale: boolean);
var
   lX,lY,lPos: integer;
 {$IFDEF FPC}
 //Start: DWord;
   {$IFDEF SLOW}
   lRGB: TColor;
   {$ELSE}
   lExtra,lBPL,lBits,lPosS: integer;
   PData: PByte;
   lClr: TRGB;
   {$ENDIF}
 {$ELSE}
   lRGB: TColor;
 {$ENDIF}
begin
    if (lSrc.Picture.Height < 1) or (lSrc.Picture.Width < 1) then
       exit;
    (*if (lForceGrayScale) or (lSrc.Picture.Bitmap.PixelFormat = pf8bit) then begin
        CopyInput8(lSrc,lDest);
        exit;
    end; *)
    ResizeImg(lSrc.Picture.Height,lSrc.Picture.Width, lDest,true);
    lPos := 0;
    {$IFDEF FPC}
    //Start := GetTickCount;
      {$IFDEF SLOW}

      for lY := 1 to (lDest.Ht) do
         for lX := 1 to lDest.Wid do begin

             inc(lPos);
             lRGB := lSrc.Canvas.Pixels[lX-1,lY-1];
             lDest.Img^[lPos] := TColor2Gray(lRGB);
             lDest.Img32^[lPos] := ColorToRGB32(lRGB);
         end;
      {$ELSE}
      //On OSX, Pixels[X,Y] is exceptionally slow, this code works much faster, eg 8000 -> 4ms!
      lBits := lSrc.Picture.Bitmap.RawImage.Description.BitsPerPixel;
      lBPL := lSrc.Picture.Bitmap.RawImage.Description.BytesPerLine;
      lExtra := lBPL- (lDest.Wid*lBits div 8);
      PData := lSrc.Picture.Bitmap.RawImage.Data;
      lPosS := 1;
      for lY := 1 to (lDest.Ht) do begin
          for lX := 1 to lDest.Wid do begin
              inc(lPos);
              if lBits = 24 then begin
                    lClr := Byte2RGB( PData[lPosS], PData[lPosS+1], PData[lPosS+2]);
                 inc(lPosS,3);
              end else if lBits = 32 then begin
                 lClr := Byte2RGB( PData[lPosS], PData[lPosS+1], PData[lPosS+2]);
                 inc(lPosS,4);
              end;
              lDest.Img^[lPos] := RGB2Gray(lClr);
              lDest.Img32^[lPos] := RGB2RGBA(lClr);
         end;  //for X
         lPosS := lPosS+lExtra;
      end; //for Y
      {$ENDIF}
     //Form1.caption := inttostr(GetTickCount-Start);
    {$ELSE}
    for lY := lDest.Ht downto 1 do
         for lX := 1 to lDest.Wid do begin
             inc(lPos);
             lRGB := lSrc.Canvas.Pixels[lX-1,lY-1];
             lDest.Img^[lPos] := TColor2Gray(lRGB);
             lDest.Img32^[lPos] := ColorToRGB32(lRGB);
         end;
    {$ENDIF}
end;

function SupportPNG: boolean;
begin
 {$IFDEF PNG} result := true;
 {$ELSE} result := false;
 {$ENDIF}
end;

function LoadBMP2Image(lFilename: string; lImage: TImage): boolean;
{$IFDEF FPC}
var
   lIm: TImage;
   lExt: string;
   lI: integer;
begin
  result := false;
  if not fileexists(lFilename) then
        exit;
  lExt := ExtractFileExt(lFilename);
  if length(lExt) > 0 then
    for lI := 1 to length(lExt) do
       lExt[lI] := upcase(lExt[lI]);
  if (lExt ='.BMP')  then
     lImage.Picture.Bitmap.LoadFromFile(lFilename)
   else begin
     lIm := TImage.Create(application);
     lIm.Picture.LoadFromFile(lFilename);
     lImage.Picture.Bitmap.Height := lIm.Picture.Height;
     lImage.Picture.Bitmap.Width := lIm.Picture.Width;
     lImage.Canvas.Draw(0,0,lIm.Picture.Graphic);
     lImage.Picture.Bitmap.PaletteModified := true;
     lIm.Free;
  end;
  result := true;
end;
{$ELSE}
var
   {$IFDEF PNG}lPNG: TPNGObject; {lPNG: TPNGImage;}{$ENDIF}
   lJPG: TJPEGImage;
   lI: integer;
   lExt: string;
   lStreamLoaded: boolean;
   lStream: TmemoryStream;
begin
  result := false;
  if not fileexists(lFilename) then
        exit;
  lExt := ExtractFileExt(lFilename);
  if length(lExt) > 0 then
    for lI := 1 to length(lExt) do
       lExt[lI] := upcase(lExt[lI]);
  result := true;
  if (lExt ='.BMP')  then begin
     //lImage.Picture.Bitmap.LoadFromFile(lFilename);
     lImage.Picture.LoadFromFile(lFilename);
  end else if (lExt = '.PNG') then begin
           {$IFDEF PNG}
          lPNG := TPNGObject.Create;
          try
            lPNG.LoadFromFile(lFilename);
            lImage.Picture.Bitmap.Assign(lPNG);
            //lImage.Picture.Assign(lPNG);    //Convert data into png
          finally
            lPNG.Free;
          end
           (*lStreamLoaded := true;
           lStream := TMemoryStream.Create;
           try
              lStream.LoadFromFile(lFilename);
              lStream.Seek(0, soFromBeginning);
              lPNG := TPNGImage.Create;
              try
                 lPNG.LoadFromStream(lStream);
                 lStream.Free;
                 lStreamLoaded := false;
                 lImage.Picture.Bitmap.PixelFormat :=  lPNG.PixelFormat;
                 lImage.Picture.Bitmap.Height := lPNG.Height;
                 lImage.Picture.Bitmap.Width := lPNG.Width;
                 if lPNG.PixelFormat = pf8Bit then lImage.Picture.Bitmap.Palette := lPNG.Palette;
                 lImage.Canvas.Draw(0,0,lPNG);
                 lImage.Picture.Bitmap.PaletteModified := true;
              finally
                     lPNG.Free;
              end;
           finally
                if lStreamLoaded then lStream.Free;
           end;  //try..finally  *)
           {$ELSE}
           result := false;
           showmessage('Not compiled with PNG support.');
           {$ENDIF}
  end {PNG} else if ('.JPG'= lExt) or  ('.JPG'= lExt) then begin
           lStreamLoaded := true;
           lStream := TMemoryStream.Create;
           try
              lStream.LoadFromFile(lFilename);
              lStream.Seek(0, soFromBeginning);
              lJpg := TJPEGImage.Create;
              try
                 lJpg.LoadFromStream(lStream);
                 lStream.Free;
                 lStreamLoaded := false;
                 lImage.Picture.Bitmap.PixelFormat := pf24bit;
                 lImage.Picture.Bitmap.Height := lJpg.Height;
                 lImage.Picture.Bitmap.Width := lJpg.Width;
                 lImage.Canvas.Draw(0,0,lJpg);
              finally
                     lJPG.Free;
              end;
           finally
                  if lStreamLoaded then lStream.Free;
           end; //try..finally
  end {JPG} else begin
      result := false;
      showmessage('Unknown file extension: what is the format of this image?');
  end;
  lImage.Height := lImage.Picture.Height;
  lImage.Width := lImage.Picture.Width;
  lImage.refresh;
end;
{$ENDIF}


{$IFDEF FPC}
procedure SaveImgAsPNGCore (lImage: TBitmap; lFilename: string);
var
  PNG: TPortableNetworkGraphic;
begin
	if (lImage = nil) then begin
		Showmessage('No image found to save.');
		exit;
	end;
  PNG := TPortableNetworkGraphic.Create;
  try
    PNG.Assign(lImage);    //Convert data into png
    PNG.SaveToFile(ChangeFileExt(lFilename,'.png'));
  finally
    PNG.Free;
  end
end;
{$ENDIF}

procedure Save2BMP( lFilename: string; lImage: TImage);
{$IFDEF FPC}
var
   lExt: string;
   lI: integer;
begin
     lExt := ExtractFileExt(lFileName);
     if length(lExt) > 0 then
       for lI := 1 to length(lExt) do
          lExt[lI] := upcase(lExt[lI]);
     if (lExt='.BMP')  then
        lImage.Picture.Bitmap.SaveToFile(changefileext(lFileName,'.bmp'))
     else
         SaveImgAsPNGCore(lImage.Picture.Bitmap,changefileext(lFileName,'.png'));
end;
{$ELSE}
var
lExt: String;
lI: integer;
lJPG:  TJPEGImage;
begin

  lExt := ExtractFileExt(lFileName);
  if length(lExt) > 0 then
    for lI := 1 to length(lExt) do
       lExt[lI] := upcase(lExt[lI]);

     if lImage.Picture.Graphic = nil then begin
            showmessage('There is no image currently loaded to save.');
            exit;
     end;
     if ('.JPG'= lExt) or (lExt='.JPEG')  then begin
           lJPG := TJPEGImage.Create;
           TRY
              lJPG.CompressionQuality := 80;
              lJPG.Assign(lImage.Picture.Bitmap);
              lJPG.SaveToFile(ChangeFileExt(lFileName,'.jpg'));
           FINALLY
               lJPG.Free
           END;
     end else  if ('.BMP'= lExt)  then begin
  	lImage.Picture.Bitmap.SaveToFile( ChangeFileExt(lFileName,'.bmp' ));
     {$IFDEF PNG}
     end else begin

          with TPNGObject.Create do begin
            Assign(lImage.Picture.Bitmap);    //Convert data into png
            SaveToFile(lFilename);
            Free;
          end;

         (*with TPNGImage.Create do begin
           //filters(efNone, efSub, efUp, efAverage, efPaeth);
           Filter := [efNone];    //None typically best for 8-bit images Lossless compression handbook By Khalid Sayood
           Assign(lImage.Picture.Graphic);
           SaveToFile(ChangeFileExt(lFileName,'.png'));
           free;
         end;  *)
    end; //PNG
   {$ELSE}
    end else
  	lImage.Picture.Bitmap.SaveToFile( ChangeFileExt(lFileName,'.bmp' ));
   {$ENDIF}
end;
{$ENDIF}
function Tx: boolean;
begin
     result := true;

end;

{$IFNDEF FPC}
procedure SetDimension32(  var lImage: TImage; lBuff: TImg);
var
 sbBits : PByteArray;
 nBytesInImage,lPGWid,lPGHt: integer;
      lBMP: TBitmap;
begin
	 lPGWid := lBuff.Wid;
	 lPGHt := lBuff.Ht;
	 lBMP := TBitmap.Create;
	 TRY
            lBMP.PixelFormat := pf32bit;
            lBMP.Width := lPGwid;
            lBMP.Height := lPGHt;
            sbBits := lBmp.ScanLine[lPGHt-1]; //<-check
	    nBytesInImage := lPGWid*lPGHt * 4;
            CopyMemory(Pointer(sbBits),Pointer(lBuff.Img32),nBytesInImage);
			  lImage.Width := (lBmp.Width);//xx
        lImage.Height := (lBmp.Height);//xx
			  lImage.Picture.Graphic := lBMP;
	 FINALLY
			   lBMP.Free;
	 END; //try..finally
end;

procedure SetDimension8(var lImage: TImage; lBuff: TImg; lSelf: TForm);
  VAR
   PixMap: pointer;
   Bmp     : TBitmap;
   hBmp    : HBITMAP;
   BI      : PBitmapInfo;
   BIH     : TBitmapInfoHeader;
   lPixmapInt,lBuffInt: integer ;
   ImagoDC : hDC;
   I,lScanLineSz,lScanLineSz8: integer;
  BEGIN
     BIH.biSize := Sizeof(BIH);
     BIH.biWidth := lBuff.wid;
     BIH.biHeight := lBuff.Ht;
     BIH.biPlanes := 1;
     BIH.biBitCount := 8;
     BIH.biCompression := BI_RGB;
     BIH.biSizeImage := 0;
     BIH.biXPelsPerMeter := 0;
     BIH.biYPelsPerMeter := 0;
     BIH.biClrUsed := 0;
     BIH.biClrImportant := 0;
     BI := AllocMem(SizeOf(TBitmapInfoHeader) + 256*Sizeof(TRGBQuad));
     BI^.bmiHeader := BIH;
         for I:=0 to 255 do begin
             BI^.bmiColors[I].rgbRed     := I;
             BI^.bmiColors[I].rgbGreen    := I;
             BI^.bmiColors[I].rgbBlue      := I;
             BI^.bmiColors[I].rgbReserved := 0;
         end;
     Bmp        := TBitmap.Create;
     Bmp.Height := lBuff.Ht{width};
     Bmp.Width  := lBuff.wid;
     ImagoDC := GetDC(lSelf.Handle);
     hBmp:= CreateDIBSection(imagodc,bi^,DIB_RGB_COLORS,pixmap,0,0);
     lScanLineSz := lBuff.wid;
     if(lBuff.wid mod 4) <> 0 then
               lScanLineSz8 := 4*((lBuff.Wid + 3)div 4)
     else lScanLineSz8 := lBuff.wid;
     if lBuff.img <> nil then begin
        lPixmapInt  := Integer(pixmap);
        lBuffInt := Integer(lBuff.Img);
           For i:= (Bmp.Height-1)  downto 0 do
               CopyMemory(Pointer(lPixmapInt+lScanLineSz8*(i)),
                     Pointer(lBuffInt+i*lScanLineSz),lScanLineSz);
     end; //lBuff full
     ReleaseDC(0,ImagoDC);
     Bmp.Handle := hBmp;
     lImage.Picture.Assign(Bmp);
     lImage.width := Bmp.Width ;
     lImage.height := Bmp.Height;
     Bmp.Free;
     FreeMem( BI);
end;
{$ELSE} //if not delphi use FPC
 {$IFDEF Darwin}// LCLCocoa}
procedure DrawBMP( lx, ly: integer; var lBuff: Longintp; var lImage: TImage);
  var
    DestPtr: PInteger;
    row, i: integer;
  begin
    lImage.Width := lx;
    lImage.Height := ly;

    lImage.Picture.Bitmap.Width:=lx;
    lImage.Picture.Bitmap.Height:=ly;
    lImage.Picture.Bitmap.PixelFormat := pf32bit;
    if lBuff = nil then exit;
    lImage.Picture.Bitmap.BeginUpdate(False);
    i := 1;
    for row:= 0 to (ly-1) do begin
    //for row:= ly-1 downto 0 do begin
      DestPtr := PInteger(lImage.Picture.Bitmap.RawImage.GetLineStart(row));
      Move(lBuff^[i], DestPtr^, lx * sizeof(TRGBQuad));
      inc(i, lx);
    end;
    lImage.Picture.Bitmap.EndUpdate(False);

  end;
 {$ELSE}

procedure DrawBMP( lx, ly: integer; var lBuff: Longintp; var lImage: TImage);
var
  TempBitmap: TBitmap;
  lRGBBitmap: TRGB32Bitmap;
begin
  TempBitmap := TBitmap.Create;
    TempBitmap.Width := lx;
    TempBitmap.Height := ly;
    if lBuff <> nil then begin
          lRGBBitmap := TRGB32Bitmap.CreateFromData(@lBuff[0],lx,ly);
          DrawRGB32Bitmap(TempBitmap.Canvas.Handle, 0, 0, 0, 0, lx, ly,lRGBBitmap {Self});
    end;  //if lBuff=nil
    lImage.Picture.Bitmap := TempBitmap;
    lImage.Width := lx;
    lImage.Height := ly;
    TempBitmap.Free;
end;
{$ENDIF}

(*procedure DrawBMPrgb( lx, ly: integer; var lBuff: RGBQuadp; var lImage: TImage);
var
  TempBitmap: TBitmap;
  lRGBBitmap: TRGB32Bitmap;
begin
  TempBitmap := TBitmap.Create;
    TempBitmap.PixelFormat := pf32bit;
    TempBitmap.Width := lx;
    TempBitmap.Height := ly;
    if lBuff <> nil then begin
          lRGBBitmap := TRGB32Bitmap.CreateFromData(@lBuff[0],lx,ly);
          DrawRGB32Bitmap(TempBitmap.Canvas.Handle, 0, 0, 0, 0, lx, ly,lRGBBitmap {Self});
    end;
    lImage.Picture.Bitmap.PixelFormat := pf32bit;
    lImage.Picture.Bitmap := TempBitmap;
    lImage.Width := lx;
    lImage.Height := ly;
    TempBitmap.Free;
end;      *)
procedure SetDimension32(lImage: TImage; lBuff: TImg);
begin
    if (lBuff.img32 =nil) or (lBuff.wid < 1) or (lBuff.Ht < 1) then
       exit;
    //DrawBMPrgb( lBuff.Wid, lBuff.Ht, (lBuff.Img32),lImage);
    DrawBMP( lBuff.Wid, lBuff.Ht, Longintp(lBuff.Img32),lImage);
end;

procedure SetDimension8(lImage: TImage; lBuff: TImg);
{$DEFINE HORZFLIP} //flips image on horizon
var
  lPixel,lPos: integer;
  {$IFDEF HORZFLIP}
  lx,ly,lystart: integer;
  {$ENDIF}
  rgbRA : array [0..255] of longint;
  lBuff32: longintp;
begin
    if (lBuff.img =nil) or (lBuff.wid < 1) or (lBuff.Ht < 1) then
       exit;
    {$IFDEF Darwin}
    for lPos := 0 to 255 do
        rgbRA[lPos] :=  255 + (lPos shl 8) + (lPos shl 16)+(lPos shl 24);//look up table

    {$ELSE}
    for lPos := 0 to 255 do
        rgbRA[lPos] :=  lPos + (lPos shl 8) + (lPos shl 16)+(lPos shl 24);//look up table
    {$ENDIF}

    //  rgbRA[lPos] :=  lPos + (lPos shl 8) + (lPos shl 16)+(127 shl 24);//look up table
    //note: in the line above, the 127 is just to check alpha
    lPixel := lBuff.Wid*lBuff.Ht;
    getmem(lBuff32,lPixel*4);
{$IFDEF HORZFLIP}
       lPos := 0;
       for ly := 1 to lBuff.Ht do begin
           lystart := (lBuff.Ht-ly)*lBuff.Wid;
           for lx := 1 to lBuff.Wid do begin
               inc(lPos);
               lBuff32^[lPos] := rgbRA[lBuff.Img^[lystart+lx]];
           end;
       end;
{$ELSE}
           for lPos := 1 to lPixel do
            lBuff32^[lPos] := rgbRA[lBuff.Img^[lPos]];
{$ENDIF}
    DrawBMP( lBuff.Wid, lBuff.Ht, lBuff32,lImage);
    freemem(lBuff32);
end;
{$ENDIF}
procedure SetDimension(var lImage: TImage; lBuff: TImg; lSelf: TForm);
begin
     if lBuff.PixelFormat32 = 32 then
         SetDimension32(lImage,lBuff)
     else
     {$IFDEF FPC}
     SetDimension8(lImage,lBuff);
     {$ELSE}
         SetDimension8(lImage,lBuff,lSelf);
     {$ENDIF}
end;

end.
