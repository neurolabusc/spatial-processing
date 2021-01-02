unit smooth;
{$IFDEF FPC}
{$mode objfpc}{$H+}
// {$FPUTYPE SSE2}
{$ENDIF}
interface

uses

{$IFDEF FPC} LResources,LCLIntf,
{$ELSE}
Windows,ClipBrd,mmsystem,
{$ENDIF}

  Classes, SysUtils,  Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Spin, Buttons, StdCtrls, Menus, imgfunc,graphicfunctions,img2image,math;
type

   { TForm1 }

   TForm1 = class(TForm)
    BMPImage: TImage;
    Label1: TLabel;
    MainMenu1: TMainMenu;
    FileMenuItem: TMenuItem;
    ClearMenuItem: TMenuItem;
    DifferenceMenuItem: TMenuItem;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    RotateMenuItem: TMenuItem;
    TransformMenuItem: TMenuItem;
    SaveMenuItem: TMenuItem;
    OpenMenuItem: TMenuItem;
    OpenGSMenuItem: TMenuItem;
    QuitMenuItem: TMenuItem;
    OpenMeanMenuItem: TMenuItem;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    SaveDialog1: TSaveDialog;
    ImageSrc: TImage;
    ImageDst: TImage;
    ImageSrcCross: TImage;
    ImageDstCross: TImage;
    SmoothEdit: TSpinEdit;
    TransformCombo: TComboBox;
    procedure ClearMenuItemClick(Sender: TObject);
    procedure OpenTwo(Sender: TObject);
    procedure  DoOpenImage (lForceGrayScale: boolean);
    procedure OpenGSMenuItemClick(Sender: TObject);
    procedure OpenMeanMenuItemClick(Sender: TObject);
    procedure OpenMenuItemClick(Sender: TObject);
    procedure QuitMenuItemClick(Sender: TObject);
    //procedure SaveBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    //procedure OpenBMPBtnClick(Sender: TObject);
    procedure DrawInput;
    procedure RotateMenuItemClick(Sender: TObject);
    procedure SaveMenuItemClick(Sender: TObject);
    //procedure CopyInput(lSrc: TImage; var lDest: TImg);
    procedure ShowCrossSection (lInPut: boolean);
    //procedure SetDimensionGrayscale(lImage: TImage;   lBuff: TImg);
    procedure SmoothEditChange(Sender: TObject);
    procedure TransformComboChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

    procedure FormResize(Sender: TObject);
    procedure TransformMenuItemClick(Sender: TObject);
    //procedure Open2BMPBtnClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;
  gSrc,gDest: TImg;
implementation

{$IFNDEF FPC}//Delphi drawing...
{$R *.DFM}
{$ENDIF}


//Delphi direct drawing routines
(*procedure TForm1.SetDimensionGrayscale(lImage: TImage; lBuff: TImg);
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
     ImagoDC := GetDC(Self.Handle);
     hBmp:= CreateDIBSection(imagodc,bi^,DIB_RGB_COLORS,pixmap,0,0);
     lScanLineSz := lBuff.wid;
     if(lBuff.wid mod 4) <> 0 then
               lScanLineSz8 := 4*((lBuff.Wid + 3)div 4)
               //lScanLineSz8 := 8*((lPGWid + 7)div 8)
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
uses
     rgbgraphics,rgbroutines;
procedure DrawBMP( lx, ly: integer; var lBuff: Longintp; var lImage: TImage);
var
  TempBitmap: TBitmap;
  lRGBBitmap: TRGB32Bitmap;
begin
  TempBitmap := TBitmap.Create;
    TempBitmap.Width := lx;
    TempBitmap.Height := ly;
    if lBuff <> nil then begin
          lRGBBitmap := TRGB32Bitmap.CreateFromData(@lBuff[1],lx,ly);
          DrawRGB32Bitmap(TempBitmap.Canvas.Handle, 0, 0, 0, 0, lx, ly,lRGBBitmap {Self});
    end;  //if lBuff=nil
    lImage.Picture.Bitmap := TempBitmap;
    lImage.Width := lx;
    lImage.Height := ly;
    TempBitmap.Free;
end;

procedure TForm1.SetDimensionGrayscale(lImage: TImage; lBuff: TImg);
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
    for lPos := 0 to 255 do
      rgbRA[lPos] :=  lPos + (lPos shl 8) + (lPos shl 16)+(127 shl 24);//look up table
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
            lBuff32^[lPos] := rgbRA[lBuff^[lPos]];
{$ENDIF}
    DrawBMP( lBuff.Wid, lBuff.Ht, lBuff32,lImage);
    freemem(lBuff32);
end;
{$ENDIF}
  *)


procedure TForm1.DrawInput;
begin
     GenerateTestImg( 217,181, gSrc);
     //GenerateTestImg( 217,181, gDest);
     SetDimension(ImageSrc, gSrc,Form1);
end;


function GetInt (lTitle,lMsg: string; var lDefault: integer): boolean;
var
   lS: String;
begin
     result := false;
     lS := inttostr(lDefault);
     if not InputQuery(lTitle,lMsg,lS) then exit;
     result := true;
     try
        lDefault := StrToInt(lS);// Trailing blanks are not supported
     except
     on Exception : EConvertError do
        result := false;
     end;
end;

procedure TForm1.RotateMenuItemClick(Sender: TObject);
const
     kMaxSinc = 36;
var
   i,filt,rep: integer;
   lTemp: TImg;
   {$ifndef FPC}Start: DWord;{$endif}
begin
     if SmoothEdit.value = 0 then begin
        showmessage('Error: please enter a non-zero number for the degrees rotation.');
        exit;

     end;
     rep := 36;
     filt := kCatRomFilter;
     if not GetInt('Enter a number','Repeat rotating how many times (errors accumulate)?',rep) then exit;
     if not GetInt('Select interpolation filter' ,'0=Near,1=Lin,2=Lanczos,3=Cat,4=Mitch,5=Cubic',filt) then exit;
     if (filt = kLanczosFilter) and (rep > kMaxSinc) then begin
        showmessage('The Lanczos sinc filter is very slow - please select no more than '+inttostr(kMaxSinc)+' consecutive rotations.');
        exit;
     end;
     CreateImg(lTemp);
     CopyImg(gSrc,lTemp);
     {$ifndef FPC}Start := TimeGetTime;{$endif}
     for i := 1 to rep do begin
         RotateDeg(SmoothEdit.value,lTemp,gDest,filt);
         CopyImg(gDest,lTemp);
     end;
     //CopyImg(gDest,gSrc);
     {$ifndef FPC} Caption := 'Processing time: '+inttostr(TimeGetTime-Start);{$endif}
     FreeImg(lTemp);
     //SetDimension(ImageSrc, gSrc,Form1);
     SetDimension(ImageDst, gDest,Form1);
     ShowCrossSection(true);
     ShowCrossSection(false);
end;

procedure TForm1.SaveMenuItemClick(Sender: TObject);
begin
   if not SaveDialog1.Execute then exit;
      {$IFDEF FPC}
        BMPIMage.Picture.Bitmap.Width:=ImageDst.Picture.Bitmap.Width;
  BMPIMage.Picture.Bitmap.Height:=ImageDst.Picture.Bitmap.Height;

      BMPImage.Canvas.Rectangle(0,0,ImageDst.Picture.Bitmap.Width,ImageDst.Picture.Bitmap.Height);
    BMPImage.Canvas.Draw(0,0,ImageDst.Picture.Graphic);
    Save2BMP(SaveDialog1.Filename, BMPImage);
      {$ELSE}
      Save2BMP(SaveDialog1.Filename, ImageDst);
      {$ENDIF}
end;

procedure TForm1.ShowCrossSection (lInPut: boolean);
var
   lSrc,lBuff: TImg;
   lYOut,lInten,lX,lY,lPos,lYPos: integer;
begin
     lBuff.Wid := gSrc.Wid;
     lBuff.Ht := 256;
     lYOut := 256;
     if lInput then
        lSrc := gSrc
     else
         lSrc := gDest;
     getmem(lBuff.Img,  lBuff.Ht* lBuff.Wid);
     for lPos := 1 to (lBuff.Ht* lBuff.Wid) do
             lBuff.Img^[lPos] := 255;
     lYPos := ((gSrc.Ht div 2)-1) *gSrc.Wid;
     for lX := 1 to lBuff.Wid do begin
         if lSrc.PixelFormat32 = 32 then
            lInten := TRGBQuad2Gray(lSrc.Img32^[lX+lYPos])
         else
             lInten := (lSrc.Img^[lX+lYPos]) ;
         if lInten > lYOut then
            lInten := lYOut;
         if lInten > 0 then
            for lY := 1 to lInten do begin
                lBuff.Img^[lX+((lY-1)*lBuff.Wid)] := 0;
            end;
     end;
     if lInput then
         SetDimension(ImageSrcCross,  lBuff,Form1)
     else
         SetDimension(ImageDstCross, lBuff,Form1);
end;

procedure TForm1.SmoothEditChange(Sender: TObject);
begin
     if (SmoothEdit.value = 0) and (TransformCombo.ItemIndex <> 4) and (TransformCombo.ItemIndex <> 6) then begin
        CopyImg(gSrc,gDest);
     end else begin
        case TransformCombo.ItemIndex of
             1: applyPerspective(SmoothEdit.value,gSrc,gDest);
             2: applygradient(SmoothEdit.value, gSrc,gDest);
             3: ApplyNoise(SmoothEdit.value, gSrc,gDest);
             4: ApplyMedian(gSrc,gDest);
             5: ApplyFishEye(SmoothEdit.value, gSrc,gDest);
             6: ApplyOtsuImg(gSrc,gDest, SmoothEdit.value);
             7: ApplyBayer(gSrc,gDest);
             else
                 SmoothInput(SmoothEdit.value,gSrc,gDest);
        end;
     end;
     SmoothEdit.enabled := TransformCombo.ItemIndex <> 4; //median does not use this parameter
        SetDimension(ImageDst, gDest,Form1);
        ShowCrossSection(true);
        ShowCrossSection(false);
end;

procedure TForm1.TransformComboChange(Sender: TObject);
begin
     SmoothEditChange(nil);
end;  //TransformComboChange

{$DEFINE noDEBUG}
procedure TForm1.FormCreate(Sender: TObject);
begin
     {$IFDEF FPC}Application.ShowButtonGlyphs := sbgNever; {$ENDIF} //disable button glyphs
     CreateImg(gSrc);
     CreateImg(gDest);
     TransformCombo.ItemIndex  := 0;
     DrawInput;
     {$IFDEF DEBUG}
      if LoadBMP2Image('/Users/chris/documents/pas/smooth/T1.png', BMPImage) then
      //if LoadBMP2Image('C:\pas\Delphi\smooth\T1.png', BMPImage) then
     CopyInput(BMPImage, gSrc,false);
     SetDimension(ImageSrc, gSrc,Form1);
     {$ENDIF}
     SmoothEditChange(nil);
     if SupportPNG then begin
     OpenDialog1.Filter := 'Bitmap (bmp,jpg,png)|*.bmp;*.jpg;*.png';
       {$IFDEF FPC}
       SaveDialog1.Filter := 'Bitmap (png)|*.png|Bitmap (bmp)|*.bmp';
       {$ELSE}
       SaveDialog1.Filter := 'Bitmap (png)|*.png|Bitmap (bmp)|*.bmp|JPEG (jpg)|*.jpg';
       {$ENDIF}
     end;
end; //FormCreate







procedure TForm1.OpenMeanMenuItemClick(Sender: TObject);
begin
     //make multiple
     openDialog1.Options := openDialog1.Options+[ofAllowMultiSelect];
     if not OpenDialog1.execute then begin
        openDialog1.Options := openDialog1.Options-[ofAllowMultiSelect];
        exit;
     end;
     openDialog1.Options := openDialog1.Options-[ofAllowMultiSelect] ;
     if not MakeMeanImg (openDialog1.Files, gSrc, BMPImage) then exit;
     //if (Sender as TSpeedButton).Tag = 1 then
     //   ForceGrayScale(gSrc);
     SetDimension(ImageSrc, gSrc,Form1);
     SmoothEditChange(nil);



end;

procedure  TForm1.DoOpenImage (lForceGrayScale: boolean);
begin
     if not OpenDialog1.execute then  begin
        DrawInput;
        SmoothEditChange(nil);
     end else
         LoadBMP2Image(OpenDialog1.filename,BMPImage);
     CopyInput(BMPImage, gSrc,false);
     if lForceGrayScale then
        ForceGrayScale(gSrc);
     SetDimension(ImageSrc, gSrc,Form1);
     SmoothEditChange(nil);

end;

procedure TForm1.ClearMenuItemClick(Sender: TObject);
begin
     DrawInput;
     SmoothEditChange(nil);
end;

procedure TForm1.OpenTwo(Sender: TObject);
begin
     if not OpenDialog1.execute then
        exit;
     if not  LoadBMP2Image(OpenDialog1.filename,BMPImage) then
        exit;//BMPImage.Picture.LoadFromFile(OpenDialog1.filename);
     CopyInput(BMPImage, gSrc,false);

     if not OpenDialog1.execute then
        exit;
     if not LoadBMP2Image(OpenDialog1.filename,BMPImage) then
        exit;//BMPImage.Picture.LoadFromFile(OpenDialog1.filename);
     CopyInput(BMPImage, gDest,false);
     Calc2Img( (Sender as TMenuItem).tag,gSrc,gDest,gSrc);
     SetDimension(ImageSrc, gSrc,Form1);
     SmoothEditChange(nil);
end;

procedure TForm1.OpenMenuItemClick(Sender: TObject);
begin
  DoOpenImage(false);
end;

procedure TForm1.OpenGSMenuItemClick(Sender: TObject);
begin
  DoOpenImage(true);
end;

procedure TForm1.QuitMenuItemClick(Sender: TObject);
begin
     Close;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
     FreeImg(gSrc);
     FreeImg(gDest);
end; //FormDestroy









procedure TForm1.FormResize(Sender: TObject);
const
     kBrdr = 5;
var
   lW,lH: integer;
begin
     lW := Form1.ClientWidth;
     lH := Form1.ClientHeight;// - Panel1.Height;
     lW := lW div 2;
     lH := lH-257;//lH div 2;
     if lW < 32 then
        lW := 32;
     if lH < 32 then
        lH := 32;
     ImageDst.Left := lW;
     ImageDstCross.Left := lW;
     ImageDstCross.Top := lH;
     ImageSrcCross.Top := lH;
end;

procedure TForm1.TransformMenuItemClick(Sender: TObject);
begin

end;

initialization
{$IFDEF FPC}
  {$I smooth.lrs}
{$ENDIF}
end.

