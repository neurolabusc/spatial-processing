unit MatrixForm;
{$IFDEF FPC}{$MODE Delphi}{$ENDIF}
interface
uses
  {$IFDEF FPC}LResources, LCLIntf,{$ELSE}Windows,{$ENDIF} SysUtils, Classes, Controls, Forms, Dialogs,
  Buttons,ExtCtrls, StdCtrls, Graphics,
  ComCtrls,  Menus, img2image,imgfunc,matrices;
type
  TOpType = (opNone, opTranslate, opRotate, opScale, opShear);
  TOpRec = record
    OpType: TOpType;
    Dx, Dy: Extended;        // shifts for opTranslate mode
    Sx, Sy: Extended;        // scale factors for opScale mode
    Cx, Cy, Alpha: Extended; // rotation center and angle (deg) for opRotate mode
    Fx, Fy: Extended;        // shear factors for opShear mode
  end;
  TOpRecs = array[0..7] of TOpRec;

const
  OpTypes: array [0..4] of TOpType = (opNone,opTranslate,opRotate, opScale, opShear);
type

{$IFNDEF FPC}
  TForm1 = class(TForm)
    BenchBtn: TButton;
    eCx: TEdit;
    eCy: TEdit;
    eAlpha: TEdit;
    eFY: TEdit;
    eFX: TEdit;
    eSy: TEdit;
    eSx: TEdit;
    eDx: TEdit;
    eDy: TEdit;
    Label11: TLabel;
    Label18: TLabel;
    Label13: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label12: TLabel;
    Label14: TLabel;
    Label17: TLabel;
    Label2: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    ResizeWarnLabel2: TLabel;
    ResizeWarnLabel1: TLabel;
    Label5: TLabel;
    Label3: TLabel;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Opensource1: TMenuItem;
    Saveoutput1: TMenuItem;
    SaveDialog1: TSaveDialog;
    Panel3: TPanel;
    ListBox: TListBox;
    Panel1: TPanel;
    Label1: TLabel;
    Notebook: TNotebook;
    Label4: TLabel;
    ComboBox: TComboBox;
    Label10: TLabel;
    CodeString: TEdit;
    Label9: TLabel;
    About1: TMenuItem;
    sbDx: TTrackBar;
    sbDy: TTrackBar;
    sbAlpha: TTrackBar;
    sbSx: TTrackBar;
    sbSy: TTrackBar;
    sbFX: TTrackBar;
    sbFY: TTrackBar;
    Version1: TMenuItem;
    OpenDialog1: TOpenDialog;
    Shape1: TShape;
    Shape2: TShape;
    Label21: TLabel;
    InterpolationDrop: TComboBox;
    ResizeOutCheck: TCheckBox;
    Panel2: TPanel;
    ImageSrc: TImage;
    ImageDest: TImage;
    ImageKernel: TImage;
    MatrixLabel: TLabel;
    ClearBtn: TButton;
    {TransPage: TPage;
    RotPage: TPage;
    ScalePage: TPage;
    ShearPage: TPage;
    NilPage: TPage;
    BMPImage: TImage;}

    procedure NewSource;
    procedure FormCreate(Sender: TObject);
    procedure ListBoxClick(Sender: TObject);
    procedure ComboBoxChange(Sender: TObject);
    procedure TranslationChanged(Sender: TObject);
    procedure ScaleChanged(Sender: TObject);
    procedure TranslationScrolled(Sender: TObject);
    procedure ClearBtnClick(Sender: TObject);
    procedure ScaleScrolled(Sender: TObject);
    procedure RotationChanged(Sender: TObject);
    procedure RotationScrolled(Sender: TObject);
    procedure ShearChanged(Sender: TObject);
    procedure ShearScrolled(Sender: TObject);
    procedure Opensource1Click(Sender: TObject);
    procedure Saveoutput1Click(Sender: TObject);
    procedure Version1Click(Sender: TObject);
    procedure InterpolationDropChange(Sender: TObject);
    procedure ResizeOutCheckClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BenchClick(Sender: TObject);

  protected
{$ELSE}
  //This is cumbersome - Delphi deals with pages differently then Lazarus, and I can not use IFDEF comment out individual components

  { TForm1 }

  TForm1 = class(TForm)
    BenchBtn: TButton;
    eCx: TEdit;
    eCy: TEdit;
    eAlpha: TEdit;
    eFY: TEdit;
    eFX: TEdit;
    eSy: TEdit;
    eSx: TEdit;
    eDx: TEdit;
    eDy: TEdit;
    BMPImage: TImage;
    Label11: TLabel;
    Label18: TLabel;
    Label13: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label12: TLabel;
    Label14: TLabel;
    Label17: TLabel;
    Label2: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    ResizeWarnLabel2: TLabel;
    ResizeWarnLabel1: TLabel;
    Label5: TLabel;
    Label3: TLabel;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Opensource1: TMenuItem;
    Saveoutput1: TMenuItem;
    SaveDialog1: TSaveDialog;
    Panel3: TPanel;
    ListBox: TListBox;
    Panel1: TPanel;
    Label1: TLabel;
    Notebook: TNotebook;
    Label4: TLabel;
    ComboBox: TComboBox;
    Label10: TLabel;
    CodeString: TEdit;
    Label9: TLabel;
    About1: TMenuItem;
    sbDx: TTrackBar;
    sbDy: TTrackBar;
    sbAlpha: TTrackBar;
    sbSx: TTrackBar;
    sbSy: TTrackBar;
    sbFX: TTrackBar;
    sbFY: TTrackBar;
    Version1: TMenuItem;
    OpenDialog1: TOpenDialog;
    Shape1: TShape;
    Shape2: TShape;
    Label21: TLabel;
    InterpolationDrop: TComboBox;
    ResizeOutCheck: TCheckBox;
    Panel2: TPanel;
    ImageSrc: TImage;
    ImageDest: TImage;
    ImageKernel: TImage;
    MatrixLabel: TLabel;
    ClearBtn: TButton;
    TransPage: TPage;
    RotPage: TPage;
    ScalePage: TPage;
    ShearPage: TPage;
    NilPage: TPage;

    procedure NewSource;
    procedure FormCreate(Sender: TObject);
    procedure ListBoxClick(Sender: TObject);
    procedure ComboBoxChange(Sender: TObject);
    procedure TranslationChanged(Sender: TObject);
    procedure ScaleChanged(Sender: TObject);
    procedure TranslationScrolled(Sender: TObject);
    procedure ClearBtnClick(Sender: TObject);
    procedure ScaleScrolled(Sender: TObject);
    procedure RotationChanged(Sender: TObject);
    procedure RotationScrolled(Sender: TObject);
    procedure ShearChanged(Sender: TObject);
    procedure ShearScrolled(Sender: TObject);
    procedure Opensource1Click(Sender: TObject);
    procedure Saveoutput1Click(Sender: TObject);
    procedure Version1Click(Sender: TObject);
    procedure InterpolationDropChange(Sender: TObject);
    procedure ResizeOutCheckClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BenchClick(Sender: TObject);

  protected
{$ENDIF}
    gLoadingValues: Boolean;
     {$IFDEF XX} ShearPage: TPage;{$ENDIF}
  public
    Operation: TOpRecs;
    Current: ^TOpRec;
    gMat: TMatrix2D;
      gSrc,gDest: TImg;
    procedure ClearTransformations;
    procedure DoTransform;
    procedure GenTransform;
    procedure ShowSettings(OperationNum: Integer);
  end;
var
  Form1: TForm1;

implementation
{$IFNDEF FPC}//Delphi drawing...
{$R *.DFM}
{$ENDIF}


type TSPoint = packed record
     X: single;
     Y: single;
end;
procedure BoundingBox (lMat: TMatrix2D; lSrc: TImg; var lXoffset,lYoffset: single; var lXSz,lYSz: integer);
function Pos (lX,lY: integer): TSPoint;
begin
     result.x := (lX*lMat[1,1])+(lY*lMat[2,1])     +lMat[3,1];
     result.y := (lX*lMat[1,2])+(lY*lMat[2,2])     +lMat[3,2];
end;
procedure MinMax (lX,lY: integer; var lMin,lMax: TSPoint);
var
   lNew: TSPoint;
begin
     lNew := Pos(lX,lY);
     if lNew.X < lMin.X then
        lMin.X := lNew.X;
     if lNew.X > lMax.X then
        lMax.X := lNew.X;
     if lNew.Y < lMin.Y then
        lMin.Y := lNew.Y;
     if lNew.Y > lMax.Y then
        lMax.Y := lNew.Y;
end;
var
   lMin,lMax: TSPoint;
begin
     lMin := Pos(1,1);
     lMax := Pos(1,1);
     MinMax(1,lSrc.Ht,lMin,lMax);
     MinMax(lSrc.Wid,1,lMin,lMax);
     MinMax(lSrc.Wid,lSrc.Ht,lMin,lMax);
     lXOffset:= lMin.X-1;//indexed from zero not 1
     lYOffset := lMin.Y-1;
     lXSz := trunc(lMax.X-lMin.X+0.9999)+1;//+1 fence post problem
     lYSz := trunc(lMax.Y-lMin.Y+0.9999)+1;
end;
procedure ApplyMatrix (lInMat: TMatrix2D; lSrc: TImg; var lDest: TImg; lInterpolation: integer; lResize: boolean);
//apply 3x3 transformation matrix to our image
var
   lXo,lYo,lXs,lYs: single;
   lY,lX,lXi,lYi,lYPos: integer;
   lMat: TMatrix2D;
begin
     lMat := lInMat;
     if not lResize then
        CopyImg(lSrc,lDest)//ensure same size
     else begin
        BoundingBox (lMat, lSrc, lXo,lYo, lXi,lYi);
        ResizeImg( lYi,lXi, lDest,(lSrc.PixelFormat32 = 32));
        TranslateMat (lMat,-lXo,-lYo);
     end;
     Invert(lMat);
        lXi := lDest.Wid;
        lYi := lDest.Ht;
     if (lXi < 1) or (lYi < 1) then
        exit;
     if lSrc.PixelFormat32 = 32 then begin //32 bit
        for lY := 1 to lYi do begin
            lyPos := (lY-1)*lXi;
            for lX := 1 to lXi do begin
                lXs := (lX*lMat[1,1])+(lY*lMat[2,1])     +lMat[3,1];
                lYs := (lX*lMat[1,2])+(lY*lMat[2,2])     +lMat[3,2];
                lDest.Img32^[lX+lYPos] := Intensity32(lXs,lYs,LSrc,lInterpolation);//Intensity(lXs,lYs,LSrc,lInterpolation);//round(lImgFrac*gBuff[lX+lYPos]+(lGradFrac*255) );
            end; {lX}
        end; {lY}
     end
    else
      begin //8 bit
        for lY := 1 to lYi do begin
            lyPos := (lY-1)*lXi;
            for lX := 1 to lXi do begin
                lXs := (lX*lMat[1,1])+(lY*lMat[2,1])     +lMat[3,1];
                lYs := (lX*lMat[1,2])+(lY*lMat[2,2])     +lMat[3,2];
                lDest.Img^[lX+lYPos] := Intensity(lXs,lYs,LSrc,lInterpolation);//round(lImgFrac*gBuff[lX+lYPos]+(lGradFrac*255) );
            end; {lX}
        end; {lY}
     end;
end;

function GetVal(Src: string; var Dst: Extended): Boolean;
var
  Code: Integer;
begin
  Val(Src, Dst, Code);
  Result := Code = 0;
end;

function PixPos(Val,Min,Max: single; Pix: integer): integer;
begin
     result := round((Val-Min)/(Max-Min)*Pix);
end;

procedure DrawKernel (lImage: TImage; lFilter: integer);
const
  kHMax = 3;//horizontal
  kHMin = -3;
  kHStep = 1;
  kVMax = 1.25;//vertical range
  kVMin = -0.25;
  kVStep = 1;
var
  P: Single;
  Ym,Vi,Hi,Xi,Yi: integer;
begin
  with lImage.Canvas do begin
    //Kernel := TKernelResampler(Src.Bitmap.Resampler).Kernel;
    Pen.Width := 1;
    Vi := lImage.Height;
    Hi := lImage.Width;
    Brush.Color := clBlack;
    Brush.Style := bsSolid;
    Pen.Color := clBlack;
    Pen.Width := 1;
    Rectangle(0,0,Hi,Vi);
    Pen.Color := clGray;
    //horizontal lines
    P := round(kVMin / kVStep)*kVStep;
    while P <= kVMax do begin
          Yi := Vi-PixPos(P,kVMin,kVMax,Vi);
          MoveTo(1,Yi);
          LineTo(Hi,Yi);
          P := P + kVStep;
    end;
    //horizontal lines
    P := round(kHMin / kHStep)*kHStep;
    while P <= kHMax do begin
          Xi := PixPos(P,kHMin,kHMax,Hi);
          MoveTo(Xi,1);
          LineTo(Xi,Vi);
          P := P + kHStep;
    end;
    //draw function
    Pen.Color := $3400F4;//clRed;
    for Xi := 1 to Hi do begin
        P := ((Xi-1)/Hi*(kHMax-kHMin))+kHMin;
        P := FilterHt(P,lFilter);
        Ym := Vi-PixPos(P,kVMin,kVMax,Vi);

        P := ((Xi+1)/Hi*(kHMax-kHMin))+kHMin;
        P := FilterHt(P,lFilter);
        Yi := Vi-PixPos(P,kVMin,kVMax,Vi);
        MoveTo(Xi-1,Ym+1);
          LineTo(Xi+1,Yi+1);
        MoveTo(Xi-1,Ym);
          LineTo(Xi+1,Yi);
    end;

(*  {$IFDEF Darwin}
    for Xi := 1 to Hi do begin
        P := ((Xi-1)/Hi*(kHMax-kHMin))+kHMin;
        P := FilterHt(P,lFilter);
        Ym := Vi-PixPos(P,kVMin,kVMax,Vi);

        P := ((Xi+1)/Hi*(kHMax-kHMin))+kHMin;
        P := FilterHt(P,lFilter);
        Yi := Vi-PixPos(P,kVMin,kVMax,Vi);
        MoveTo(Xi-1,Ym);
          LineTo(Xi+1,Yi);
    end;
{$ELSE}
    for Xi := 1 to Hi do begin
        P := (Xi/Hi*(kHMax-kHMin))+kHMin;
        P := FilterHt(P,lFilter);
        Yi := Vi-PixPos(P,kVMin,kVMax,Vi);
        if Xi = 1 then
           MoveTo(Xi,Yi)
        else
          LineTo(Xi,Yi);
    end;
{$ENDIF}      *)
  end;
end; //DrawKernel

{$DEFINE DEBUG}
procedure TForm1.FormCreate(Sender: TObject);
begin
  Form1.DoubleBuffered := true;
 CreateImg(gSrc);
 CreateImg(gDest);

 GenerateTestImg( 217,181, gSrc);
 SetDimension(ImageSrc, gSrc,Form1);

 {$IFDEF DEBUG}
 if LoadBMP2Image('/Users/rorden/Documents/pas/smooth/a.png', ImageSrc) then
    CopyInput(ImageSrc, gSrc,false);
 {$ENDIF}
 NewSource;
  ClearTransformations;

  ShowSettings(0);
  EyeMat(gMat);
  ComboBox.ItemIndex := 2;
  ComboBoxChange(nil);
  InterpolationDrop.ItemIndex := kCatRomFilter;
  DrawKernel(ImageKernel,InterpolationDrop.ItemIndex);
  ResizeOutCheckClick(nil); //invokes DoTransform;
end;

procedure TForm1.ClearTransformations;
var
  I: Integer;
begin
  FillChar(Operation[0], SizeOf(TOpRecs), 0);
  for I := 0 to 7 do
  begin
    Operation[I].Sx := 1;
    Operation[I].Sy := 1;
    Operation[I].Cx := ImageSrc.Picture.Bitmap.Width / 2;
    Operation[I].Cy := ImageSrc.Picture.Bitmap.Height / 2;
  end;
end;

procedure TForm1.DoTransform;
const
{$IFDEF UNIX} kCR=chr(10);{$ELSE}kCR = chr (13);{$ENDIF}
var
  S: string;
  i, j: Integer;
begin
  Application.ProcessMessages;
  GenTransform;
  CreateImg(gDest);
  ApplyMatrix (gMat,gSrc,gDest,InterpolationDrop.itemindex,ResizeOutCheck.checked);
  SetDimension(ImageDest, gDest,Form1);
  S := '';
    for j := 1 to kMatSz do begin
      for i := 1 to kMatSz do
        S  := S+' '+Format('%10.3f', [gMat[i, j]]);
      if j <> kMatSz then S := S+kCR;
    end;
    MatrixLabel.caption := S;
end;

procedure TForm1.GenTransform;
var
  I: Integer;
  Rec: TOpRec;
  S: string;
begin
    EyeMat(gMat);
    for I := 0 to 7 do begin
      Rec := Operation[I];
      case Rec.OpType of
        opTranslate:  TranslateMat(gMat,Rec.Dx, Rec.Dy);
        opScale:      ScaleMat(gMat,Rec.Sx, Rec.Sy);
        opRotate:     RotateMat(gMat,Rec.Cx, Rec.Cy, Rec.Alpha);
        opShear:       SkewMat(gMat,Rec.Fx, Rec.Fy);
      end;
      case Rec.OpType of
        opTranslate:  s := s + Format('Trans(%.3g, %.3g); ', [Rec.Dx, Rec.Dy]);
        opScale:      s := s + Format('Zoom(%.3g, %.3g); ', [Rec.Sx, Rec.Sy]);
        opRotate:     s := s + Format('Rot(%.3g, %.3g, %3g); ', [Rec.Cx, Rec.Cy, Rec.Alpha]);
        opShear:       s := s + Format('Shear(%.3g, %.3g); ', [Rec.Fx, Rec.Fy]);
      end;
    end;
    if Length(s) = 0 then s := 'Clear;';
    CodeString.Text := s;
end;

procedure TForm1.ClearBtnClick(Sender: TObject);
begin
  ClearTransformations;
  ShowSettings(Listbox.ItemIndex);
  DoTransform;
end;

procedure TForm1.ListBoxClick(Sender: TObject);
begin
  //Caption := inttostr(ListBox.ItemIndex);
  ShowSettings(ListBox.ItemIndex);
end;

procedure TForm1.ShowSettings(OperationNum: Integer);
begin
  gLoadingValues := True;
  ListBox.ItemIndex := OperationNum;
   // Current := @Operation[OperationNum];
    Current := @Operation[OperationNum];

  Combobox.ItemIndex := Ord(Current.OpType);
  NoteBook.PageIndex := Ord(Current.OpType);
  eDx.Text := Format('%.4g', [Current.Dx]);
  eDy.Text := Format('%.4g', [Current.Dy]);

  sbDx.Position := Round(Current.Dx * 10);
  sbDy.Position := Round(Current.Dy * 10);
  eSx.Text := Format('%.4g', [Current.Sx]);
  eSy.Text := Format('%.4g', [Current.Sy]);
  sbSx.Position := Round(Current.Sx * 100);
  sbSy.Position := Round(Current.Sy * 100);
  eCx.Text := Format('%.4g', [Current.Cx]);
  eCy.Text := Format('%.4g', [Current.Cy]);
  eAlpha.Text := Format('%.4g', [Current.Alpha]);
  sbAlpha.Position := Round(Current.Alpha * 2);
  eFx.Text := Format('%.4g', [Current.Fx]);
  eFy.Text := Format('%.4g', [Current.Fy]);
  sbFx.Position := Round(Current.Fx * 100);
  sbFy.Position := Round(Current.Fy * 100);
  gLoadingValues := False;
end;

procedure TForm1.ComboBoxChange(Sender: TObject);
begin
  Current.OpType := OpTypes[ComboBox.ItemIndex];
  ShowSettings(ListBox.ItemIndex);
  DoTransform;
end;

procedure TForm1.TranslationChanged(Sender: TObject);
var
  Tx, Ty: Extended;
begin
  if gLoadingValues then Exit;
  if GetVal(eDx.Text, Tx) and GetVal(eDy.Text, Ty) then
  begin
    Current.Dx := Tx;
    Current.Dy := Ty;
    DoTransform;
    gLoadingValues := True;
    sbDx.Position := Round(Current.Dx * 10);
    sbDy.Position := Round(Current.Dy * 10);
    gLoadingValues := False;
  end;
end;

procedure TForm1.TranslationScrolled(Sender: TObject);
begin
  if gLoadingValues then Exit;
  Current.Dx := sbDx.Position / 10;
  Current.Dy := sbDy.Position / 10;
  DoTransform;
  gLoadingValues := True;
  eDx.Text := FloatToStr(Current.Dx);
  eDy.Text := FloatToStr(Current.Dy);
  gLoadingValues := False;
end;

procedure TForm1.ScaleChanged(Sender: TObject);
var
  Sx, Sy: Extended;
begin
  if gLoadingValues then Exit;
  if GetVal(eSx.Text, Sx) and GetVal(eSy.Text, Sy) then
  begin
    Current.Sx := Sx;
    Current.Sy := Sy;
    DoTransform;
    gLoadingValues := True;
    sbSx.Position := Round(Current.Sx * 100);
    sbSy.Position := Round(Current.Sy * 100);
    gLoadingValues := False;
  end;
end;

procedure TForm1.ScaleScrolled(Sender: TObject);
begin
  if gLoadingValues then Exit;
  Current.Sx := sbSx.Position / 100;
  Current.Sy := sbSy.Position / 100;
  DoTransform;
  gLoadingValues := True;
  eSx.Text := FloatToStr(Current.Sx);
  eSy.Text := FloatToStr(Current.Sy);
  gLoadingValues := False;
end;

procedure TForm1.RotationChanged(Sender: TObject);
var
  Cx, Cy, Alpha: Extended;
begin
  if gLoadingValues then Exit;
  if GetVal(eCx.Text, Cx) and GetVal(eCy.Text, Cy) and
    GetVal(eAlpha.Text, Alpha) then
  begin
    Current.Cx := Cx;
    Current.Cy := Cy;
    Current.Alpha := Alpha;
    DoTransform;
    gLoadingValues := True;
    sbAlpha.Position := Round(Alpha * 2);
    gLoadingValues := False;
  end;
end;

procedure TForm1.RotationScrolled(Sender: TObject);
begin
  if gLoadingValues then Exit;
  Current.Alpha := sbAlpha.Position;
  DoTransform;
  gLoadingValues := True;
  eAlpha.Text := FloatToStr(Current.Alpha);
  gLoadingValues := False;
end;

procedure TForm1.ShearChanged(Sender: TObject);
var
  Fx, Fy: Extended;
begin
  if gLoadingValues then Exit;
  if GetVal(eFx.Text, Fx) and GetVal(eFy.Text, Fy) then begin
    Current.Fx := Fx;
    Current.Fy := Fy;
    DoTransform;
    gLoadingValues := True;
    sbFx.Position := Round(Current.Fx * 10);
    sbFy.Position := Round(Current.Fy * 10);
    gLoadingValues := False;
  end;
end;

procedure TForm1.ShearScrolled(Sender: TObject);
begin
  if gLoadingValues then Exit;
  Current.Fx := sbFx.Position / 10;
  Current.Fy := sbFy.Position / 10;
  DoTransform;
  gLoadingValues := True;
  eFx.Text := FloatToStr(Current.Fx);
  eFy.Text := FloatToStr(Current.Fy);
  gLoadingValues := False;
end;

procedure TForm1.NewSource;
begin
    ImageDest.Left := ImageSrc.Left + gSrc.Wid+3;
end;

procedure TForm1.Opensource1Click(Sender: TObject);
begin
  if not OpenDialog1.execute then exit;
  LoadBMP2Image(OpenDialog1.Filename, ImageSrc);
  CopyInput(ImageSrc, gSrc,false);
  gSrc.PixelFormat32 := 32;
  SetDimension(ImageSrc, gSrc,Form1);
  NewSource;
  ClearTransformations;
  ShowSettings(0);
  ComboBoxChange(nil);
end;

procedure TForm1.Saveoutput1Click(Sender: TObject);
begin
  if not SaveDialog1.execute then exit;
  {$IFDEF FPC}
  BMPIMage.Picture.Bitmap.Width:=ImageDest.Picture.Bitmap.Width;
  BMPIMage.Picture.Bitmap.Height:=ImageDest.Picture.Bitmap.Height;
  BMPImage.Canvas.Rectangle(0,0,ImageDest.Picture.Bitmap.Width,ImageDest.Picture.Bitmap.Height);
    BMPImage.Canvas.Draw(0,0,ImageDest.Picture.Graphic);
    Save2BMP(SaveDialog1.Filename, BMPImage);
  {$ELSE}
  Save2BMP(SaveDialog1.Filename, ImageDest);
  {$ENDIF}
end;

procedure TForm1.Version1Click(Sender: TObject);
begin
 showmessage('Demonstration of 2D affine transforms and filters. Adapted by Chris Rorden from a graphics32.org demo. Provided under Mozilla Public License.');
end;

procedure TForm1.InterpolationDropChange(Sender: TObject);

begin
  DoTransform;
  DrawKernel(ImageKernel,InterpolationDrop.ItemIndex);
end;

procedure TForm1.ResizeOutCheckClick(Sender: TObject);
begin
     ResizeWarnLabel1.visible :=ResizeOutCheck.checked;
     ResizeWarnLabel2.visible :=ResizeOutCheck.checked;
     DoTransform;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
     FreeImg(gSrc);
     FreeImg(gDest);
end;

procedure TForm1.BenchClick(Sender: TObject);
var
   i: integer;
   Start: DWord;
begin
     Start := GetTickCount;
     for i := 1 to 10 do
         DoTransform;
     Caption := inttostr(GetTickCount-Start)+'ms';
end;

initialization
  {$IFDEF FPC}
  {$i MatrixForm.lrs}
{$ENDIF}

end.
