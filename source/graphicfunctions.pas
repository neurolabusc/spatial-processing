unit graphicfunctions;
{$ifdef fpc}{$mode objfpc}{$endif}

interface

uses
  {$ifndef UNIX}Windows,{$ENDIF}
  ExtCtrls, math,

  {$IFNDEF UNIX}Windows,{$ENDIF}Classes, SysUtils, imgfunc, dialogs, img2image, Graphics;
procedure ApplyMedian (lSrc: TImg; var lDest: TImg);
procedure ApplyNoise (Amount: single; lSrc: TImg; var lDest: TImg);
procedure ApplyFishEye (Amount: single; lSrc: TImg; var lDest: TImg);
procedure ApplyPerspective (Amount: single; lSrc: TImg; var lDest: TImg);
procedure ApplyGradient (Amount: single; lSrc: TImg; var lDest: TImg);
procedure SmoothInput (lFWHM: integer; lSrc: TImg; var lDest: TImg);
procedure Calc2Img (lFunction: integer; lSrc1,lSrc2: TImg; var lDest: TImg);
function MakeMeanImg (lFilenames: TStrings; var lDest: TImg; var Bmp: TImage): boolean;
procedure ApplyOtsuImg (lSrc: TImg; var lDest: TImg; Levels: integer);
procedure ApplyBayer (lSrc: TImg; var lDest: TImg);
procedure RotateDeg (Amount: single; lSrc,lDest: TImg; lSmoothFunc: integer);

implementation

uses Otsuml, fsmooth;

function FishEye (lPos,lBiasIn,lGainIn: single): single;
//lPos is distance from center 0..1 output is factor of scaling that should be applied to distance  [e.g. result of 1 = no change, 2= strecth to twice input]
//bias 1..100 - sets slope
//gain 1..100 - sets intercept
{http://dept-info.labri.fr/~schlick/DOC/gem2.html
http://dept-info.labri.fr/~schlick/publi.html
Fast Alternatives to Perlin's Bias and Gain Functions
Christophe Schlick Graphics Gems IV, p379-382, April 1994  }
var
	lA,lT,lBias,lG,lGain: single;
begin
     result := 1;
     if (lPos <= 0) or (abs(lPos) >= 1) then
        exit;
     lA := (lBiasIn);
     if lA <= 0 then
        lA := 0.000001;
     if lA >= 1 then
        lA := 0.99999;
     lG := lGainIn;
     if lG <= 0 then
        lG := 0.00001;
     if lG >= 1 then
        lG := 0.99999;
     lT := abs(lPos);
     //apply bias
     lBias := (lt/((1/la-2)*(1-lt)+1)) ;
     //next apply gain
     if lT < 0.5 then
        lGain := (lT/((1/lG-2)*(1-2*lT)+1))
     else
         lGain := (( (1/lG-2)*(1-2*lT)-lT ) / ( (1/lG-2)*(1-2*lT)-1 ) );
     lGain := lGain / lT;
     Result := (lBias*lGain)/lT;
end;

procedure ApplyFishEye (Amount: single; lSrc: TImg; var lDest: TImg);
var
   outpix, ty, tx : integer;
   Amt,bias,lensradius,r1, xmid,ymid,dx, dy: Single;
begin
  Amt := Amount / 100;
  CopyImg(lSrc,lDest);//ensure same size
  xmid := lSrc.Wid/2;
  ymid := lSrc.Ht/2;
  if xmid < ymid then
     lensradius := xmid
  else
      lensradius := ymid;
  if lensradius <= 0 then
     exit;
  outpix := 0;
  for ty := 0 to lSrc.Ht - 1 do begin
	for tx := 0 to lSrc.Wid - 1 do begin
	  dx := tx - xmid;
	  dy := ty - ymid;
          r1 := Sqrt(dx * dx + dy * dy);
          bias := FishEye (r1/lensradius,Amt,0.5);
          dx := xmid+(dx*bias);
          dy := ymid+(dy*bias);
          inc(outpix);
          if lSrc.PixelFormat32 = 32 then
              lDest.Img32^[outpix] :=  Intensity32 (dx,dy, lSrc,kLanczosFilter)// kMitchellFilter)
          else
              lDest.Img^[outpix] :=  Intensity (dx,dy, lSrc, kLanczosFilter);//kMitchellFilter);
              //choice of filter makes a big difference
	end;
  end;
end;

(*procedure ApplyFishEye (Amount: single; lSrc: TImg; var lDest: TImg);
begin
     if lSrc.PixelFormat32 = 32 then
        ApplyFishEye32(Amount,lSrc,lDest)
     else
        ApplyFishEye8(Amount,lSrc,lDest)
end; *)

function NormalRand : single;
//returns random number from normal distribution...
//mean is zero, SD is 1 [I think]
var
   x1, x2, w: single;
begin
     repeat
           x1 := 2.0 * random - 1.0;
           x2 := 2.0 * random - 1.0;
           w := x1 * x1 + x2 * x2;
    until ( w {>=}<  1.0 );
    w := sqrt( (-2.0 * ln( w ) ) / w );
    result := x1 * w;
end;

function AddNoise(lInput: byte; lSD: integer): byte;
var
   s: single;
begin
     s := lInput+ (NormalRand * lSD);
     if s > 255 then
        result := 255
     else if s < 0 then
        result := 0
     else
         result := round(s);
end;

procedure ApplyNoise32 (Amount: single; lSrc: TImg; var lDest: TImg);
//each voxel gets gaussian noise... amplitude of noise is variable...
var
   lSD,lY,lX,lXi,lYi,lYPos: integer;
begin
        CopyImg(lSrc,lDest);//ensure same size
        lSD := round(Amount);
        lXi := lSrc.Wid;
        lYi := lSrc.Ht;
        for lY := 1 to lYi do begin
            lyPos := (lY-1)*lXi;
            for lX := 1 to lXi do begin
                lDest.Img32^[lX+lYPos].rgbRed := AddNoise(lSrc.Img32^[lX+lYPos].rgbRed, lSD);
                lDest.Img32^[lX+lYPos].rgbGreen := AddNoise(lSrc.Img32^[lX+lYPos].rgbGreen, lSD);
                lDest.Img32^[lX+lYPos].rgbBlue := AddNoise(lSrc.Img32^[lX+lYPos].rgbBlue, lSD)
            end; {lX}
        end; {lY}
end;

procedure ApplyNoise8 (Amount: single; lSrc: TImg; var lDest: TImg);
//each voxel gets gaussian noise... amplitude of noise is variable...
var
   //s: single;
   lSD,lY,lX,lXi,lYi,lYPos: integer;
begin
        CopyImg(lSrc,lDest);//ensure same size
        lSD := round(Amount);
        lXi := lSrc.Wid;
        lYi := lSrc.Ht;
        for lY := 1 to lYi do begin
            lyPos := (lY-1)*lXi;
            for lX := 1 to lXi do begin
                lDest.Img^[lX+lYPos] := AddNoise(lSrc.Img^[lX+lYPos], lSD)

                (*s := lSrc.Img^[lX+lYPos]+ (NormalRand * lSD);
                if s > 255 then
                   s := 255
                else if s < 0 then
                     s := 0;

                lDest.Img^[lX+lYPos] := round(s);*)
            end; {lX}
        end; {lY}
end;

procedure ApplyNoise (Amount: single; lSrc: TImg; var lDest: TImg);
begin
     if lSrc.PixelFormat32 = 32 then
        ApplyNoise32(Amount,lSrc,lDest)
     else
        ApplyNoise8(Amount,lSrc,lDest)
end;

procedure ApplyBayer32 (lSrc: TImg; var lDest: TImg);
//each voxel gets gaussian noise... amplitude of noise is variable...
var
   lRGB,lY,lX,lXi,lYi,lYPos,lOddY: integer;
begin
        if (lSrc.PixelFormat32 <> 32) then
           exit;
        CopyImg(lSrc,lDest);//ensure same size
        lXi := lSrc.Wid;
        lYi := lSrc.Ht;
        for lY := 1 to lYi do begin
            lyPos := (lY-1)*lXi;
            if odd(lY) then
               lOddY := 0
            else
               lOddY := 1;
            for lX := 1 to lXi do begin
                lDest.Img32^[lX+lYPos].rgbRed :=0;
                lDest.Img32^[lX+lYPos].rgbGreen := 0;
                lDest.Img32^[lX+lYPos].rgbBlue := 0;
                lRGB := (lX mod 2)+lOddY;
                case lRGB of
                     0: lDest.Img32^[lX+lYPos].rgbRed := lSrc.Img32^[lX+lYPos].rgbRed;
                     1: lDest.Img32^[lX+lYPos].rgbGreen := lSrc.Img32^[lX+lYPos].rgbGreen;
                     else  lDest.Img32^[lX+lYPos].rgbBlue := lSrc.Img32^[lX+lYPos].rgbBlue;
                end;
            end; {lX}
        end; {lY}
end;

procedure ApplyBayer (lSrc: TImg; var lDest: TImg);
var
   lTemp: TImg;
begin
     if lSrc.PixelFormat32 = 32 then
        ApplyBayer32(lSrc,lDest)
     else begin
        CreateImg(lTemp);
        CopyImg(lSrc,lTemp);
        ForceRGB(lTemp);
        ApplyBayer32(lTemp,lDest);
        FreeImg(lTemp);
     end;
end;

procedure ApplyMedian32 (lSrc: TImg; var lDest: TImg);
//3x3 pixel median filter...
type
   TObsRA = array [1..9] of integer;

var
   lObsRAr,lObsRAg,lObsRAb: TObsRA;
   lObs,lY,lX,lXi,lYi,lYPos,lR,lC: integer;
function Median9 (lObsRA: TObsRA): integer;
//determines median of 9 observations...
var
   lP,lPx,lRank,lRankEq: integer;
begin
     for lP := 1 to 8 do begin
         result := lObsRA[lP];
         lRank := 0;
         lRankEq  := 0;
         for lPx := 1 to 9 do begin
             if lObsRA[lPx] = result then
                inc(lRankEq);
             if lObsRA[lPx] <= result then
                inc(lRank);
         end;
         if lRank = 5 then //found the median
            exit;
         if (lRank > 5) and ((lRank-lRankEq) < 5) then
            exit;
         //showmessage(inttostr(lRank));
     end;
     //we would have exited if one of first 9 was median...
     result := lObsRA[9];
end; //nested median9
begin
        lXi := lSrc.Wid;
        lYi := lSrc.Ht;
        if (lXi < 3) or (lYi < 3) then
           exit;
        CopyImg(lSrc,lDest);//the median filter does not adjust the edges - so get these directly from the source
        for lY := 2 to (lYi-1) do begin
            lyPos := (lY-1)*lXi;
            for lX := 2 to (lXi-1) do begin
                lObs := 0;
                for lR := -1 to 1 do begin
                    for lC := -1 to 1 do begin
                        inc(lObs);
                        lObsRAr[lObs] := lSrc.Img32^[lX+lYPos+lC+(lR*lXi)].rgbRed;
                        lObsRAg[lObs] := lSrc.Img32^[lX+lYPos+lC+(lR*lXi)].rgbGreen;
                        lObsRAb[lObs] := lSrc.Img32^[lX+lYPos+lC+(lR*lXi)].rgbBlue;
                    end;//each col
                end;//each row
                lDest.Img32^[lX+lYPos].rgbRed := Median9(lObsRAr);
                lDest.Img32^[lX+lYPos].rgbGreen := Median9(lObsRAg);
                lDest.Img32^[lX+lYPos].rgbBlue := Median9(lObsRAb);
            end; {lX}
        end; {lY}
end;

procedure ApplyMedian8 (lSrc: TImg; var lDest: TImg);
//3x3 pixel median filter...
var
   lObsRA: array [1..9] of integer;
   lObs,lY,lX,lXi,lYi,lYPos,lR,lC: integer;
function Median9: integer;
//determines median of 9 observations...
var
   lP,lPx,lRank,lRankEq: integer;
begin
     for lP := 1 to 8 do begin
         result := lObsRA[lP];
         lRank := 0;
         lRankEq  := 0;
         for lPx := 1 to 9 do begin
             if lObsRA[lPx] = result then
                inc(lRankEq);
             if lObsRA[lPx] <= result then
                inc(lRank);
         end;
         if lRank = 5 then //found the median
            exit;
         if (lRank > 5) and ((lRank-lRankEq) < 5) then
            exit;
         //showmessage(inttostr(lRank));
     end;
     //we would have exited if one of first 9 was median...
     result := lObsRA[9];
end; //nested median9
begin
        lXi := lSrc.Wid;
        lYi := lSrc.Ht;
        if (lXi < 3) or (lYi < 3) then
           exit;
        CopyImg(lSrc,lDest);//the median filter does not adjust the edges - so get these directly from the source
        for lY := 2 to (lYi-1) do begin
            lyPos := (lY-1)*lXi;
            for lX := 2 to (lXi-1) do begin
                lObs := 0;
                for lR := -1 to 1 do begin
                    for lC := -1 to 1 do begin
                        inc(lObs);
                        lObsRA[lObs] := lSrc.Img^[lX+lYPos+lC+(lR*lXi)]
                    end;//each col
                end;//each row
                lDest.Img^[lX+lYPos] := Median9;
            end; {lX}
        end; {lY}
end;

procedure ApplyMedian (lSrc: TImg; var lDest: TImg);
begin
     if lSrc.PixelFormat32 = 32 then
        ApplyMedian32(lSrc,lDest)
     else
         ApplyMedian8(lSrc,lDest);
end;

procedure ApplyOtsuImg (lSrc: TImg; var lDest: TImg; Levels: integer);
begin
  CopyImg8(lSrc,lDest);
  lDest.PixelFormat32 := 8;

  ApplyOtsux(lDest.Img,lDest.Wid*lDest.Ht,Levels);
end;

procedure ApplyPerspective32 (Amount: single; lSrc: TImg; var lDest: TImg);
//amount 0..100
var
   lXSrc,lLineSource,lDistance,lZoom,lTopWid,lWid: single;
   lTopPct,lMidX,lY,lX,lXi,lYi,lLineStart: integer;
begin
        lTopPct := 100-round(Amount);
        CopyImg(lSrc,lDest);//ensure same size
        if lTopPct < 1 then
           lTopPct := 1
        else if lTopPct > 100 then
             lTopPct := 100;
        lXi := lSrc.Wid;
        lYi := lSrc.Ht;
        lMidX := lXi div 2;
        lTopWid := (lTopPct/100*lXi);
        lLineSource := 1;
        lDistance := 0;
        for lY := 1 to lYi do begin
            lLineStart := (lY-1)*lXi;
            lWid := lTopWid + ((lXi-lTopWid) * ((lYi-lY+1)/lYi) );
            lZoom := lXi/lWid;
            if lLineSource < lYi then begin
              for lX := 1 to lXi do begin
                lXsrc := (lMidX + round(lX-lMidX)* lZoom);
                if (lXsrc < 1) or (lXsrc > lXi) then
                   lDest.Img32^[lX+lLineStart] := kBackGround32
                else
                    lDest.Img32^[lX+lLineStart] := Intensity32 (lXSrc,lLineSource, lSrc, true);
              end; {lX}
             end else begin
                 for lX := 1 to lXi do
                   lDest.Img32^[lX+lLineStart] := kBackGround32
             end;
            lDistance := lDistance + lZoom;
            lLineSource := round(lDistance);
        end; {lY}
end;

procedure ApplyPerspective8 (Amount: single; lSrc: TImg; var lDest: TImg);
//amount 0..100
var
   lXSrc,lLineSource,lDistance,lZoom,lTopWid,lWid: single;
   lTopPct,lMidX,lY,lX,lXi,lYi,lLineStart: integer;
begin
        lTopPct := 100-round(Amount);
        CopyImg(lSrc,lDest);//ensure same size
        if lTopPct < 1 then
           lTopPct := 1
        else if lTopPct > 100 then
             lTopPct := 100;
        lXi := lSrc.Wid;
        lYi := lSrc.Ht;
        lMidX := lXi div 2;
        lTopWid := (lTopPct/100*lXi);
        lLineSource := 1;
        lDistance := 0;
        for lY := 1 to lYi do begin
            lLineStart := (lY-1)*lXi;
            lWid := lTopWid + ((lXi-lTopWid) * ((lYi-lY+1)/lYi) );
            lZoom := lXi/lWid;
            if lLineSource < lYi then begin
              for lX := 1 to lXi do begin
                lXsrc := (lMidX + round(lX-lMidX)* lZoom);
                if (lXsrc < 1) or (lXsrc > lXi) then
                   lDest.Img^[lX+lLineStart] := kBackground8
                else
                    lDest.Img^[lX+lLineStart] := Intensity (lXSrc,lLineSource, lSrc, true);
              end; {lX}
             end else begin
                 for lX := 1 to lXi do
                   lDest.Img^[lX+lLineStart] := kBackground8
             end;
            lDistance := lDistance + lZoom;
            lLineSource := round(lDistance);
        end; {lY}
end;

procedure ApplyPerspective (Amount: single; lSrc: TImg; var lDest: TImg);
begin
     if lSrc.PixelFormat32 = 32 then
        ApplyPerspective32(Amount,lSrc,lDest)
     else
         ApplyPerspective8(Amount,lSrc,lDest);
end;
function BiasGain (lPos,lBiasIn,lGainIn: single): single;
//lPos is distance from center 0..1 output is factor nonlinear ramp 0..1
//bias 1..100 - sets slope
//gain 1..100 - sets intercept
{http://dept-info.labri.fr/~schlick/DOC/gem2.html
http://dept-info.labri.fr/~schlick/publi.html
Fast Alternatives to Perlin's Bias and Gain Functions
Christophe Schlick Graphics Gems IV, p379-382, April 1994  }
var
	lA,lT,lBias,lG,lGain: single;
begin
     result := 1;
     if (lPos <= 0) or (abs(lPos) >= 1) then
        exit;
     lA := (lBiasIn);
     if lA <= 0 then
        lA := 0.000001;
     if lA >= 1 then
        lA := 0.99999;
     lG := lGainIn;
     if lG <= 0 then
        lG := 0.00001;
     if lG >= 1 then
        lG := 0.99999;
     lT := abs(lPos);
     //apply bias
     lBias := (lt/((1/la-2)*(1-lt)+1)) ;
     //next apply gain
     if lT < 0.5 then
        lGain := (lT/((1/lG-2)*(1-2*lT)+1))
     else
         lGain := (( (1/lG-2)*(1-2*lT)-lT ) / ( (1/lG-2)*(1-2*lT)-1 ) );
     lGain := lGain / lT;
     Result := (lBias*lGain);
end;

procedure ApplyGradient32 (Amount: single; lSrc: TImg; var lDest: TImg);
var
   bias: single;
   lY,lX,lXi,lYi,lYPos: integer;
begin
        lXi := lSrc.Wid;
        lYi := lSrc.Ht;
        if (lXi < 1) or (lYi < 1) then
           exit;
        CopyImg(lSrc,lDest);//ensure same size
        for lY := 1 to lYi do begin
             bias := BiasGain (lY/lYi,Amount/100,0.5);
            lyPos := (lY-1)*lXi;
            for lX := 1 to lXi do begin
                lDest.Img32^[lX+lYPos].rgbRed := round(bias*lSrc.Img32^[lX+lYPos].rgbRed);
                lDest.Img32^[lX+lYPos].rgbGreen := round(bias*lSrc.Img32^[lX+lYPos].rgbGreen);
                lDest.Img32^[lX+lYPos].rgbBlue := round(bias*lSrc.Img32^[lX+lYPos].rgbBlue);
            end;
        end;
end;

procedure ApplyGradient8 (Amount: single; lSrc: TImg; var lDest: TImg);
var
   bias: single;
   lY,lX,lXi,lYi,lYPos: integer;
begin
        lXi := lSrc.Wid;
        lYi := lSrc.Ht;
        if (lXi < 1) or (lYi < 1) then
           exit;
        CopyImg(lSrc,lDest);//ensure same size
        for lY := 1 to lYi do begin
             bias := BiasGain (lY/lYi,Amount/100,0.5);
            lyPos := (lY-1)*lXi;
            for lX := 1 to lXi do
                lDest.Img^[lX+lYPos] := round(bias*lSrc.Img^[lX+lYPos]);
        end;
end;

procedure ApplyGradient (Amount: single; lSrc: TImg; var lDest: TImg);
begin
     if lSrc.PixelFormat32 = 32 then
        ApplyGradient32(Amount,lSrc,lDest)
     else
        ApplyGradient8(Amount,lSrc,lDest)
end;
procedure SmoothInput32 (lFWHM: integer; lSrc: TImg; var lDest: TImg);
var
   lsigma,lexpd,lcumgaussr,lCumGaussG,lCumGaussB: single;
   lcutoffvoxx,lI,lY,lX,lXi,lYi,lMin,lMax,lPos,lYPos: integer;
   l32: TRGBQuad;
   lxra: singlep0;
   lTempBuffR,lTempBuffG,lTempBuffB: Singlep;
begin
        CopyImg(lSrc,lDest);//Set same file size
        lXi := lSrc.Wid;
        lYi := lSrc.Ht;
        lsigma  := (lFWHM)/sqrt(8*ln(2));  //      % FWHM -> sigma
        lcutoffvoxx  := round(6*lsigma);       //    % highest / lowest voxel to go out to
           getmem(lxra,(lcutoffvoxx+1)*sizeof(single));
        lexpd := 2*lsigma*lsigma;
        lCumGaussR := 0;
        for lI := 0 to lcutoffvoxx do begin
            lxra^[lI] := exp(-1*(lI*lI)/lexpd) ;
            lCumGaussR := lCumGaussR + lxra^[lI];
        end;
        lCumGaussR := 2*lCumGaussR - lxra^[0];
        if lCumGaussR <> 0 then
           for lI := 0 to lcutoffvoxx do begin
            lxra^[lI] := lxra^[lI]/lCumGaussR;
           end;
        getmem(lTempBuffR,lXi*lYi*sizeof(single));
        getmem(lTempBuffG,lXi*lYi*sizeof(single));
        getmem(lTempBuffB,lXi*lYi*sizeof(single));
        //smooth horizontally
        for lY := 1 to lYi do begin
            lyPos := (lY-1)*lXi;
            for lX := 1 to lXi do begin
                lMin := lX - lCutoffVoxX;
                if lMin < 1 then lMin := 1;
                lMax := lX + lCutoffVoxX;
                if lMax > lXi then lMax := lXi;
                lCumGaussR := 0;
                lCumGaussG := 0;
                lCumGaussB := 0;
                for lPos := lMin to lMax do begin
                    l32 := lSrc.Img32^[lPos+lYPos];
                    lCumGaussR := lCumGaussR + l32.rgbRed*lXra^[abs(lX-lPos)] ;
                    lCumGaussG := lCumGaussG + l32.rgbGreen*lXra^[abs(lX-lPos)] ;
                    lCumGaussB := lCumGaussB + l32.rgbBlue*lXra^[abs(lX-lPos)] ;
                end;
                lTempBuffR^[lX+lYPos] := (lCumGaussR);
                lTempBuffG^[lX+lYPos] := (lCumGaussG);
                lTempBuffB^[lX+lYPos] := (lCumGaussB);
            end; {lX}
        end; {lY}
        //smooth vertically
        for lX := 1 to lXi do begin
            for lY := 1 to lYi do begin
                lMin := lY - lCutoffVoxX;
                if lMin < 1 then lMin := 1;
                lMax := lY + lCutoffVoxX;
                if lMax > lYi then lMax := lYi;
                lCumGaussR := 0;
                lCumGaussG := 0;
                lCumGaussB := 0;
                for lPos := lMin to lMax do begin
                    lyPos := ((lPos-1)*lXi)+lX;
                    lCumGaussR := lCumGaussR + lTempBuffR^[lYPos]*lXra^[abs(lY-lPos)] ;
                    lCumGaussG := lCumGaussG + lTempBuffG^[lYPos]*lXra^[abs(lY-lPos)] ;
                    lCumGaussB := lCumGaussB + lTempBuffB^[lYPos]*lXra^[abs(lY-lPos)] ;
                end;
                lyPos := (lY-1)*lXi;
                lDest.Img32^[lX+lYPos].rgbRed := round(lCumGaussR);
                lDest.Img32^[lX+lYPos].rgbGreen := round(lCumGaussG);
                lDest.Img32^[lX+lYPos].rgbBlue := round(lCumGaussB);
            end; {lX}
        end; {lY}
        //free memory
           freemem(lTempBuffR);
           freemem(lTempBuffG);
           freemem(lTempBuffB);
           freemem(lxra);
end; //SmoothInput

procedure SmoothInput8 (lFWHM: integer; lSrc: TImg; var lDest: TImg);
var
   lsigma,lexpd,lcumgauss: single;
   lcutoffvoxx,lI,lY,lX,lXi,lYi,lMin,lMax,lPos,lYPos: integer;
   lxra: singlep0;
   lTempBuff: Singlep;
begin
        CopyImg(lSrc,lDest);//Set same file size
        lXi := lSrc.Wid;
        lYi := lSrc.Ht;
        lsigma  := (lFWHM)/sqrt(8*ln(2));  //      % FWHM -> sigma
        lcutoffvoxx  := round(6*lsigma);       //    % highest / lowest voxel to go out to
           getmem(lxra,(lcutoffvoxx+1)*sizeof(single));
           getmem(lTempBuff,lXi*lYi*sizeof(single));
        lexpd := 2*lsigma*lsigma;
        lCumGauss := 0;
        for lI := 0 to lcutoffvoxx do begin
            lxra^[lI] := exp(-1*(lI*lI)/lexpd) ;
            lCumGauss := lCumGauss + lxra^[lI];
        end;
        lCumGauss := 2*lCumGauss - lxra^[0];
        if lCumGauss <> 0 then
           for lI := 0 to lcutoffvoxx do begin
            lxra^[lI] := lxra^[lI]/lCumGauss;
           end;
        //smooth horizontally
        for lY := 1 to lYi do begin
            lyPos := (lY-1)*lXi;
            for lX := 1 to lXi do begin
                lMin := lX - lCutoffVoxX;
                if lMin < 1 then lMin := 1;
                lMax := lX + lCutoffVoxX;
                if lMax > lXi then lMax := lXi;
                lCumGauss := 0;
                for lPos := lMin to lMax do
                    lCumGauss := lCumGauss + lSrc.Img^[lPos+lYPos]*lXra^[abs(lX-lPos)] ;
                lTempBuff^[lX+lYPos] := (lCumGauss);
            end; {lX}
        end; {lY}
        //smooth vertically
        for lX := 1 to lXi do begin
            for lY := 1 to lYi do begin
                lMin := lY - lCutoffVoxX;
                if lMin < 1 then lMin := 1;
                lMax := lY + lCutoffVoxX;
                if lMax > lYi then lMax := lYi;
                lCumGauss := 0;
                for lPos := lMin to lMax do begin
                    lyPos := ((lPos-1)*lXi)+lX;
                    lCumGauss := lCumGauss + lTempBuff^[lYPos]*lXra^[abs(lY-lPos)] ;
                end;
                lyPos := (lY-1)*lXi;
                lDest.Img^[lX+lYPos] := round(lCumGauss);
            end; {lX}
        end; {lY}
        //free memory
           freemem(lTempBuff);
           freemem(lxra);
end; //SmoothInput

procedure SmoothInput8F (lFWHM: integer; lSrc: TImg; var lDest: TImg);
begin
     CopyImg(lSrc,lDest);//Set same file size
     FastSmooth(lDest.Img,lDest.Wid, lDest.Ht,1);
end;

procedure SmoothInput8Fx (lFWHM: integer; lSrc: TImg; var lDest: TImg);
const
  k0=240;//weight of center voxel
  k1=120;//weight of nearest neighbors
  k2=15;//weight of subsequent neighbors
  kTot=k0+k1+k1+k2+k2; //weight of center plus all neighbors within 2 voxels
  kWid = 2; //we will look +/- 2 voxels from center
var
   lWSum,
   lI,lY,lX,lXi,lYi,lMin,lMax,lPos,lYPos: integer;
   lTemp: ByteP;
begin
     lXi := lSrc.Wid;
     lYi := lSrc.Ht;
        CopyImg(lSrc,lDest);//Set same file size

        getmem(lTemp,lXi*lYi*sizeof(byte));
        for lPos := 1 to (LXi*lYi) do
            lTemp^[lPos] := lSrc.Img^[lPos];
        //smooth horizontally
        for lY := (1+kWid) to (lYi-kWid) do begin
            lyPos := (lY-1)*lXi;
            for lX := (1+kWid) to (lXi-kWid) do begin
                lPos := lyPos + lX;
                lWSum := lSrc.Img^[lPos-2]*k2+lSrc.Img^[lPos-1]*k1
                      +lSrc.Img^[lPos-2]*k0
                      +lSrc.Img^[lPos+1]*k1+lSrc.Img^[lPos+2]*k2;
                lTemp^[lPos] := lWSum div kTot;
            end; {lX}
        end; {lY}
       (* //smooth vertically
        for lX := 1 to lXi do begin
            for lY := 1 to lYi do begin
                lMin := lY - lCutoffVoxX;
                if lMin < 1 then lMin := 1;
                lMax := lY + lCutoffVoxX;
                if lMax > lYi then lMax := lYi;
                lCumGauss := 0;
                for lPos := lMin to lMax do begin
                    lyPos := ((lPos-1)*lXi)+lX;
                    lCumGauss := lCumGauss + lTempBuff^[lYPos]*lXra^[abs(lY-lPos)] ;
                end;
                lyPos := (lY-1)*lXi;
                lDest.Img^[lX+lYPos] := round(lCumGauss);
            end; {lX}
        end; {lY}
        //free memory   *)
        for lPos := 1 to (LXi*lYi) do
            lDest.Img^[lPos] := lTemp^[lPos];
           freemem(lTemp);
end; //SmoothInput8F

procedure SmoothInput (lFWHM: integer; lSrc: TImg; var lDest: TImg);
begin
     if lSrc.PixelFormat32 = 32 then
        SmoothInput32(lFWHM,lSrc,lDest)
     else
        SmoothInput8F(lFWHM,lSrc,lDest);
end;

procedure RescaleToByte (lFBuff: singlep; lN: integer);
//scales values 0..255
var
   lMin,lMax,lScale: single;
   lPos: integer;
begin
     if lN < 1 then
        exit;
     lMin := lFBuff^[1];
     lMax := lMin;
     for lPos := 1 to lN do begin
         if lFBuff^[lPos]> lMax then
            lMax := lFBuff^[lPos];
         if lFBuff^[lPos]< lMin then
            lMin := lFBuff^[lPos];
     end;
     if lMax = lMin then
        lScale := 1
     else
         lScale := 255/(lMax-lMin);
     for lPos := 1 to lN do
         lFBuff^[lPos] := ((lFBuff^[lPos]-lMin)*lScale);
end;

procedure Calc2Img32 (lFunction: integer; lSrc1,lSrc2: TImg; var lDest: TImg);
//function 0=Mean,1=Difference,2=AbsDifference,3=SqrDifference
var
   lPos,lPix: integer;
   lFBuffR,lFBuffG,lFBuffB: singlep;
begin
     if (lSrc1.Ht<> lSrc2.Ht) or (lSrc1.Wid<> lSrc2.Wid) then begin
        showmessage('Error: source and 2nd image do not have same dimensions.');
        exit;
     end;
     lPix := lSrc1.Wid*lSrc1.Ht;
     if lPix < 1 then
        exit;
     getmem(lFBuffR, lPix*sizeof(single));//FloatBuffer
     getmem(lFBuffG, lPix*sizeof(single));//FloatBuffer
     getmem(lFBuffB, lPix*sizeof(single));//FloatBuffer
     if lFunction = 0 then begin  //calc mean
        for lPos := 1 to lPix do begin
             lFBuffr^[lPos] := (lSrc1.Img32^[lPos].rgbred+lSrc2.Img32^[lPos].rgbred) div 2;
             lFBuffg^[lPos] := (lSrc1.Img32^[lPos].rgbgreen+lSrc2.Img32^[lPos].rgbgreen) div 2;
             lFBuffb^[lPos] := (lSrc1.Img32^[lPos].rgbblue+lSrc2.Img32^[lPos].rgbblue) div 2;

        end;
     end else begin//calc diff
         for lPos := 1 to lPix do begin
             lFBuffr^[lPos] := (lSrc1.Img32^[lPos].rgbRed-lSrc2.Img32^[lPos].rgbRed) div 2;
             lFBuffg^[lPos] := (lSrc1.Img32^[lPos].rgbGreen-lSrc2.Img32^[lPos].rgbGreen) div 2;
             lFBuffb^[lPos] := (lSrc1.Img32^[lPos].rgbBlue-lSrc2.Img32^[lPos].rgbBlue) div 2;
         end;
     end;
     //variance...
     if lFunction = 3 then //2=AbsDifference,3=SqrDifference
        for lPos := 1 to lPix do begin
            lFBuffr^[lPos] :=  lFBuffr^[lPos]*lFBuffr^[lPos];
            lFBuffg^[lPos] :=  lFBuffg^[lPos]*lFBuffg^[lPos];
            lFBuffb^[lPos] :=  lFBuffb^[lPos]*lFBuffb^[lPos];
        end;
     if lFunction = 2 then //2=AbsDifference,3=SqrDifference
        for lPos := 1 to lPix do begin
         lFBuffr^[lPos] :=  abs(lFBuffr^[lPos]);
         lFBuffg^[lPos] :=  abs(lFBuffg^[lPos]);
         lFBuffb^[lPos] :=  abs(lFBuffb^[lPos]);
        end;
     lDest.PixelFormat32 := 32;
     ResizeImg(lSrc1.Ht,lSrc1.Wid,lDest,true);
     RescaleToByte (lFBuffr, lPix);
     RescaleToByte (lFBuffg, lPix);
     RescaleToByte (lFBuffb, lPix);
     for lPos := 1 to lPix do begin
         lDest.Img32^[lPos].rgbRed := round(lFBuffr^[lPos]);
         lDest.Img32^[lPos].rgbGreen := round(lFBuffg^[lPos]);
         lDest.Img32^[lPos].rgbBlue := round(lFBuffb^[lPos]);
     end;
     freemem(lFBuffr);
     freemem(lFBuffg);
     freemem(lFBuffb);
end;

procedure Calc2Img8 (lFunction: integer; lSrc1,lSrc2: TImg; var lDest: TImg);
//function 0=Mean,1=Difference,2=AbsDifference,3=SqrDifference
var
   lPos,lPix: integer;
   lFBuff: singlep;
begin
     if (lSrc1.Ht<> lSrc2.Ht) or (lSrc1.Wid<> lSrc2.Wid) then begin
        showmessage('Error: source and 2nd image do not have same dimensions.');
        exit;
     end;

     lPix := lSrc1.Wid*lSrc1.Ht;
     if lPix < 1 then
        exit;
     getmem(lFBuff, lPix*sizeof(single));//FloatBuffer
     if lFunction = 0 then  //calc mean
        for lPos := 1 to lPix do
             lFBuff^[lPos] := (lSrc1.Img^[lPos]+lSrc2.Img^[lPos]) div 2
     else //calc diff
         for lPos := 1 to lPix do
             lFBuff^[lPos] := (lSrc1.Img^[lPos]-lSrc2.Img^[lPos]) div 2;
     //variance...
     if lFunction = 3 then //2=AbsDifference,3=SqrDifference
        for lPos := 1 to lPix do
            lFBuff^[lPos] :=  lFBuff^[lPos]*lFBuff^[lPos];
     if lFunction = 2 then //2=AbsDifference,3=SqrDifference
        for lPos := 1 to lPix do
         lFBuff^[lPos] :=  abs(lFBuff^[lPos]);
     (*//next compute min and max...
     lMin := lFBuff^[1];
     lMax := lMin;
     for lPos := 1 to lPix do begin
         if lFBuff^[lPos]> lMax then
            lMax := lFBuff^[lPos];
         if lFBuff^[lPos]< lMin then
            lMin := lFBuff^[lPos];
     end;
     if lMax = lMin then
        lScale := 1
     else
         lScale := 255/(lMax-lMin);*)
      RescaleToByte (lFBuff, lPix);
     ResizeImg(lSrc1.Ht,lSrc1.Wid,lDest,false);
     for lPos := 1 to lPix do
         lDest.Img^[lPos] := round(lFBuff^[lPos]);
     //    lDest.Img^[lPos] := round((lFBuff^[lPos]-lMin)*lScale);
     freemem(lFBuff);
end;


procedure Calc2Img (lFunction: integer; lSrc1,lSrc2: TImg; var lDest: TImg);
begin
     if (lSrc1.PixelFormat32 = 32) and (lSrc2.PixelFormat32 = 32) then
        Calc2Img32 (lFunction,lSrc1,lSrc2,lDest)
     else if (lSrc1.PixelFormat32 = 8) and (lSrc2.PixelFormat32 = 8) then
        Calc2Img8 (lFunction,lSrc1,lSrc2,lDest)
     else
         showmessage('Both images must be of the same type (either grayscale or rgb).');

end;

procedure AddImg2SingleArray (lImg: TImg; lMeanRA : Singlep);
var
   lnPix,lPix,lOffset: integer;
begin
     lnPix := lImg.Wid * lImg.Ht;
     if (lImg.PixelFormat32 = 32) then begin
        for lPix := 1 to lnPix do
            lMeanRA^[lPix] := lMeanRA^[lPix]+lImg.Img32^[lPix].rgbRed;
        lOffset :=  lnPix;
        for lPix := 1 to lnPix do
            lMeanRA^[lPix+lOffset] := lMeanRA^[lPix+lOffset]+lImg.Img32^[lPix].rgbGreen;
        lOffset :=  lnPix*2;
        for lPix := 1 to lnPix do
            lMeanRA^[lPix+lOffset] := lMeanRA^[lPix+lOffset]+lImg.Img32^[lPix].rgbBlue;
        lOffset :=  lnPix*3;
        for lPix := 1 to lnPix do
            lMeanRA^[lPix+lOffset] := lMeanRA^[lPix+lOffset]+lImg.Img32^[lPix].rgbreserved;
     end else
         for lPix := 1 to lnPix do
             lMeanRA^[lPix] := lMeanRA^[lPix]+lImg.Img^[lPix];
end;

function MakeMeanImg (lFilenames: TStrings; var lDest: TImg; var Bmp: TImage): boolean;
label
  666;
var
   lTemp: TImg;
   lI, lBytes, lnPix,lOffset: integer;
   lMeanRA : Singlep;
begin
     result := false;
     if lFilenames.Count < 2 then begin
        Showmessage('Error: You need to select at least 2 images to generate a mean');
        exit;
     end;


     if not  LoadBMP2Image(lFilenames[0],Bmp) then
        exit;//BMPImage.Picture.LoadFromFile(OpenDialog1.filename);
     CopyInput(Bmp, lDest,false);

     lnPix := lDest.Wid * lDest.Ht;
     lBytes := lnPix;
     if (lDest.PixelFormat32 = 32) then
        lBytes := lBytes * 4;
     if (lBytes < 0) then
        exit;
     GetMem(lMeanRA,lBytes* sizeof(single));
     for lI := 1 to lBytes do
            lMeanRA^[lI] := 0;
     AddImg2SingleArray(lDest,lMeanRA);

     CreateImg(lTemp);
     for lI := 2 to lFilenames.Count do begin
         if not  LoadBMP2Image(lFilenames[lI-1],Bmp) then goto 666;
         CopyInput(Bmp, lTemp,false);
         if (lTemp.Wid <> lDest.Wid) or  (lTemp.Ht <> lDest.Ht) or (lTemp.PixelFormat32 <> lDest.PixelFormat32) then begin
            Showmessage('Error: all images must have the same dimensions and number of colors.');
            goto 666;
         end;
         AddImg2SingleArray(lTemp,lMeanRA);

     end;
     //mean: divide by number of images we are averaging
     for lI := 1 to lBytes do
            lMeanRA^[lI] := lMeanRA^[lI]/lFilenames.Count;

     //save image

     if (lDest.PixelFormat32 = 32) then begin
        for lI := 1 to lnPix do
            lDest.Img32^[lI].rgbRed := round(lMeanRA^[lI]);
        lOffset :=  lnPix;
        for lI := 1 to lnPix do
            lDest.Img32^[lI].rgbGreen := round(lMeanRA^[lI+lOffset]);
        lOffset :=  lnPix*2;
        for lI := 1 to lnPix do
            lDest.Img32^[lI].rgbBlue := round(lMeanRA^[lI+lOffset]);
        lOffset :=  lnPix*3;
        for lI := 1 to lnPix do
            lDest.Img32^[lI].rgbreserved := round(lMeanRA^[lI+lOffset]);
     end else
         for lI := 1 to lnPix do
             lDest.Img^[lI] := round(lMeanRA^[lI]);



     //CopyInput(Bmp, lDest,false);

     result := true;
666:
    FreeImg(lTemp);
    freemem(lMeanRA);

end;

procedure RotateDeg (Amount: single; lSrc,lDest: TImg; lSmoothFunc: integer);
//uses math unit for degtorad
Type
    TMatrix  = record
       Xx,Yx,Xy,Yy : single;
    end;
var
   lMat:TMatrix;
   lXs,lYs,lAngle,lMidX,lMidY: single;
   lX,lY,lXi,lYi,lOut: integer;
begin
     lAngle := degtorad(Amount);
     lMat.Xx := cos(lAngle);
     lMat.Yx := -sin(lAngle);
     lMat.Xy := sin(lAngle);
     lMat.Yy := cos(lAngle);
     lXi := lSrc.Wid;
     lYi := lSrc.Ht;
     lMidX := lXi / 2;
     lMidY := lYi /2;
     lOut := 0;
     //CopyImg( lSrc, lDest);
     if lSrc.PixelFormat32 = 32 then begin
         for lY := 1 to lYi do begin
           for lX := 1 to lXi do begin
             inc(lOut);
             lXs := lMidX+((lX-lMidX)*lMat.Xx)+((lY-lMidY)*lMat.Xy);
             lYs := lMidY+((lX-lMidX)*lMat.Yx)+((lY-lMidY)*lMat.Yy);
             lDest.Img32^[lOut] := Intensity32 (lXs,lYs, lSrc, lSmoothFunc);
           end; {lX}
         end; {lY}

     end else begin
         for lY := 1 to lYi do begin
           for lX := 1 to lXi do begin
             inc(lOut);
             lXs := lMidX+((lX-lMidX)*lMat.Xx)+((lY-lMidY)*lMat.Xy);
             lYs := lMidY+((lX-lMidX)*lMat.Yx)+((lY-lMidY)*lMat.Yy);
             lDest.Img^[lOut] := Intensity (lXs,lYs, lSrc, lSmoothFunc);
           end; {lX}
         end; {lY}
     end;
end;

end.
