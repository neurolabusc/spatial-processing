unit otsu;
interface
uses imgfunc, sysutils, otsuml;

function FindOtsux (var Img: Bytep; nVox: integer): byte;
function ApplyOtsux (var Img: Bytep; nVox: integer): byte;
 // procedure TestO;
implementation

Type
HistoRA = array [0..255] of longint;
HistoRAd = array [0..255] of double;

Function OtsuCostFunc(H: HistoRA): integer;
//Otsu N (1979) A threshold selection method from gray-level histograms". IEEE Trans. Sys., Man., Cyber. 9: 62-66.
//http://en.wikipedia.org/wiki/Otsu's_method
//http://www.labbookpages.co.uk/software/imgProc/otsuThreshold.html
//returns threshold for binarizing an image
// all voxel <=Threshold are background
// all voxel >Threshold are object
const
  kMaxBin = 255;
var
   t,total: integer;
   wB,wF,Sum,SumB,mF,mB,varBetween,varMax: double;
begin
     result := 0;
     wB := 0;
     wF := 0;
     SumB := 0;
  	 Sum := 0;
     Total := 0;
     varMax := 0;
     for t := 0 to kMaxBin do
     	 Total := Total + H[t];
     if Total = 0 then exit;
     for t := 0 to kMaxBin do
     	 Sum := Sum + (t*H[t]);
	 for t :=0 to kMaxBin do begin
   	 	 wB :=  wB + H[t];               // Weight Background
   		 if (wB = 0) then continue;
		 wF := Total - wB;                 // Weight Foreground
   		 if (wF = 0) then break;
   		 sumB := sumB+(t * H[t]);
         mB := sumB / wB;            // Mean Background
   		 mF := (sum - sumB) / wF;    // Mean Foreground
         // Calculate Between Class Variance
   		 varBetween := (wB/Total) * (wF/Total) * sqr(mB - mF);
         //showmessage('t'+inttostr(t)+' weightB '+floattostr(wB/Total)+' weightF'+floattostr(wF/Total)+' meanB'+floattostr(mB)+' meanF'+floattostr(mF)+' vBetween'+floattostr(varBetween));
         // Check if new maximum found
   		 if (t=0) or (varBetween > varMax) then begin
      	 	varMax := varBetween;
      		result := t;
   		 end;
     end;
end;


(*procedure TestO;
//http://www.labbookpages.co.uk/software/imgProc/otsuThreshold.html
var
  H: HistoRA;
begin
	 H[0] := 8;
     H[1] := 7;
     H[2] := 2;
	 H[3] := 6;
     H[4] := 9;
     H[5] := 4;
     OtsuCostFuncx(H);
end;  *)

function FindOtsux (var Img: Bytep; nVox: integer): byte;
var
  n: integer;
  lHisto: HistoRA;
begin
  result := 128;
  if nVox < 1 then exit;
  //create histogram
  for n := 0 to 255 do
    lHisto[n] := 0;
  for n := 0 to nVox do
    inc(lHisto[Img^[n]]);
  //now find minimum intraclass variance....
  result := OtsuCostFunc(lHisto);
end;

function ApplyOtsux (var Img: Bytep; nVox: integer): byte;
var
  n: integer;
begin
  result := 128;
  if nVox < 1 then exit;
  result := FindOtsu2(Img,nVox);
  //showmessage(inttostr(result));
  for n := 1 to nVox do
    if Img^[n] > result then
      Img^[n] := 255
    else
      Img^[n] := 0;
end;

end.
 