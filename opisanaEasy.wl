(* ::Package:: *)

BeginPackage["OpisanaEasy`"]

Begin["`Private`"]

(*pomocn\[EAcute] funkcie *)
zlomok[q_] := If[IntegerQ[q], ToString[q],
  ToString[Numerator[q]] <> "/" <> ToString[Denominator[q]]]

(* \[CHacek]\[IAcute]slo s desatinnou \[CHacek]iarkou, d = po\[CHacek]et desatinn\[YAcute]ch miest *)
cislo[x_, d_: 2] := If[IntegerQ[x], ToString[x],
  StringReplace[
   StringReplace[
    StringReplace[ToString[NumberForm[N[x], {15, d}]],
     (("0" ..) ~~ EndOfString) -> ""],
    ("." ~~ EndOfString) -> ""],
   "." -> ","]]

(* text \[CHacek]\[IAcute]sla: 3 Sqrt[2] -> 3\[Sqrt]2, 3/5 -> 3/5 *)
txt[x_] := StringReplace[ToString[x, InputForm],
  {RegularExpression["Sqrt\\[(\\d+)\\]"] -> "\[Sqrt]$1", "*" -> ""}]

hodnota[q_, d_: 2] := Which[
  IntegerQ[q], ToString[q],
  Head[q] === Rational && Divisible[10^d, Denominator[q]],
   zlomok[q] <> " = " <> cislo[q, d],
  Head[q] === Rational, zlomok[q] <> " \[TildeTilde] " <> cislo[q, d],
  True, txt[q] <> " \[TildeTilde] " <> cislo[q, d]]

popisVrcholov[p_, mena_] := MapThread[
  Text[Style[#1, Bold, 14], Offset[14 Normalize[N[#2 - Mean[p]]], #2]] &,
  {mena, p}]

(* gener\[AAcute]tor troj\[IAcute]c*)
cele = Range[1, 15];
odmocniny = Flatten[Table[x Sqrt[y], {x, 1, 4}, {y, 2, 9}]];
zlomky = {1/2, 1/3, 2/3, 1/4, 3/4, 2/5, 3/5, 4/5, 3/2, 4/3, 5/3, 5/2};

strany = DeleteDuplicates[Join[cele, odmocniny, zlomky]];
strany = SortBy[Select[strany, N[#] <= 15 &], N];   (* \[CHacek]\[IAcute]seln\[EAcute] zoradenie *)

(* a < b < c, preto s\[UAcute] odvesny v\[ZHacek]dy r\[OHat]zne *)
trojice = Select[
   Subsets[strany, {3}],
   PossibleZeroQ[#[[1]]^2 + #[[2]]^2 - #[[3]]^2] &];


opisanaEasy[] := Module[
  {tr, l1, l2, c, la, lb, dl, kr, pE, pF, pG, cen, m, zad, ries, ob},

  tr = RandomChoice[trojice];
  {l1, l2, c} = tr;
  dl = Max[l1, l2];
  kr = Min[l1, l2];
  {la, lb} = RandomSample[{l1, l2}];

  zad = StringForm[
    "O pravouhlom trojuholn\[IAcute]ku EFG s r\[OHat]znymi d\:013a\[ZHacek]kami odvesien vieme, \[ZHacek]e jeho najdlh\[SHacek]ia v\[YAcute]\[SHacek]ka meria `1` cm a jeho druh\[AAcute] najdlh\[SHacek]ia v\[YAcute]\[SHacek]ka meria `2` cm. Ak\[YAcute] polomer m\[AAcute] kru\[ZHacek]nica op\[IAcute]san\[AAcute] tomuto trojuholn\[IAcute]ku?",
    txt[dl], txt[kr]];

  ries = StringForm[
    StringRiffle[{
      "Rie\[SHacek]enie",
      "1. V pravouhlom trojuholn\[IAcute]ku s\[UAcute] odvesny z\[AAcute]rove\[NHacek] v\[YAcute]\[SHacek]kami (v\[YAcute]\[SHacek]ka na jednu odvesnu je druh\[AAcute] odvesna). V\[YAcute]\[SHacek]ka na preponu v_c = a \[CenterDot] b / c je men\[SHacek]ia ako obe odvesny, preto\[ZHacek]e a / c < 1 aj b / c < 1.",
      "2. Dve najdlh\[SHacek]ie v\[YAcute]\[SHacek]ky s\[UAcute] preto odvesny: `1` cm a `2` cm.",
      "3. Prepona: c = \[Sqrt](`1`\.b2 + `2`\.b2) = \[Sqrt]`3` = `4` cm.",
      "4. Pod\:013ea T\[AAcute]lesovej vety le\[ZHacek]\[IAcute] stred op\[IAcute]sanej kru\[ZHacek]nice v strede prepony, tak\[ZHacek]e R = c / 2 = `4` / 2 = `5` cm.",
      "Odpove\[DHacek]: polomer op\[IAcute]sanej kru\[ZHacek]nice je `5` cm."},
     "\n"],
    txt[dl], txt[kr], txt[dl^2 + kr^2], txt[c], hodnota[c/2]];

  (* prav\[YAcute] uhol pri G prepona EF *)
  pG = {0, 0};
  pF = {la, 0};
  pE = {0, lb};
  cen = (pE + pF)/2;
  m = Min[la, lb]/8;

  ob = Graphics[{
    LightBlue, EdgeForm[{Thick, Black}], Polygon[{pE, pF, pG}],
    {Opacity[0.15, Yellow], Disk[cen, c/2]},
    {Gray, Dashed, Circle[cen, c/2]},
    {Black, Line[{{m, 0}, {m, m}, {0, m}}]},
    Black, PointSize[Large], Point[{pE, pF, pG, cen}],
    popisVrcholov[{pE, pF, pG}, {"E", "F", "G"}],
    Text[Style["S", Bold, 14], cen, {1.6, 1.2}],
    Text[Style["c = " <> txt[c], 12, Background -> White], cen, {-1.2, -1.2}],
    Text[Style[txt[la] <> " cm", 12, Background -> White], (pF + pG)/2, {0, 1.6}],
    Text[Style[txt[lb] <> " cm", 12, Background -> White], (pE + pG)/2, {1.6, 0}]},
   Frame -> True,
   PlotRange -> All,
   AspectRatio -> Automatic,
   PlotRangePadding -> Scaled[0.15],
   ImageSize -> 320];

  {zad, ries, ob}
]

End[]
EndPackage[]

OpisanaEasy`Private`opisanaEasy[]




