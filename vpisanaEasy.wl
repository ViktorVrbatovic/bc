(* ::Package:: *)

BeginPackage["VpisanaEasy`"]
Begin["`Private`"]

popisVrcholov[p_, mena_] := MapThread[
  Text[Style[#1, Bold, 14], Offset[14 Normalize[N[#2 - Mean[p]]], #2]] &,
  {mena, p}]
(* body A, B, C trojuholnika so stranami a = |BC|, b = |CA|, c = |AB| *)
telesaTrojuholnika[a_, b_, c_] := Module[{x},
  x = (b^2 + c^2 - a^2)/(2 c);
  N[{{0, 0}, {c, 0}, {x, Sqrt[b^2 - x^2]}}]]
(* popisy stran trojuholnika: texty = {text pre BC, CA, AB}, "" = nepopisovat *)
popisStran[p_, texty_List, odstup_: 24] := Table[
  With[{mid = Mean[p[[{{2, 3}, {3, 1}, {1, 2}}[[i]]]]]},
   If[texty[[i]] === "", {},
    Text[Style[texty[[i]], 12],
     Offset[odstup Normalize[N[mid - Mean[p]]], mid]]]],
  {i, 3}]
vpisanaEasy[] := Module[
  {xa, xb, xc, g, p, q, ab, bc, ca, o, sp, obs, r, pts, pA, pB, pC, cen, pT, pV, pU,
   zad, ries, ob},

  (* xa, xb, xc = dlzky dotycnic z vrcholov A, B, C *)
  While[True,
   {xa, xb, xc} = RandomInteger[{1, 8}, 3];
   If[xa != xb, Break[]]
  ];
  g = GCD[xa, xb];
  p = xa/g;
  q = xb/g;
  ab = xa + xb;
  bc = xb + xc;
  ca = xa + xc;
  o = ab + bc + ca;
  sp = o/2;

  zad = StringForm[
    "Obvod trojuholn\[IAcute]ka ABC je `1` cm. Bod T je bod dotyku jeho vp\[IAcute]sanej kru\[ZHacek]nice na strane AB. Pre bod T plat\[IAcute] |AT| : |TB| = `2` : `3` a strana AB m\[AAcute] `4` cm. Ko\:013eko cm m\[AAcute] strana BC?",
    o, p, q, ab];

ries = StringForm[
  StringRiffle[{
    "Rie\[SHacek]enie",
    "1. Vypo\[CHacek]\[IAcute]tame stranu c = |AB| a jej rozdelenie. Stranu AB rozdel\[IAcute]me v pomere `1` : `2` na `3` dielov. Jeden diel je `4` : `3` = `5` cm, tak\[ZHacek]e |AT| = `1` \[CenterDot] `5` = `6` cm a |TB| = `2` \[CenterDot] `5` = `7` cm.",
    "2. Ke\[DHacek]\[ZHacek]e doty\[CHacek]nice veden\[EAcute] z jedn\[EAcute]ho bodu k vp\[IAcute]sanej kru\[ZHacek]nici s\[UAcute] rovnako dlh\[EAcute], ozna\[CHacek]me U, V body dotyku na stran\[AAcute]ch AC, BC. Plat\[IAcute] |AU| = |AT| = `6` cm, |BV| = |TB| = `7` cm a |CU| = |CV|, preto t\[YAcute]mto \[UAcute]se\[CHacek]k\[AAcute]m prirad\[IAcute]me d\:013a\[ZHacek]ky.",
    "3. Od obvodu `8` cm odpo\[CHacek]\[IAcute]tame d\:013a\[ZHacek]ky \[UAcute]sekov, ktor\[EAcute] u\[ZHacek] pozn\[AAcute]me: `8` - (2 \[CenterDot] `6`) - (2 \[CenterDot] `7`) = `9` cm.",
    "4. Zvy\[SHacek]ok tvoria dve rovnak\[EAcute] \[UAcute]se\[CHacek]ky |CU| a |CV|, preto del\[IAcute]me dvomi: |CV| = `9` / 2 = `10` cm.",
    "5. |BC| = |BV| + |CV| = `7` + `10` = `11` cm.",
    "Odpove\[DHacek]: strana BC m\[AAcute] `11` cm."},
   "\n"],
  p, q, p + q, ab, g, xa, xb, o, sp, xc, bc];

  pts = telesaTrojuholnika[bc, ca, ab];
  {pA, pB, pC} = pts;
  obs = Sqrt[sp (sp - bc) (sp - ca) (sp - ab)];
  r = N[obs/sp];
  cen = (bc pA + ca pB + ab pC)/o;
  pT = pA + xa/ab (pB - pA);
  pV = pB + xb/bc (pC - pB);
  pU = pC + xc/ca (pA - pC);

  ob = Graphics[{
    LightBlue, EdgeForm[{Thick, Black}], Polygon[pts],
    {Opacity[0.3, Yellow], Disk[cen, r]},
    {Black, Circle[cen, r]},
    {Black,Thick, Line[{cen, pT}],
      Line[{cen, pV}],
      Line[{cen, pU}]},
     {Black,Thick, Line[{cen, pA}],
      Line[{cen, pB}],
      Line[{cen, pC}]},
    Black, PointSize[Large], Point[Join[pts, {pT, pV, pU, cen}]],
    popisVrcholov[pts, {"A", "B", "C"}],
    popisStran[pts, {"a = " <> ToString[bc], "b = " <> ToString[ca], "c = " <> ToString[ab]}, 32],
    Text[Style["T", Bold, 14], Offset[14 Normalize[pT - cen], pT]],
    Text[Style["V", Bold, 14], Offset[14 Normalize[pV - cen], pV]],
    Text[Style["U", Bold, 14], Offset[14 Normalize[pU - cen], pU]],
    Text[Style[ToString[xa], 12, Background -> White], (pA + pT)/2, {0, 1.6}],
    Text[Style[ToString[xb], 12, Background -> White], (pT + pB)/2, {0, 1.6}]},
   Frame -> True,
   PlotRange -> All,
   AspectRatio -> Automatic,
   PlotRangePadding -> Scaled[0.15],
   ImageSize -> 320];

  {zad, ries, ob}
]
End[]
EndPackage[]


VpisanaEasy`Private`vpisanaEasy[]

