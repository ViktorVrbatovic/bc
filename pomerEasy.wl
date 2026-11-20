(* ::Package:: *)

BeginPackage["PomerEasy`"]
Begin["`Private`"]

(* trojuholnik podla uhlov (v stupnoch) pri vrcholoch A, B, C *)
vykresliTrojuholnik[uhly_List] := Module[{al, be, ga, b, p, st},
  {al, be, ga} = uhly Degree;
  b = Sin[be]/Sin[ga];
  p = N[{{0, 0}, {1, 0}, {b Cos[al], b Sin[al]}}];
  st = Mean[p];
  Graphics[{
    LightBlue, EdgeForm[{Thick, Black}], Polygon[p],
    Black, PointSize[Large], Point[p],
    MapThread[
     Text[Style[#1, Bold, 14], Offset[14 Normalize[#2 - st], #2]] &,
     {{"A", "B", "C"}, p}],
    MapThread[
     Text[Style[ToString[#1] <> "\:00b0", 12], #2 + 0.3 (st - #2)] &,
     {uhly, p}]},
   Frame -> True,
   PlotRange -> All,
   AspectRatio -> Automatic,
   PlotRangePadding -> Scaled[0.15],
   ImageSize -> 300]
  ]

pomerEasy[] := Module[
  {pomer, perm, mena, d1, d2, a, b, c, d, l, e1, e3, spolu, diel, uc, u,
   zad, ries, ob},

  While[True,
   pomer = RandomInteger[{1, 8}, 3];

   (* aby sa to pekne dalo rozlozit na 3 pomery *)
   If[! (Length[DeleteDuplicates[pomer]] == 3 &&
        MemberQ[{6, 9, 10}, Total[pomer]]), Continue[]];

   d1 = GCD[pomer[[1]], pomer[[2]]];
   d2 = GCD[pomer[[2]], pomer[[3]]];
   {a, b} = {pomer[[1]], pomer[[2]]}/d1;
   {c, d} = {pomer[[2]], pomer[[3]]}/d2;
   l = LCM[b, c];
   e1 = a l/b;
   e3 = d l/c;
   spolu = e1 + l + e3;
   diel = 180/spolu;
   If[IntegerQ[diel], Break[]]
  ];

  (* ktore uhly su v retazci pomerov a v akom poradi *)
  perm = RandomSample[{1, 2, 3}];
  mena = {"\:03b1", "\:03b2", "\:03b3"}[[perm]];
  uc = {e1, l, e3} diel;
  u = ConstantArray[0, 3];
  Do[u[[perm[[i]]]] = uc[[i]], {i, 3}];

  zad = StringForm[
    "Ve\:013ekosti vn\:00fatorn\:00fdch uhlov trojuholn\:00edka ABC s\:00fa v pomere `1`:`2` = `4`:`5` a `2`:`3` = `6`:`7`. Ur\:010dte ve\:013ekosti v\:0161etk\:00fdch uhlov trojuholn\:00edka.",
    mena[[1]], mena[[2]], mena[[3]], a, b, c, d];

  ries = StringForm[
    StringRiffle[{
      "Rie\:0161enie",
      "1. Spolo\:010dn\:00fd uhol oboch pomerov je `2`: v prvom pomere mu zodpoved\:00e1 \:010d\:00edslo `5`, v druhom \:010d\:00edslo `6`. Najmen\:0161\:00ed spolo\:010dn\:00fd n\:00e1sobok \:010d\:00edsel `5` a `6` je `8`.",
      "2. Pomery roz\:0161\:00edrime tak, aby `2` bolo v oboch rovn\:00e9 `8`:",
      "\t`1`:`2` = `4`:`5` = `11`:`8`   (roz\:0161\:00edren\:00e9 `9`-kr\:00e1t)",
      "\t`2`:`3` = `6`:`7` = `8`:`12`   (roz\:0161\:00edren\:00e9 `10`-kr\:00e1t)",
      "3. Spojen\:00fd pomer: `1`:`2`:`3` = `11`:`8`:`12`",
      "4. S\:00fa\:010det vn\:00fatorn\:00fdch uhlov je 180\:00b0. Pomer m\:00e1 `11` + `8` + `12` = `13` dielov, jeden diel: 180\:00b0 : `13` = `14`\:00b0.",
      "5. `1` = `11` \:00b7 `14`\:00b0 = `15`\:00b0,  `2` = `8` \:00b7 `14`\:00b0 = `16`\:00b0,  `3` = `12` \:00b7 `14`\:00b0 = `17`\:00b0.",
      "Odpove\:010f: \:03b1 = `18`\:00b0, \:03b2 = `19`\:00b0, \:03b3 = `20`\:00b0."},
     "\n"],
    mena[[1]], mena[[2]], mena[[3]], a, b, c, d, l, l/b, l/c, e1, e3, spolu, diel,
    uc[[1]], uc[[2]], uc[[3]], u[[1]], u[[2]], u[[3]]];

  ob = vykresliTrojuholnik[u];

  {zad, ries, ob}
]
End[]
EndPackage[]
