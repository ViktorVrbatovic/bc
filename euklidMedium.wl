(* ::Package:: *)

BeginPackage["EuklidMedium`"]
Begin["`Private`"]

popisVrcholov[p_, mena_] := MapThread[
  Text[Style[#1, Bold, 14], Offset[14 Normalize[N[#2 - Mean[p]]], #2]] &,
  {mena, p}]
oznac[meno_, sub_, hodn_] := Row[{Subscript[meno, sub], " = ", ToString[hodn]}]
(* pravouhly trojuholnik ABC (pravy uhol pri C), vyska CP na preponu AB *)
vykresliTrojuholnik[ca_, cb_, v_, lca_, lcb_, lv_, la_, lb_, lc_] := Module[
  {pA, pB, pC, pP, m, u1, u2},
  pA = N[{0, 0}]; pB = N[{ca + cb, 0}]; pP = N[{cb, 0}]; pC = N[{cb, v}];
  m = N[Min[ca, cb, v]/5];
  u1 = Normalize[pA - pC]; u2 = Normalize[pB - pC];
  Graphics[{
    LightBlue, EdgeForm[{Thick, Black}], Polygon[{pA, pB, pC}],
    {Black, Dashed, Line[{pC, pP}]},
    {Black, Line[{pP + {m, 0}, pP + {m, m}, pP + {0, m}}]},
    {Black, Line[{pC + m u1, pC + m (u1 + u2), pC + m u2}]},
    Black, PointSize[Large], Point[{pA, pB, pC, pP}],
    popisVrcholov[{pA, pB, pC}, {"A", "B", "C"}],
    Text[Style["P", Bold, 14], pP, {0, 1.6}],
    Text[Style[lcb, 12, Background -> White], (pA + pP)/2, {0, 3.2}],
    Text[Style[lca, 12, Background -> White], (pP + pB)/2, {0, 3.2}],
    Text[Style[lv, 12, Background -> White], (pC + pP)/2, {1.3, 0}],
  
    If[la =!= "",
     Text[Style[la, 12, Background -> White], (pB + pC)/2, {-1.2, -1.2}], {}],
    If[lb =!= "",
     Text[Style[lb, 12, Background -> White], (pA + pC)/2, {1.2, -1.2}], {}],
    If[lc =!= "",
     Text[Style[lc, 12, Background -> White], (pA + pB)/2, {0, 6}], {}]},
   Frame -> True,
   PlotRange -> All,
   AspectRatio -> Automatic,
   PlotRangePadding -> Scaled[0.15],
   ImageSize -> 350]
  ]
euklidMedium[] := Module[
  {mnq, m, n, q, k, ca, cb, c, a, b, v, dis, odm, obs, zad, ries, ob},

  (* c_a = k m^2, c_b = k n^2, c = k q^2, a = k m q, v_c = k m n, kde m^2 + n^2 = q^2 *)
  mnq = {{3, 4, 5}, {4, 3, 5}, {5, 12, 13}, {12, 5, 13}, {8, 15, 17}, {15, 8, 17}};
  {m, n, q} = RandomChoice[mnq];
  k = If[q == 5, RandomInteger[{1, 3}], 1];
  ca = k m^2;
  cb = k n^2;
  c = k q^2;
  a = k m q;
  b = k n q;
  v = k m n;
  dis = cb^2 + 4 a^2;
  odm = Sqrt[dis];
  obs = c v/2;

  zad = StringForm[
    "V pravouhlom trojuholn\[IAcute]ku ABC s preponou c je c_b = `1` cm a a = `2` cm. Vypo\[CHacek]\[IAcute]tajte v\[YAcute]\[SHacek]ku na preponu a obsah trojuholn\[IAcute]ka.",
    cb, a];

  ries = StringForm[
    StringRiffle[{
      "Rie\[SHacek]enie",
      "1. Euklidova veta o odvesne: a\.b2 = c \[CenterDot] c_a, pri\[CHacek]om c = c_a + c_b, teda c_a = c - c_b.",
      "2. Dosad\[IAcute]me: `1`\.b2 = c \[CenterDot] (c - `2`), \[CHacek]i\[ZHacek]e c\.b2 - `2`c - `3` = 0.",
      "3. Kladn\[YAcute] kore\[NHacek]: c = (`2` + \[Sqrt](`2`\.b2 + 4 \[CenterDot] `3`)) / 2 = (`2` + \[Sqrt]`4`) / 2 = (`2` + `5`) / 2 = `6` cm.",
      "4. c_a = c - c_b = `6` - `2` = `7` cm.",
      "5. Euklidova veta o v\[YAcute]\[SHacek]ke: v_c\.b2 = c_a \[CenterDot] c_b = `7` \[CenterDot] `2` = `8`, teda v_c = `9` cm.",
      "6. Obsah: S = c \[CenterDot] v_c / 2 = `6` \[CenterDot] `9` / 2 = `10` cm\.b2.",
      "Odpove\[DHacek]: v\[YAcute]\[SHacek]ka na preponu je `9` cm a obsah trojuholn\[IAcute]ka je `10` cm\.b2."},
     "\n"],
    a, cb, a^2, dis, odm, c, ca, ca cb, v, obs];

  ob = vykresliTrojuholnik[ca, cb, v,
    oznac["c", "a", ca], oznac["c", "b", cb], oznac["v", "c", v],
    "a = " <> ToString[a], "b = " <> ToString[b], "c = " <> ToString[c]];

  {zad, ries, ob}
]
End[]
EndPackage[]

EuklidMedium`Private`euklidMedium[]



