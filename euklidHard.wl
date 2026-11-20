(* ::Package:: *)

BeginPackage["EuklidHard`"]
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
euklidHard[] := Module[
  {moznosti, p, q, t, r, ca, cb, c, v, a, b, zad, ries, ob},

  (* c_a : c_b = p^2 : q^2, p^2 + q^2 = r^2, c_a = p^2 t, c_b = q^2 t, v_c = p q t *)
  moznosti = {{3, 4, 1}, {3, 4, 2}, {3, 4, 3}, {4, 3, 1}, {4, 3, 2}, {4, 3, 3},
    {5, 12, 1}, {12, 5, 1}};
  {p, q, t} = RandomChoice[moznosti];
  r = Sqrt[p^2 + q^2];
  ca = p^2 t;
  cb = q^2 t;
  c = ca + cb;
  v = p q t;
  a = r p t;
  b = r q t;

  zad = StringForm[
    "Vypo\:010d\:00edtajte odvesny pravouhl\:00e9ho trojuholn\:00edka s preponou c, ak v\:00fd\:0161ka na preponu je v_c = `1` cm a \:00faseky prepony s\:00fa v pomere c_a : c_b = `2` : `3`.",
    v, p^2, q^2];

  ries = StringForm[
    StringRiffle[{
      "Rie\:0161enie",
      "1. \:00daseky prepony zap\:00ed\:0161eme ako c_a = `1`x a c_b = `2`x pre nejak\:00e9 x > 0.",
      "2. Euklidova veta o v\:00fd\:0161ke: v_c\:00b2 = c_a \:00b7 c_b, teda `3`\:00b2 = `1`x \:00b7 `2`x = `4`x\:00b2.",
      "3. Z toho x\:00b2 = `3`\:00b2 / `4` = `5`, a ke\:010f\:017ee x > 0, je x = `6`.",
      "4. c_a = `1` \:00b7 `6` = `7` cm, c_b = `2` \:00b7 `6` = `8` cm, c = c_a + c_b = `9` cm.",
      "5. Euklidova veta o odvesne: a\:00b2 = c \:00b7 c_a = `9` \:00b7 `7` = `10`, teda a = `11` cm.",
      "6. Podobne b\:00b2 = c \:00b7 c_b = `9` \:00b7 `8` = `12`, teda b = `13` cm.",
      "Odpove\:010f: odvesny maj\:00fa `11` cm a `13` cm."},
     "\n"],
    p^2, q^2, v, p^2 q^2, t^2, t, ca, cb, c, c ca, a, c cb, b];

  ob = vykresliTrojuholnik[ca, cb, v,
    oznac["c", "a", ca], oznac["c", "b", cb], oznac["v", "c", v],
    "a = " <> ToString[a], "b = " <> ToString[b], "c = " <> ToString[c]];

  {zad, ries, ob}
]
End[]
EndPackage[]
