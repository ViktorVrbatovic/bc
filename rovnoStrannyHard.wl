(* ::Package:: *)

BeginPackage["RovnostrannyHard`"]
Begin["`Private`"]

(* rovnostranny trojuholnik so stranou a; extra = dalsie grafick\:00e9 prvky *)
vykresliTrojuholnik[a_, extra_List : {}] := Module[{p, st},
  p = N[{{0, 0}, {a, 0}, {a/2, Sqrt[3]/2 a}}];
  st = Mean[p];
  Graphics[{
    LightBlue, EdgeForm[{Thick, Black}], Polygon[p],
    Black, PointSize[Large], Point[p],
    MapThread[
     Text[Style[#1, Bold, 14], Offset[14 Normalize[#2 - st], #2]] &,
     {{"A", "B", "C"}, p}],
    extra},
   Frame -> True,
   PlotRange -> All,
   AspectRatio -> Automatic,
   PlotRangePadding -> Scaled[0.15],
   ImageSize -> 300]
  ]

rovnostrannyHard[] := Module[
  {a, ok, sk, r2, a2, s, cen, extra, zad, ries, ob},

  a = RandomChoice[{2, 4, 6, 8, 12, 14, 16, 18}];
  ok = Sqrt[3]/6 a;
  r2 = a^2/12;
  sk = Pi r2;
  a2 = a^2;
  s = Sqrt[3]/4 a^2;

  cen = N[{a/2, Sqrt[3]/6 a}];
  extra = {
    {Opacity[0.3, Yellow], Disk[cen, N[ok]]},
    {Black, Circle[cen, N[ok]]},
    {Black, PointSize[Large], Point[cen]},
    Text[Style["O", Bold, 14], cen, {1.6, 0}]};

  zad = StringForm[
    "Ko\:013eko cm\:00b2 m\:00e1 obsah rovnostrann\:00e9ho trojuholn\:00edka, ak viete, \:017ee obsah kruhu, ktor\:00fd je mu vp\:00edsan\:00fd, je S = `1` cm\:00b2?",
    sk];

  ries = StringForm[
    StringRiffle[{
      "Rie\:0161enie",
      "1. Obsah vp\:00edsan\:00e9ho kruhu je S_k = \:03c0 r\:00b2, preto r\:00b2 = S_k / \:03c0 = `1` / \:03c0 = `2` cm\:00b2.",
      "2. V rovnostrannom trojuholn\:00edku so stranou a plat\:00ed r = v / 3 = (a\:221a3 / 2) / 3 = a\:221a3 / 6, teda r\:00b2 = a\:00b2 / 12.",
      "3. Z toho a\:00b2 = 12 \:00b7 r\:00b2 = 12 \:00b7 `2` = `3` cm\:00b2 (a = `4` cm).",
      "4. Obsah trojuholn\:00edka: S = (\:221a3 / 4) \:00b7 a\:00b2 = (\:221a3 / 4) \:00b7 `3` = `5` cm\:00b2.",
      "Odpove\:010f: obsah trojuholn\:00edka je `5` cm\:00b2."},
     "\n"],
    sk, r2, a2, a, s];

  ob = vykresliTrojuholnik[a, extra];

  {zad, ries, ob}
]
End[]
EndPackage[]
