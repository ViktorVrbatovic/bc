(* ::Package:: *)

BeginPackage["RovnostrannyNormal`"]
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

rovnostrannyNormal[] := Module[
  {a, v, s, ok, an, hn, cen, extra, zad, ries, ob},

  a = RandomInteger[{1, 8}];
  v = Sqrt[3]/2 a;
  s = Sqrt[3]/4 a^2;
  ok = Sqrt[3]/6 a;

  an = N[a];
  hn = N[Sqrt[3]/2 a];
  cen = N[{a/2, Sqrt[3]/6 a}];
  extra = {
    {Opacity[0.3, Yellow], Disk[cen, N[ok]]},
    {Black, Circle[cen, N[ok]]},
    {Black, Dashed, Line[{{an/2, 0}, {an/2, hn}}]},
    {Black, PointSize[Large], Point[cen]},
    Text[Style["O", Bold, 14], cen, {1.6, 0}],
    Text[Style["v", Italic, 14], {an/2, 0.85 hn}, {-1.6, 0}]};

  zad = StringForm[
    "Rovnostrann\:00fd trojuholn\:00edk m\:00e1 v\:00fd\:0161ku v = `1` cm. Vypo\:010d\:00edtajte jeho obsah a polomer vp\:00edsanej kru\:017enice.",
    v];

  ries = StringForm[
    StringRiffle[{
      "Rie\:0161enie",
      "1. Zo vzorca v = a\:221a3 / 2 vyjadr\:00edme stranu: a = 2v / \:221a3 = `2` cm.",
      "2. Obsah: S = a \:00b7 v / 2 = `2` \:00b7 `1` / 2 = `3` cm\:00b2.",
      "3. \:0164a\:017eisko (stred vp\:00edsanej kru\:017enice) del\:00ed v\:00fd\:0161ku v pomere 2 : 1, preto r = v / 3 = `1` / 3 = `4` cm.",
      "Odpove\:010f: S = `3` cm\:00b2, r = `4` cm."},
     "\n"],
    v, a, s, ok];

  ob = vykresliTrojuholnik[a, extra];

  {zad, ries, ob}
]
End[]
EndPackage[]
