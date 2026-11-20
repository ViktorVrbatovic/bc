(* ::Package:: *)

BeginPackage["VpisanaMedium`"]
Begin["`Private`"]

zlomok[q_] := If[IntegerQ[q], ToString[q],
  ToString[Numerator[q]] <> "/" <> ToString[Denominator[q]]]
(* cislo s desatinnou ciarkou, d = pocet desatinnych miest *)
cislo[x_, d_: 2] := If[IntegerQ[x], ToString[x],
  StringReplace[
   StringReplace[
    StringReplace[ToString[NumberForm[N[x], {15, d}]],
     (("0" ..) ~~ EndOfString) -> ""],
    ("." ~~ EndOfString) -> ""],
   "." -> ","]]
popisVrcholov[p_, mena_] := MapThread[
  Text[Style[#1, Bold, 14], Offset[14 Normalize[N[#2 - Mean[p]]], #2]] &,
  {mena, p}]
(* popisy stran trojuholnika: texty = {text pre BC, CA, AB}, "" = nepopisovat *)
popisStran[p_, texty_List, odstup_: 24] := Table[
  With[{mid = Mean[p[[{{2, 3}, {3, 1}, {1, 2}}[[i]]]]]},
   If[texty[[i]] === "", {},
    Text[Style[texty[[i]], 12],
     Offset[odstup Normalize[N[mid - Mean[p]]], mid]]]],
  {i, 3}]
(* rovnostranny trojuholnik so stranou a; extra = dalsie graficke prvky *)
vykresliTrojuholnik[a_, extra_List : {}, popisA_: ""] := Module[{p},
  p = N[{{0, 0}, {a, 0}, {a/2, Sqrt[3]/2 a}}];
  Graphics[{
    LightBlue, EdgeForm[{Thick, Black}], Polygon[p],
    Black, PointSize[Large], Point[p],
    popisVrcholov[p, {"A", "B", "C"}],
    If[popisA =!= "", popisStran[p, {popisA, popisA, popisA}], {}],
    extra},
   Frame -> True,
   PlotRange -> All,
   AspectRatio -> Automatic,
   PlotRangePadding -> Scaled[0.15],
   ImageSize -> 300]
  ]
vpisanaMedium[] := Module[
  {rho, v, a, obs, an, hn, cen, extra, zad, ries, ob},

  rho = RandomInteger[{20, 150}]/10;
  v = 3 rho;
  a = 2 v/Sqrt[3];
  obs = 3 Sqrt[3] rho^2;

  zad = StringForm[
    "Polomer kru\:017enice vp\:00edsanej do rovnostrann\:00e9ho trojuholn\:00edka je \:03c1 = `1` cm. Vypo\:010d\:00edtajte obsah trojuholn\:00edka.",
    cislo[rho]];

  ries = StringForm[
    StringRiffle[{
      "Rie\:0161enie",
      "1. V rovnostrannom trojuholn\:00edku spl\:00fdva stred vp\:00edsanej kru\:017enice s \:0165a\:017eiskom, ktor\:00e9 del\:00ed v\:00fd\:0161ku v pomere 2 : 1. Preto \:03c1 = v / 3, \:010di\:017ee v = 3\:03c1 = 3 \:00b7 `1` = `2` cm.",
      "2. Zo vzorca v = a\:221a3 / 2 vyjadr\:00edme stranu: a = 2v / \:221a3 = 2 \:00b7 `2` / \:221a3 \:2248 `3` cm.",
      "3. Obsah: S = a \:00b7 v / 2 = v\:00b2 / \:221a3 = `2`\:00b2 / \:221a3 = 3\:221a3 \:00b7 \:03c1\:00b2 = 3\:221a3 \:00b7 `1`\:00b2 \:2248 `4` cm\:00b2.",
      "Odpove\:010f: obsah trojuholn\:00edka je pribli\:017ene `4` cm\:00b2."},
     "\n"],
    cislo[rho], cislo[v], cislo[N[a]], cislo[N[obs]]];

  an = N[a];
  hn = N[Sqrt[3]/2 a];
  cen = N[{a/2, Sqrt[3]/6 a}];
  extra = {
    {Opacity[0.3, Yellow], Disk[cen, N[rho]]},
    {Black, Circle[cen, N[rho]]},
    {Black, Dashed, Line[{{an/2, 0}, {an/2, hn}}]},
    {Red, Thick, Line[{cen, {an/2, 0}}]},
    {Black, PointSize[Large], Point[cen]},
    Text[Style["O", Bold, 14], cen, {1.6, 0}],
    Text[Style[Row[{"\:03c1 = ", cislo[rho]}], 12, Red, Background -> White],
     {an/2, cen[[2]]/2}, {1.3, 0}],
    Text[Style[Row[{"v = ", cislo[v]}], 12, Background -> White],
     {an/2, 0.8 hn}, {-1.3, 0}]};

  ob = vykresliTrojuholnik[an, extra, "a \:2248 " <> cislo[an] <> " cm"];

  {zad, ries, ob}
]
End[]
EndPackage[]
