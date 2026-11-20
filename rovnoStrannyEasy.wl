(* ::Package:: *)

BeginPackage["RovnostrannyEasy`"]
Begin["`Private`"]

(* rovnostranny trojuholnik so stranou a; extra = dalsie grafick\[EAcute] prvky *)
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

rovnostrannyEasy[] := Module[
  {a, k, v, o, an, hn, mm, extra, zad, ries, ob},

  a = RandomChoice[{2, 4, 6, 8, 12, 14, 16, 18}];
  k = a/2;
  v = k Sqrt[3];
  o = 3 a;

  an = N[a];
  hn = N[Sqrt[3]/2 a];
  mm = an/12;
  extra = {
    {Black, Dashed, Line[{{an/2, 0}, {an/2, hn}}]},
   
    Text[Style["v", Italic, 14], {an/2, hn/2}, {-1.6, 0}]};

  zad = StringForm[
    "Rovnostrann\[YAcute] trojuholn\[IAcute]k m\[AAcute] v\[YAcute]\[SHacek]ku v = `1` cm. Vypo\[CHacek]\[IAcute]tajte jeho obvod v cm.",
    v];

  ries = StringForm[
    StringRiffle[{
      "Rie\[SHacek]enie",
      "1. V\[YAcute]\[SHacek]ka rovnostrann\[EAcute]ho trojuholn\[IAcute]ka so stranou a je v = a\[Sqrt]3 / 2 (z Pytagorovej vety: v\.b2 = a\.b2 - (a/2)\.b2).",
      "2. Zo zadanej v\[YAcute]\[SHacek]ky vypo\[CHacek]\[IAcute]tame stranu: a = 2v / \[Sqrt]3 = 2 \[CenterDot] `1` / \[Sqrt]3 = `2` cm.",
      "3. Obvod: o = 3a = 3 \[CenterDot] `2` = `3` cm.",
      "Odpove\[DHacek]: obvod trojuholn\[IAcute]ka je `3` cm."},
     "\n"],
    v, a, o];

  ob = vykresliTrojuholnik[a, extra];

  {zad, ries, ob}
]
End[]
EndPackage[]
