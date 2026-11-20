(* ::Package:: *)

BeginPackage["RovnoRamennyNormal`"]
Begin["`Private`"]

(* rovnoramenny trojuholnik so zakladnou AB dlzky c a vyskou v; extra = dalsie grafick\:00e9 prvky *)
vykresliTrojuholnik[c_, v_, extra_List : {}] := Module[{p, st},
  p = N[{{-c/2, 0}, {c/2, 0}, {0, v}}];
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

rovnoRamennyNormal[] := Module[
  {pary, r, x, y, ms, c, v, extra, zad, ries, ob},

  pary = Select[Tuples[Range[2, 7], 2], #[[1]] < #[[2]] && GCD @@ # == 1 &];

  While[True,
   r = RandomInteger[{4, 15}];
   {x, y} = RandomChoice[pary];
   If[Mod[r, y] == 0, Break[]]
  ];

  ms = r/y x;
  v = ms + r;
  (* strana c je len na vykreslenie: (c/2)^2 = r^2 - ms^2 *)
  c = 2 Sqrt[r^2 - ms^2];

  (* M = {0, 0} je stred AB, S = {0, ms} je stred opisanej kruznice *)
  extra = {
    {Gray, Dashed, Circle[{0, N[ms]}, r]},
    {Red, Thick, Line[{{0, 0}, {0, N[v]}}]},
    {Black, PointSize[Large], Point[{{0, 0}, {0, N[ms]}}]},
    Text[Style["M", Bold, 14], {0, 0}, {0, 1.8}],
    Text[Style["S", Bold, 14], {0, N[ms]}, {-1.8, 0}]};

  zad = StringForm[
    "Je dan\:00fd ostrouholn\:00fd rovnoramenn\:00fd trojuholn\:00edk ABC so z\:00e1klad\:0148ou AB. Polomer jeho op\:00edsanej kru\:017enice so stredom S je `1` cm a pre bod M, ktor\:00fd je stredom z\:00e1kladne AB, plat\:00ed |MS| : |SC| = `2` : `3`. Ko\:013eko cm m\:00e1 \:00fase\:010dka MC?",
    r, x, y];

  ries = StringForm[
    StringRiffle[{
      "Rie\:0161enie",
      "1. Bod C le\:017e\:00ed na op\:00edsanej kru\:017enici, preto |SC| = r = `1` cm.",
      "2. Z pomeru |MS| : |SC| = `2` : `3` dostaneme |MS| = |SC| \:00b7 `2`/`3` = `1` \:00b7 `2`/`3` = `4` cm.",
      "3. Trojuholn\:00edk je ostrouholn\:00fd, preto stred op\:00edsanej kru\:017enice S le\:017e\:00ed vn\:00fatri trojuholn\:00edka, teda na \:00fase\:010dke MC medzi bodmi M a C.",
      "4. |MC| = |MS| + |SC| = `4` + `1` = `5` cm.",
      "Odpove\:010f: \:00fase\:010dka MC m\:00e1 `5` cm."},
     "\n"],
    r, x, y, ms, v];

  ob = vykresliTrojuholnik[N[c], N[v], extra];

  {zad, ries, ob}
]
End[]
EndPackage[]
