(* ::Package:: *)

BeginPackage["RovnoRamennyEasy`"]
Begin["`Private`"]

(* rovnoramenny trojuholnik so zakladnou AB dlzky c a vyskou v; extra = dalsie grafick\[EAcute] prvky *)
vykresliTrojuholnik[c_, v_, extra_List : {}] := Module[{p, st},
  p = N[{{-c/2, 0}, {c/2, 0}, {0, v}}];
  st = Mean[p];
  Graphics[{
    LightBlue, EdgeForm[{Thick, Black}], Polygon[p],
    Black, PointSize[Large], Point[p],
    MapThread[
     Text[Style[#1, Bold, 14], Offset[18 Normalize[#2 - st], #2]] &,
     {{"A", "B", "C"}, p}],
    extra},
   Frame -> True,
   PlotRange -> All,
   AspectRatio -> Automatic,
   PlotRangePadding -> Scaled[0.15],
   ImageSize -> 300]
  ]

rovnoRamennyEasy[] := Module[
  {x, c, vysledok, pA, pB, pC, u1, u2, m, extra, zad, ries, ob},

  x = RandomInteger[{3, 12}];
  c = N[Sqrt[2] x];
  vysledok = x^2/2;

  (* body len na vykreslenie: S je stred AB, trojuholnik je pravouhly pri C *)
  pA = {-c/2, 0};
  pB = {c/2, 0};
  pC = {0, c/2};
  u1 = Normalize[pA - pC];
  u2 = Normalize[pB - pC];
  m = c/8;
  
  
  extra = {
    {Gray, Dashed, Circle[{0, 0}, c/2]},
    {Black, PointSize[Large], Point[{0, 0}]},
    Text[Style["S", Bold, 14], {0, 0}, {0, 1.8}]};
    
    (*,

    Text[Style[ToString[x] <> " cm", 12], (pA + pC)/2, {1.2, 0}],
    Text[Style[ToString[x] <> " cm", 12], (pB + pC)/2, {-1.2, 0}]*)

  zad = StringForm[
    "Stred op\[IAcute]sanej kru\[ZHacek]nice rovnoramenn\[EAcute]ho trojuholn\[IAcute]ka ABC le\[ZHacek]\[IAcute] v strede jeho z\[AAcute]kladne AB. Rameno AC m\[AAcute] `1` cm. Ko\:013eko cm\.b2 m\[AAcute] obsah tohto trojuholn\[IAcute]ka?",
    x];

  ries = StringForm[
    StringRiffle[{
      "Rie\[SHacek]enie",
      "1. Stred op\[IAcute]sanej kru\[ZHacek]nice le\[ZHacek]\[IAcute] v strede z\[AAcute]kladne AB, tak\[ZHacek]e AB je priemer kru\[ZHacek]nice. Pod\:013ea T\[AAcute]lesovej vety je uhol pri vrchole C prav\[YAcute].",
      "2. Trojuholn\[IAcute]k je rovnoramenn\[YAcute] so z\[AAcute]klad\[NHacek]ou AB, preto AC = BC = `1` cm. Ramen\[AAcute] s\[UAcute] z\[AAcute]rove\[NHacek] odvesny pravouhl\[EAcute]ho trojuholn\[IAcute]ka.",
      "3. Obsah: S = (AC \[CenterDot] BC) / 2 = (`1` \[CenterDot] `1`) / 2 = `2` cm\.b2.",
      "Odpove\[DHacek]: obsah trojuholn\[IAcute]ka je `2` cm\.b2."},
     "\n"],
    x, vysledok];

  ob = vykresliTrojuholnik[c, c/2, extra];

  {zad, ries, ob}
]
End[]
EndPackage[]



