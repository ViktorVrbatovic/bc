(* ::Package:: *)

BeginPackage["TaznicaHard`"]
Begin["`Private`"]

(* cislo s desatinnou ciarkou, d = max. pocet desatinnych miest *)
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
(* rovnoramenny trojuholnik so zakladnou AB dlzky c a vyskou v; extra = dalsie graficke prvky *)
vykresliTrojuholnik[c_, v_, extra_List : {}] := Module[{p},
  p = N[{{-c/2, 0}, {c/2, 0}, {0, v}}];
  Graphics[{
    LightBlue, EdgeForm[{Thick, Black}], Polygon[p],
    Black, PointSize[Large], Point[p],
    popisVrcholov[p, {"A", "B", "C"}],
    extra},
   Frame -> True,
   PlotRange -> All,
   AspectRatio -> Automatic,
   PlotRangePadding -> Scaled[0.2],
   ImageSize -> 300]
  ]

(* {p, q}: p = c/2, q = t_c, rameno a = Sqrt[p^2 + q^2] je cele,
   q je delitelne 3 (|TP| = q/3 je cele), vyska na rameno 2pq/a ma najviac 2 desatinne miesta
   (pri p > q je trojuholnik tupouhly a vyska na rameno lezi mimo, kresli sa ciarkovane predlzenie) *)
dobraHard[{p_, q_}] := With[{r = Sqrt[p^2 + q^2]},
  IntegerQ[r] && Mod[q, 3] == 0 && Mod[100, Denominator[2 p q/r]] == 0]
poolHard = Select[Tuples[{Range[3, 50], Range[3, 75]}], dobraHard];

taznicaHard[] := Module[
  {p, q, c, tc, a, a2, S, va, tp, ct, pF, tF, m, dF1, dF2, extra, zad, ries, ob},

  {p, q} = RandomChoice[poolHard];
  c = 2 p;  tc = q;
  a2 = p^2 + q^2;  a = Sqrt[a2];      (* a = b = rameno *)
  S = c tc/2;
  va = 2 S/a;                          (* vyska z B na rameno AC *)
  tp = tc/3;  ct = 2 tc/3;             (* teziste T leziace na t_c *)

  zad = StringForm[
    "V rovnoramennom trojuholn\[IAcute]ku so z\[AAcute]klad\[NHacek]ou AB = c = `1` cm m\[AAcute] \[THacek]a\[ZHacek]nica t_c d\:013a\[ZHacek]ku `2` cm. Vypo\[CHacek]\[IAcute]tajte d\:013a\[ZHacek]ku ramena a, obsah S trojuholn\[IAcute]ka, v\[YAcute]\[SHacek]ku v_a z vrcholu A na rameno a vzdialenos\[THacek] \[THacek]a\[ZHacek]iska T od z\[AAcute]klad\[NHacek]e AB.",
    c, tc];

  ries = StringForm[
    StringRiffle[{
      "Rie\[SHacek]enie",
      "1. V rovnoramennom trojuholn\[IAcute]ku so z\[AAcute]klad\[NHacek]ou AB je \[THacek]a\[ZHacek]nica t_c z\[AAcute]rove\[NHacek] v\[YAcute]\[SHacek]kou na z\[AAcute]klad\[NHacek]u. Jej p\[ADoubleDot]ta P je stred AB, preto |AP| = c / 2 = `1` cm.",
      "2. Rameno: a = b = \[Sqrt](t_c\.b2 + (c/2)\.b2) = \[Sqrt](`2`\.b2 + `1`\.b2) = \[Sqrt]`3` = `4` cm.",
      "3. Obsah: S = c \[CenterDot] t_c / 2 = `5` \[CenterDot] `2` / 2 = `6` cm\.b2.",
      "4. Obsah vyjadr\[IAcute]me aj pomocou ramena a v\[YAcute]\[SHacek]ky v_a z vrcholu A na rameno BC: S = a \[CenterDot] v_a / 2, odkia\:013e v_a = 2S / a = 2 \[CenterDot] `6` / `4` = `7` cm.",
      Sequence @@ If[p > q, {"trojuholn\[IAcute]k je tupouhl\[YAcute], preto v\[YAcute]\[SHacek]ka v_a le\[ZHacek]\[IAcute] mimo trojuholn\[IAcute]ka, jej p\[ADoubleDot]ta F je na pred\:013a\[ZHacek]en\[IAcute] strany BC."}, {}],
      "5. \[CapitalTHacek]a\[ZHacek]isko T le\[ZHacek]\[IAcute] na \[THacek]a\[ZHacek]nici t_c a del\[IAcute] ju v pomere 2 : 1 (od vrcholu). Vzdialenos\[THacek] T od z\[AAcute]klad\[NHacek]e je |TP| = t_c / 3 = `2` / 3 = `8` cm.",
      "Odpove\[DHacek]: a = `4` cm, S = `6` cm\.b2, v_a = `7` cm, |TP| = `8` cm."},
     "\n"],
    c/2, tc, a2, a, c, S, cislo[va], tp];

  (* A = {-p, 0}, B = {p, 0}, C = {0, q}; vyska v_a z A na priamku BC, pata F;
     ak F lezi mimo useciky BC (tupouhly trojuholnik), dokresli sa ciarkovane predlzenie *)
  tF = ((N[{-p, 0}] - N[{p, 0}]) . (N[{0, q}] - N[{p, 0}]))/((N[{0, q}] - N[{p, 0}]) . (N[{0, q}] - N[{p, 0}]));
  pF = N[{p, 0}] + tF (N[{0, q}] - N[{p, 0}]);
  m = Min[c, tc]/10;
  dF1 = Normalize[N[{-p, 0}] - pF];
  dF2 = Normalize[N[{p, 0}] - pF];

  extra = {
    {Black, Line[{{m, 0}, {m, m}, {0, m}}]},
    {Black, Line[{pF + m dF1, pF + m dF1 + m dF2, pF + m dF2}]},
    If[tF > 1, {Black, Dashed, Thick, Line[{{0, tc}, pF}]}, {}],
    {Red, Thick, Line[{{0, 0}, {0, tc}}]},
    {Blue, Thick, Line[{{-p, 0}, pF}]},
    {Black, PointSize[Large], Point[{{0, 0}, {0, tc/3}, pF}]},
    Text[Style["P", Bold, 14], {0, 0}, {-1.6, -1.2}],
    Text[Style["T", Bold, 14], {0, tc/3}, {-1.6, 0}],
    Text[Style["F", Bold, 14], pF, {-1.6, -0.3}],
    Text[Style[Subscript["t", "c"], 14, Red, Background -> White], {0, 2 tc/3}, {1.3, 0}],
    Text[Style["|TP| = " <> ToString[tp], 12, Background -> White], {0, tc/6}, {1.2, 0}],
    Text[Style[Row[{Subscript["v", "a"], " = " <> cislo[va]}], 12, Blue, Background -> White], (pF + {-p, 0})/2, {-1.2, 0.3}],
    Text[Style["c/2 = " <> ToString[p], 12, Background -> White], {-p/2, 0}, {0, 1.6}],
    Text[Style["c/2 = " <> ToString[p], 12, Background -> White], {p/2, 0}, {0, 1.6}],
    Text[Style["c = " <> ToString[c], 12, Background -> White], {0, 0}, {0, 3.4}],
    Text[Style["a = " <> ToString[a], 12, Background -> White], {-p/2, tc/2}, {1.3, 0}],
    Text[Style["b = " <> ToString[a], 12, Background -> White], {p/2, tc/2}, {-1.3, 0}]
    };

  ob = vykresliTrojuholnik[c, tc, extra];

  {zad, ries, ob}
]
End[]
EndPackage[]

TaznicaHard`Private`taznicaHard[]

