(* ::Package:: *)

BeginPackage["RovnoRamennyHard`"]
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

rozdel[x_] := Which[
  Head[x] === Power, {1, x[[1]]},              (* Sqrt[3] -> {1, 3} *)
  Head[x] === Times, {x[[1]], x[[2, 1]]},      (* 2 Sqrt[3] -> {2, 3} *)
  True, {x, 1}                                 (* 24 -> {24, 1} *)
  ]

rovnoRamennyHard[] := Module[
  {c, a, zvysok, strany, olicho, v, s, k, m, str, vn, extra, zad, ries, ob},

  While[True,
   c = RandomChoice[Range[4, 12, 2]];
   a = RandomInteger[{3, 15}];
   strany = {a, a, c};
   zvysok = Total[strany] - Max[strany];
   If[Max[strany] >= zvysok, Continue[]];

   v = Sqrt[a^2 - (c/2)^2];
   s = c v/2;
   {k, m} = rozdel[s];

   If[(k <= 7 && m <= 8) || m == 1, Break[]]
  ];

  str = c/2;
  olicho = c + str + a;
  vn = N[v];

  (* stredna priecka DE: D na AC, E na BC *)
  extra = {
    {Red, Thick, Line[{{-c/4, vn/2}, {c/4, vn/2}}]},
    {Black, PointSize[Large], Point[{{-c/4, vn/2}, {c/4, vn/2}}]},
    Text[Style["D", Bold, 14], {-c/4, vn/2}, {1.6, 0}],
    Text[Style["E", Bold, 14], {c/4, vn/2}, {-1.6, 0}],
    Text[Style[ToString[str] <> " cm", 12], {0, vn/2}, {0, -1.6}]};

  zad = StringForm[
    "Stredn\:00e1 prie\:010dka rovnobe\:017en\:00e1 so z\:00e1klad\:0148ou meria `1` cm a rozde\:013euje rovnoramenn\:00fd trojuholn\:00edk na dve \:010dasti. V\:00e4\:010d\:0161ia z nich m\:00e1 obvod O = `2` cm. Vypo\:010d\:00edtajte obvod a obsah cel\:00e9ho trojuholn\:00edka.",
    str, olicho];

  ries = StringForm[
    StringRiffle[{
      "Rie\:0161enie",
      "1. Stredn\:00e1 prie\:010dka je rovnobe\:017en\:00e1 so z\:00e1klad\:0148ou a m\:00e1 polovi\:010dn\:00fa d\:013a\:017eku, preto z\:00e1klad\:0148a c = 2 \:00b7 `1` = `3` cm.",
      "2. Stredn\:00e1 prie\:010dka sp\:00e1ja stredy ramien, tak\:017ee v\:00e4\:010d\:0161ia \:010das\:0165 je rovnoramenn\:00fd lichobe\:017en\:00edk so z\:00e1klad\:0148ami `3` cm a `1` cm a ramenami a/2 (polovica ramena trojuholn\:00edka).",
      "3. Obvod lichobe\:017en\:00edka: O = `3` + `1` + 2 \:00b7 (a/2) = `3` + `1` + a, teda a = `2` - `3` - `1` = `4` cm.",
      "4. Obvod trojuholn\:00edka: o = 2a + c = 2 \:00b7 `4` + `3` = `5` cm.",
      "5. V\:00fd\:0161ka na z\:00e1klad\:0148u: v = \:221a(a\:00b2 - (c/2)\:00b2) = \:221a(`4`\:00b2 - `1`\:00b2) = \:221a`8` = `6` cm.",
      "6. Obsah: S = c \:00b7 v / 2 = `3` \:00b7 `6` / 2 = `7` cm\:00b2.",
      "Odpove\:010f: obvod je `5` cm, obsah je `7` cm\:00b2."},
     "\n"],
    str, olicho, c, a, 2 a + c, v, s, a^2 - str^2];

  ob = vykresliTrojuholnik[c, vn, extra];

  {zad, ries, ob}
]
End[]
EndPackage[]
