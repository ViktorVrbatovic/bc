(* ::Package:: *)

BeginPackage["PomerHard`"]
Begin["`Private`"]

(* trojuholnik podla uhlov (v stupnoch) pri vrcholoch A, B, C *)
vykresliTrojuholnik[uhly_List] := Module[{al, be, ga, b, p, st},
  {al, be, ga} = uhly Degree;
  b = Sin[be]/Sin[ga];
  p = N[{{0, 0}, {1, 0}, {b Cos[al], b Sin[al]}}];
  st = Mean[p];
  Graphics[{
    LightBlue, EdgeForm[{Thick, Black}], Polygon[p],
    Black, PointSize[Large], Point[p],
    MapThread[
     Text[Style[#1, Bold, 14], Offset[14 Normalize[#2 - st], #2]] &,
     {{"A", "B", "C"}, p}],
    MapThread[
     Text[Style[ToString[#1] <> "\:00b0", 12], #2 + 0.3 (st - #2)] &,
     {uhly, p}]},
   Frame -> True,
   PlotRange -> All,
   AspectRatio -> Automatic,
   PlotRangePadding -> Scaled[0.15],
   ImageSize -> 300]
  ]

zlomok[q_] := If[IntegerQ[q], ToString[q],
  ToString[Numerator[q]] <> "/" <> ToString[Denominator[q]]]

koefT[c_] := Which[c == 1, "t", c == -1, "-t", True, ToString[c] <> "t"]

(* vyraz c t + d zapisany ako text *)
vyrazT[c_, d_] := Which[
  d == 0, koefT[c],
  c == 0, ToString[d] <> "\:00b0",
  c > 0, ToString[d] <> "\:00b0 + " <> koefT[c],
  True, ToString[d] <> "\:00b0 - " <> koefT[-c]]

pomerHard[] := Module[
  {uhly, outI, inJ, kI, pomer, p, q, m, n, roz, kt, kc, dk, dc, rr, t, ex,
   najm, vysl, vrch = {"A", "B", "C"}, nazvy = {"\:03b1", "\:03b2", "\:03b3"},
   zad, ries, ob},

  While[True,
   uhly = RandomInteger[{20, 120}, 2];
   uhly = Append[uhly, 180 - Total[uhly]];
   If[Last[uhly] < 10, Continue[]];

   (* vonkajsi uhol pri vrchole outI a vnutorny uhol inJ *)
   {outI, inJ} = RandomSample[{1, 2, 3}, 2];
   pomer = (180 - uhly[[outI]])/uhly[[inJ]];
   {p, q} = {Numerator[pomer], Denominator[pomer]};
   If[pomer == 1 || Max[p, q] > 9, Continue[]];

   (* rozdiel dvoch uhlov - m vacsi ako n *)
   {m, n} = RandomSample[{1, 2, 3}, 2];
   roz = uhly[[m]] - uhly[[n]];
   If[roz <= 0, Continue[]];

   (* kazdy uhol v tvare kt*t + kc, kde t je parameter z pomeru *)
   kI = First[Complement[{1, 2, 3}, {outI, inJ}]];
   kt = ConstantArray[0, 3];
   kc = ConstantArray[0, 3];
   kt[[inJ]] = q;
   kt[[kI]] = p - q;
   kt[[outI]] = -p;
   kc[[outI]] = 180;
   If[kt[[m]] == kt[[n]], Continue[]];

   Break[]
  ];

  dk = kt[[m]] - kt[[n]];
  dc = kc[[m]] - kc[[n]];
  rr = roz - dc;
  t = rr/dk;
  ex = MapThread[vyrazT, {kt, kc}];
  najm = Min[uhly];
  vysl = 180 - najm;

  zad = StringForm[
    "V trojuholn\:00edku ABC je vonkaj\:0161\:00ed uhol pri vrchole `1` v pomere `2`:`3` k vn\:00fatorn\:00e9mu uhlu `4`. Uhol `5` je o `6` stup\:0148ov v\:00e4\:010d\:0161\:00ed ako uhol `7`. Zistite ve\:013ekos\:0165 najv\:00e4\:010d\:0161ieho vonkaj\:0161ieho uhla.",
    vrch[[outI]], p, q, nazvy[[inJ]], nazvy[[m]], roz, nazvy[[n]]];

  ries = StringForm[
    StringRiffle[{
      "Rie\:0161enie",
      "1. Vonkaj\:0161\:00ed uhol pri vrchole `1` sa rovn\:00e1 180\:00b0 - `2`, teda s\:00fa\:010dtu zvy\:0161n\:00fdch dvoch vn\:00fatorn\:00fdch uhlov: `3` + `4`.",
      "2. Z pomeru (`3` + `4`) : `3` = `5` : `6` zvol\:00edme parameter t: `3` = `7`, `3` + `4` = `8`, preto `4` = `9`.",
      "3. Potom `2` = 180\:00b0 - `8`. Uhly pomocou t: \:03b1 = `10`, \:03b2 = `11`, \:03b3 = `12`.",
      "4. Podmienka `13` - `14` = `15`\:00b0: (`16`) - (`17`) = `15`\:00b0, po \:00faprave `18` = `19`\:00b0, teda t = `20`\:00b0.",
      "5. Dosaden\:00edm dostaneme: \:03b1 = `21`\:00b0, \:03b2 = `22`\:00b0, \:03b3 = `23`\:00b0.",
      "6. Vonkaj\:0161\:00ed uhol je najv\:00e4\:010d\:0161\:00ed pri najmen\:0161om vn\:00fatornom uhle (`24`\:00b0): 180\:00b0 - `24`\:00b0 = `25`\:00b0.",
      "Odpove\:010f: najv\:00e4\:010d\:0161\:00ed vonkaj\:0161\:00ed uhol m\:00e1 `25`\:00b0."},
     "\n"],
    vrch[[outI]], nazvy[[outI]], nazvy[[inJ]], nazvy[[kI]], p, q,
    koefT[q], koefT[p], koefT[p - q],
    ex[[1]], ex[[2]], ex[[3]],
    nazvy[[m]], nazvy[[n]], roz, ex[[m]], ex[[n]],
    koefT[dk], rr, zlomok[t],
    uhly[[1]], uhly[[2]], uhly[[3]], najm, vysl];

  ob = vykresliTrojuholnik[uhly];

  {zad, ries, ob}
]
End[]
EndPackage[]
