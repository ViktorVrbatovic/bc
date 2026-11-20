(* ::Package:: *)

BeginPackage["NerovnostHard`"]
Begin["`Private`"]

vykresliTrojuholnik[a_, b_, c_, x0_] :=
 With[{p = First[SSSTriangle[a, b, c]]},
  Graphics[{
    LightBlue, EdgeForm[{Thick, Black}], Triangle[p],
    Black, PointSize[Large], Point[p],
    MapThread[
     Text[Style[#1, Bold, 14], Offset[14 Normalize[N[#2 - Mean[p]]], #2]] &,
     {{"A", "B", "C"}, p}]},
   Frame -> True,
   PlotRange -> All,
   AspectRatio -> Automatic,
   PlotRangePadding -> Scaled[0.15],
   PlotLabel -> Style["napr. x = " <> ToString[x0], 14],
   ImageSize -> 300]
  ]

koef[k_] := Which[k == 1, "x", k == -1, "-x", True, ToString[k] <> "x"]

zlomok[q_] := If[IntegerQ[q], ToString[q],
  ToString[Numerator[q]] <> "/" <> ToString[Denominator[q]]]

vyraz[k_, c_] := StringJoin[
  koef[k],
  If[c > 0, " + " <> ToString[c], If[c < 0, " - " <> ToString[-c], ""]]]

(* vyriesenie jednej nerovnosti tvaru k x > r *)
riadok[{k_, r_}] := StringJoin[
  koef[k], " > ", ToString[r], "   =>   x ", If[k > 0, "> ", "< "], zlomok[r/k],
  If[k < 0, "   (del\:00edme z\:00e1porn\:00fdm \:010d\:00edslom, znak sa obr\:00e1ti)", ""]]

nerovnostHard[] := Module[
  {zak, a1, a2, b1, b2, nerovnosti, hranice, dolna, horna, pocet, x0,
   v1, v2, r1, r2, r3, zad, ries, ob},

  While[True,
   zak = RandomInteger[{1, 15}];
   a1 = RandomInteger[{1, 3}];
   a2 = RandomInteger[{1, 3}];
   b1 = RandomInteger[{0, 4}];
   b2 = RandomInteger[{0, 4}];
   If[a1 == a2, Continue[]];

   (* tri trojuholnikove nerovnosti v tvare k x > r *)
   nerovnosti = {
     {a1 + a2, zak - b1 - b2},
     {a1 - a2, b2 - b1 - zak},
     {a2 - a1, b1 - b2 - zak}};
   hranice = #[[2]]/#[[1]] & /@ nerovnosti;
   dolna = Max[Pick[hranice, Positive[nerovnosti[[All, 1]]]]];
   horna = Min[Pick[hranice, Negative[nerovnosti[[All, 1]]]]];

   If[IntegerQ[dolna] && IntegerQ[horna] && dolna >= 0 && horna - dolna >= 2, Break[]]
  ];

  pocet = horna - dolna - 1;
  x0 = RandomInteger[{dolna + 1, horna - 1}];
  v1 = vyraz[a1, b1];
  v2 = vyraz[a2, b2];
  {r1, r2, r3} = riadok /@ nerovnosti;

  zad = StringForm[
    "Pre ktor\:00e9 hodnoty x m\:00f4\:017eu by\:0165 \:010d\:00edsla `1`, `2` a `3` d\:013a\:017ekami str\:00e1n trojuholn\:00edka? Ko\:013eko cel\:00fdch \:010d\:00edsel x vyhovuje?",
    v1, v2, zak];

  ries = StringForm[
    StringRiffle[{
      "Rie\:0161enie",
      "1. Pou\:017eijeme trojuholn\:00edkov\:00e9 nerovnosti pre strany `1`, `2`, `3`:",
      "\t(`1`) + (`2`) > `3`",
      "\t(`1`) + `3` > (`2`)",
      "\t(`2`) + `3` > (`1`)",
      "2. Ka\:017ed\:00fa nerovnos\:0165 uprav\:00edme na tvar k\:00b7x > r a vyrie\:0161ime:",
      "\t`4`",
      "\t`5`",
      "\t`6`",
      "3. Spolo\:010dn\:00e9 rie\:0161enie (v\:0161etky tri podmienky naraz): `7` < x < `8`",
      "4. Cel\:00e9 \:010d\:00edsla x s\:00fa od `9` po `10`.",
      "5. Po\:010det cel\:00fdch \:010d\:00edsel: `8` - `7` - 1 = `11`.",
      "Odpove\:010f: x \:2208 (`7`, `8`), vyhovuje `11` cel\:00fdch \:010d\:00edsel."},
     "\n"],
    v1, v2, zak, r1, r2, r3, dolna, horna, dolna + 1, horna - 1, pocet];

  ob = vykresliTrojuholnik[a1 x0 + b1, a2 x0 + b2, zak, x0];

  {zad, ries, ob}
]
End[]
EndPackage[]
