(* ::Package:: *)

BeginPackage["PytagorasHard`"]
Begin["`Private`"]

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
pytagorasHard[] := Module[
  {kand, c, j, u, o, q, s, w, a, b, pA, pB, pC, zad, ries, ob},

  (* c = prepona, a + b = c + 2j, odvesny musia byt realne: (c + 2j)^2 < 2c^2 *)
  kand = Flatten[Table[{cc, jj}, {cc, 5, 20}, {jj, 1, 10}], 1];
  kand = Select[kand, ((#[[1]] + 2 #[[2]])^2 < 2 #[[1]]^2) &];
  {c, j} = RandomChoice[kand];

  u = c + 2 j;
  o = u + c;
  q = 2 c^2;
  s = j (c + j);

  w = Sqrt[2 c^2 - u^2];
  a = N[(u + w)/2];
  b = N[(u - w)/2];

  zad = StringForm[
    "Obvod pravouhl\[EAcute]ho trojuholn\[IAcute]ka je `1` cm. S\[UAcute]\[CHacek]et obsahov \[SHacek]tvorcov zostrojen\[YAcute]ch nad jeho tromi stranami je `2` cm\.b2. Ak\[YAcute] je obsah tohto trojuholn\[IAcute]ka?",
    o, q];

  ries = StringForm[
    StringRiffle[{
      "Rie\[SHacek]enie",
      "1. Ozna\[CHacek]me odvesny a, b a preponu c. S\[UAcute]\[CHacek]et obsahov \[SHacek]tvorcov nad stranami je a\.b2 + b\.b2 + c\.b2 = `1` cm\.b2.",
      "2. Pod\:013ea Pytagorovej vety a\.b2 + b\.b2 = c\.b2, preto 2c\.b2 = `1`, c\.b2 = `2` a c = `3` cm.",
      "3. Z obvodu: a + b = `4` - `3` = `5` cm.",
      "4. Umocn\[IAcute]me: (a + b)\.b2 = a\.b2 + b\.b2 + 2ab = c\.b2 + 2ab, teda 2ab = `5`\.b2 - `2` = `6` a ab = `7`.",
      "5. Obsah: S = ab / 2 = `7` / 2 = `8` cm\.b2.",
      "Odpove\[DHacek]: obsah trojuholn\[IAcute]ka je `8` cm\.b2."},
     "\n"],
    q, c^2, c, o, u, u^2 - c^2, (u^2 - c^2)/2, s];

  pC = {0, 0};
  pB = {a, 0};
  pA = {0, b};

  ob = Graphics[{
    {Opacity[0.3, Blue], EdgeForm[Black], Polygon[{{0, 0}, {a, 0}, {a, -a}, {0, -a}}]},
    {Opacity[0.3, Red], EdgeForm[Black], Polygon[{{0, 0}, {0, b}, {-b, b}, {-b, 0}}]},
    {Opacity[0.3, Green], EdgeForm[Black], Polygon[{pA, pB, pB + {b, a}, pA + {b, a}}]},
    {LightBlue, EdgeForm[{Thick, Black}], Polygon[{pA, pB, pC}]},
    Black, PointSize[Large], Point[{pA, pB, pC}],
    popisVrcholov[{pA, pB, pC}, {"A", "B", "C"}],
    Text[Style["a \[TildeTilde] " <> cislo[a], 12, Background -> White], (pB + pC)/2, {0, -1.6}],
    Text[Style["b \[TildeTilde] " <> cislo[b], 12, Background -> White], (pA + pC)/2, {-1.2, 0}],
    Text[Style["c = " <> ToString[c], 12, Background -> White], (pA + pB)/2, {1.2, 1.2}],
    Text[Style["a\.b2 " , 14], {a/2, -a/2}],
    Text[Style["b\.b2 " , 14], {-b/2, b/2}],
    Text[Style["c\.b2 = " <> ToString[c^2], 14], (pA + pB)/2 + {b, a}/2]},
   Frame -> True,
   PlotRange -> All,
   AspectRatio -> Automatic,
   PlotRangePadding -> Scaled[0.05],
   ImageSize -> 350];

  {zad, ries, ob}
]
End[]
EndPackage[]

PytagorasHard`Private`pytagorasHard[][[3]]





