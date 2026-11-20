(* ::Package:: *)

BeginPackage["TaznicaEasy`"]
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

(* dvojice {a, y}: a, y, w tvoria pytagorejsku trojicu, takze t_b = w je cele cislo *)
poolEasy = Select[Tuples[{Range[3, 30], Range[3, 30]}],
  IntegerQ[Sqrt[#[[1]]^2 + #[[2]]^2]] && Sqrt[#[[1]]^2 + #[[2]]^2] <= 30 &];

taznicaEasy[] := Module[
  {a, y, b, tb2, tb, S, pA, pB, pC, mB, m, zad, ries, ob},

  {a, y} = RandomChoice[poolEasy];
  b = 2 y;               (* b je parne, takze stred AC ma celociselnu polohu *)
  tb2 = a^2 + y^2;
  tb = Sqrt[tb2];
  S = a b/2;

  zad = StringForm[
    "V pravouhlom trojuholn\[IAcute]ku s preponou c s\[UAcute] d\:013a\[ZHacek]ky odvesien a = `1` cm a b = `2` cm. Vypo\[CHacek]\[IAcute]tajte \[THacek]a\[ZHacek]nicu t_b a obsah S trojuholn\[IAcute]ka.",
    a, b];

  ries = StringForm[
    StringRiffle[{
      "Rie\[SHacek]enie",
      "1. Prav\[YAcute] uhol je pri vrchole C. \[CapitalTHacek]a\[ZHacek]nica t_b sp\[AAcute]ja vrchol B so stredom D strany AC, preto |CD| = b / 2 = `1` cm.",
      "2. Trojuholn\[IAcute]k BCD je pravouhl\[YAcute] s odvesnami a a b/2: t_b\.b2 = a\.b2 + (b/2)\.b2 = `2`\.b2 + `1`\.b2 = `3`.",
      "3. t_b = \[Sqrt]`3` = `4` cm.",
      "4. Obsah: S = a \[CenterDot] b / 2 = `2` \[CenterDot] `5` / 2 = `6` cm\.b2.",
      "Odpove\[DHacek]: \[THacek]a\[ZHacek]nica t_b m\[AAcute] `4` cm, obsah S = `6` cm\.b2."},
     "\n"],
    y, a, tb2, tb, b, S];

  pC = {0, 0};
  pB = {a, 0};
  pA = {0, b};
  mB = (pA + pC)/2;
  m = Min[a, b]/8;

  ob = Graphics[{
    LightBlue, EdgeForm[{Thick, Black}], Polygon[{pA, pB, pC}],
    {Black, Line[{{m, 0}, {m, m}, {0, m}}]},
    {Blue, Thick, Line[{pB, mB}]},
    Black, PointSize[Large], Point[{pA, pB, pC, mB}],
    popisVrcholov[{pA, pB, pC}, {"A", "B", "C"}],
    Text[Style["D", Bold, 14], mB, {1.6, 0}],
    Text[Style[Row[{Subscript["t", "b"], " = " <> ToString[tb]}], 13, Blue, Background -> White], (pB + mB)/2],
    Text[Style["a = " <> ToString[a], 12, Background -> White], (pB + pC)/2, {0, 1.6}],
    Text[Style["b = " <> ToString[b], 12, Background -> White], mB, {3.4, 0}],
    Text[Style["b/2 = " <> ToString[y], 12, Background -> White], (pC + mB)/2, {1.3, 0}],
    Text[Style["b/2 = " <> ToString[y], 12, Background -> White], (pA + mB)/2, {1.3, 0}]
    },
   Frame -> True,
   PlotRange -> All,
   AspectRatio -> Automatic,
   PlotRangePadding -> Scaled[0.2],
   ImageSize -> 320];

  {zad, ries, ob}
]
End[]
EndPackage[]

TaznicaEasy`Private`taznicaEasy[]


