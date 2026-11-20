(* ::Package:: *)

BeginPackage["EuklidEasy`"]
Begin["`Private`"]

popisVrcholov[p_, mena_] := MapThread[
  Text[Style[#1, Bold, 14], Offset[14 Normalize[N[#2 - Mean[p]]], #2]] &,
  {mena, p}]
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
(* Sqrt[x] pre kladne racionalne x zapisane ako k\[Sqrt]m/d *)
koren[x_] := Module[{n, dn, f, k, m, cc, cn, cd},
  n = Numerator[x]; dn = Denominator[x];
  f = FactorInteger[n dn];
  k = Times @@ (#[[1]]^Quotient[#[[2]], 2] & /@ f);
  m = Times @@ (#[[1]]^Mod[#[[2]], 2] & /@ f);
  cc = k/dn; cn = Numerator[cc]; cd = Denominator[cc];
  If[m == 1,
   zlomok[cc],
   If[cn == 1, "", ToString[cn]] <> "\[Sqrt]" <> ToString[m] <>
    If[cd > 1, "/" <> ToString[cd], ""]]]
korenN[x_] := Module[{kk = koren[x]},
  If[StringContainsQ[kk, "\[Sqrt]"], kk <> " \[TildeTilde] " <> cislo[N[Sqrt[x]]], kk]]
oznac[meno_, sub_, hodn_] := Row[{Subscript[meno, sub], " = ", ToString[hodn]}]
(* pravouhly trojuholnik ABC (pravy uhol pri C), vyska CP na preponu AB *)
vykresliTrojuholnik[ca_, cb_, v_, lca_, lcb_, lv_, la_, lb_, lc_] := Module[
  {pA, pB, pC, pP, m, u1, u2},
  pA = N[{0, 0}]; pB = N[{ca + cb, 0}]; pP = N[{cb, 0}]; pC = N[{cb, v}];
  m = N[Min[ca, cb, v]/5];
  u1 = Normalize[pA - pC]; u2 = Normalize[pB - pC];
  Graphics[{
    LightBlue, EdgeForm[{Thick, Black}], Polygon[{pA, pB, pC}],
    {Black, Dashed, Line[{pC, pP}]},
    {Black, Line[{pP + {m, 0}, pP + {m, m}, pP + {0, m}}]},
    {Black, Line[{pC + m u1, pC + m (u1 + u2), pC + m u2}]},
    Black, PointSize[Large], Point[{pA, pB, pC, pP}],
    popisVrcholov[{pA, pB, pC}, {"A", "B", "C"}],
    Text[Style["P", Bold, 14], pP, {0, 1.6}],
    Text[Style[lcb, 12, Background -> White], (pA + pP)/2, {0, 3.2}],
    Text[Style[lca, 12, Background -> White], (pP + pB)/2, {0, 3.2}],
    Text[Style[lv, 12, Background -> White], (pC + pP)/2, {1.3, 0}],
  (*
    If[la =!= "",
     Text[Style[la, 12, Background -> White], (pB + pC)/2, {-1.2, -1.2}], {}],
    If[lb =!= "",
     Text[Style[lb, 12, Background -> White], (pA + pC)/2, {1.2, -1.2}], {}],
     *)
    If[lc =!= "",
     Text[Style[lc, 12, Background -> White], (pA + pB)/2, {0, 6}], {}]},
  
   Frame -> True,
   PlotRange -> All,
   AspectRatio -> Automatic,
   PlotRangePadding -> Scaled[0.15],
   ImageSize -> 350]
  ]
euklidEasy[] := Module[
  {pary, m, n, k, ca, cb, v, zad, ries, ob},

  (* c_a = k m^2, c_b = k n^2, v_c = k m n  =>  v_c^2 = c_a c_b *)
  pary = Select[Tuples[Range[1, 4], 2], (#[[1]] != #[[2]] && GCD @@ # == 1) &];
  {m, n} = RandomChoice[pary];
  k = RandomInteger[{1, 5}];
  ca = k m^2;
  cb = k n^2;
  v = k m n;

  zad = StringForm[
    "Vypo\[CHacek]\[IAcute]tajte c_a v pravouhlom trojuholn\[IAcute]ku s preponou c, ak c_b = `1` cm a v\[YAcute]\[SHacek]ka na preponu je v_c = `2` cm.",
    cb, v];

  ries = StringForm[
    StringRiffle[{
      "Rie\[SHacek]enie",
      "1. V\[YAcute]\[SHacek]ka na preponu del\[IAcute] pravouhl\[YAcute] trojuholn\[IAcute]k na dva podobn\[EAcute] pravouhl\[EAcute] trojuholn\[IAcute]ky. Plat\[IAcute] Euklidova veta o v\[YAcute]\[SHacek]ke: v_c\.b2 = c_a \[CenterDot] c_b.",
      "2. Vyjadr\[IAcute]me c_a: c_a = v_c\.b2 / c_b = `1`\.b2 / `2` = `3` / `2` = `4` cm.",
      "Odpove\[DHacek]: c_a = `4` cm."},
     "\n"],
    v, cb, v^2, ca];

  ob = vykresliTrojuholnik[ca, cb, v,
    oznac["c", "a", ca], oznac["c", "b", cb], oznac["v", "c", v],
    "a = " <> korenN[(ca + cb) ca], "b = " <> korenN[(ca + cb) cb],
    "c = " <> ToString[ca + cb]];

  {zad, ries, ob}
]
End[]
EndPackage[]

EuklidEasy`Private`euklidEasy[]



