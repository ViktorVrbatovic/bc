(* ::Package:: *)

BeginPackage["VpisanaHard`"]
Begin["`Private`"]

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
popisVrcholov[p_, mena_] := MapThread[
  Text[Style[#1, Bold, 14], Offset[14 Normalize[N[#2 - Mean[p]]], #2]] &,
  {mena, p}]
vpisanaHard[] := Module[
  {t, r, hh, d, disc1, sq1, u, disc2, sq2, q, p, e, f, obs, cs, foot, sa, strany, zad, ries, ob},

  (* uhlopriecky 8t a 6t (pytagorejska trojica 3-4-5), strana 5t, polomer r = 12t/5, rozdiel 2t *)
  t = RandomInteger[{1, 8}];
  r = 12 t/5;
  d = 2 t;
  hh = d/2;
  cs = cislo[#, 4] &;

  disc1 = r^4 + r^2 hh^2;
  sq1 = Sqrt[disc1];
  u = r^2 + sq1;
  disc2 = hh^2 + 4 u;
  sq2 = Sqrt[disc2];
  q = (-hh + sq2)/2;
  p = q + hh;
  e = 2 p;
  f = 2 q;
  obs = e f/2;

  zad = StringForm[
    "Vp\:00edsan\:00e1 kru\:017enica koso\:0161tvorca ABCD m\:00e1 polomer r = `1` cm. Rozdiel d\:013a\:017eok jeho uhloprie\:010dok je `2` cm. Vypo\:010d\:00edtajte d\:013a\:017eky oboch uhloprie\:010dok a obsah koso\:0161tvorca.",
    cs[r], d];

  ries = StringForm[
    StringRiffle[{
      "Rie\:0161enie",
      "1. Uhloprie\:010dky koso\:0161tvorca s\:00fa na seba kolm\:00e9 a navz\:00e1jom sa rozpo\:013euj\:00fa. Ozna\:010dme p > q ich polovice. Rozdiel uhloprie\:010dok je 2(p - q) = `1`, teda p - q = `2`.",
      "2. Strana a = \:221a(p\:00b2 + q\:00b2) je prepona pravouhl\:00e9ho trojuholn\:00edka s odvesnami p, q. Obsah koso\:0161tvorca je S = 2pq a z\:00e1rove\:0148 S = a \:00b7 v = a \:00b7 2r, preto pq = a \:00b7 r, \:010di\:017ee a = pq / r.",
      "3. Ozna\:010dme u = pq. Potom (p - q)\:00b2 = p\:00b2 + q\:00b2 - 2pq = a\:00b2 - 2u = u\:00b2 / r\:00b2 - 2u. Po dosaden\:00ed: `3` = u\:00b2 / `4` - 2u, \:010di\:017ee u\:00b2 - `5`u - `6` = 0.",
      "4. Kladn\:00fd kore\:0148: u = r\:00b2 + \:221a(r\:2074 + r\:00b2 \:00b7 (p - q)\:00b2) = `4` + \:221a(`7`) = `4` + `8` = `9`.",
      "5. Zo vz\:0165ahov p - q = `2` a pq = `9` dostaneme q\:00b2 + `2`q - `9` = 0, odkia\:013e q = (-`2` + \:221a(`2`\:00b2 + 4 \:00b7 `9`)) / 2 = (-`2` + `10`) / 2 = `11` a p = q + `2` = `12`.",
      "6. Uhloprie\:010dky: e = 2p = `13` cm, f = 2q = `14` cm. Obsah: S = e \:00b7 f / 2 = `15` cm\:00b2.",
      "Odpove\:010f: uhloprie\:010dky maj\:00fa `13` cm a `14` cm, obsah koso\:0161tvorca je `15` cm\:00b2."},
     "\n"],
    d, cs[hh], cs[hh^2], cs[r^2], cs[2 r^2], cs[r^2 hh^2], cs[disc1], cs[sq1], cs[u],
    cs[sq2], cs[q], cs[p], cs[e], cs[f], cs[obs]];

  (* A = (-p, 0), B = (0, -q), C = (p, 0), D = (0, q); stred v pociatku *)
  sa = Sqrt[p^2 + q^2];
  strany = N[{{{-p, 0}, {0, -q}}, {{0, -q}, {p, 0}}, {{p, 0}, {0, q}}, {{0, q}, {-p, 0}}}];
  foot = N[{p, 0} + p^2/(p^2 + q^2) ({0, q} - {p, 0})];

  ob = Graphics[{
    LightBlue, EdgeForm[{Thick, Black}], Polygon[N[{{-p, 0}, {0, -q}, {p, 0}, {0, q}}]],
    {Opacity[0.3, Yellow], Disk[{0, 0}, N[r]]},
    {Black, Circle[{0, 0}, N[r]]},
    {Black, Dashed, Line[N[{{-p, 0}, {p, 0}}]], Line[N[{{0, -q}, {0, q}}]]},
    {Red, Thick, Line[{{0, 0}, foot}]},
    Black, PointSize[Large], Point[N[{{-p, 0}, {0, -q}, {p, 0}, {0, q}, {0, 0}}]],
    Text[Style["A", Bold, 14], N[{-p, 0}], {1.6, 0}],
    Text[Style["B", Bold, 14], N[{0, -q}], {0, 1.6}],
    Text[Style["C", Bold, 14], N[{p, 0}], {-1.6, 0}],
    Text[Style["D", Bold, 14], N[{0, q}], {0, -1.6}],
    Text[Style["e = " <> cs[e], 12, Background -> White], N[{-p/2, 0}], {0, 1.6}],
    Text[Style["f = " <> cs[f], 12, Background -> White], N[{0, -q/2}], {-1.3, 0}],
    Text[Style["r", 12, Red, Background -> White], foot/2, {-1.2, -1.2}],
    Table[With[{mid = Mean[strany[[i]]]},
      Text[Style["a = " <> cs[sa], 12, Background -> White], Offset[26 Normalize[mid], mid]]], {i, 4}]},
   Frame -> True,
   PlotRange -> All,
   AspectRatio -> Automatic,
   PlotRangePadding -> Scaled[0.15],
   ImageSize -> 320];

  {zad, ries, ob}
]
End[]
EndPackage[]
