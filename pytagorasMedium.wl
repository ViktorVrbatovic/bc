(* ::Package:: *)

BeginPackage["PytagorasMedium`"]

Begin["`Private`"]

(* pomocn\[EAcute] funkcie *)
(* text \[CHacek]\[IAcute]sla: 3 Sqrt[2] -> 3\[Sqrt]2 *)
txt[x_] := StringReplace[ToString[x, InputForm],
  {RegularExpression["Sqrt\\[(\\d+)\\]"] -> "\[Sqrt]$1", "*" -> ""}]

(* gener\[AAcute]tor mierky: z s\:00b2 + (s+p)\:00b2 = (s+2p)\:00b2 vych\[AAcute]dza s = 3p *)
(* strany s\[UAcute] teda 3p, 4p, 5p; p >= 2, aby nevy\[SHacek]lo trivi\[AAcute]lne 3, 4, 5 *)
mierky = Range[2, 15];
typy = {"dlhsia", "kratsia", "obvod", "obsah"};


pytagorasMedium[] := Module[
  {p, typ, podm, krok, pA, pB, pC, m, zad, ries, ob},

  p = RandomChoice[mierky];
  typ = RandomChoice[typy];

  {podm, krok} = Switch[typ,
    "dlhsia",
     {"ak dlh\[SHacek]ia odvesna meria " <> txt[4 p] <> " cm",
      "4. Pod\:013ea zadania je dlh\[SHacek]ia odvesna s + p = 4p = " <> txt[4 p] <>
       " cm, preto p = " <> txt[p] <> " cm."},
    "kratsia",
     {"ak krat\[SHacek]ia odvesna meria " <> txt[3 p] <> " cm",
      "4. Pod\:013ea zadania je krat\[SHacek]ia odvesna s = 3p = " <> txt[3 p] <>
       " cm, preto p = " <> txt[p] <> " cm."},
    "obvod",
     {"ak obvod trojuholn\[IAcute]ka je " <> txt[12 p] <> " cm",
      "4. Obvod je 3p + 4p + 5p = 12p = " <> txt[12 p] <>
       " cm, preto p = " <> txt[p] <> " cm."},
    "obsah",
     {"ak obsah trojuholn\[IAcute]ka je " <> txt[6 p^2] <> " cm\:00b2",
      "4. Obsah je (3p \[CenterDot] 4p) / 2 = 6p\:00b2 = " <> txt[6 p^2] <>
       " cm\:00b2, preto p\:00b2 = " <> txt[p^2] <> " a p = " <> txt[p] <> " cm."}];

  zad = StringForm[
    "D\:013a\[ZHacek]ky str\[AAcute]n ist\[EAcute]ho pravouhl\[EAcute]ho trojuholn\[IAcute]ka sa daj\[UAcute] zap\[IAcute]sa\[THacek] v tvare s, s + p, s + 2p, kde s, p \:2208 \:211d\:207a. Ak\[AAcute] je d\:013a\[ZHacek]ka jeho prepony, `1`?",
    podm];

  ries = StringForm[
    StringRiffle[{
      "Rie\[SHacek]enie",
      "1. Najdlh\[SHacek]ia strana pravouhl\[EAcute]ho trojuholn\[IAcute]ka je prepona, teda s + 2p. Z Pytagorovej vety plat\[IAcute] s\:00b2 + (s + p)\:00b2 = (s + 2p)\:00b2.",
      "2. \:00daprava: 2s\:00b2 + 2sp + p\:00b2 = s\:00b2 + 4sp + 4p\:00b2, \[CHacek]i\[ZHacek]e s\:00b2 \:2212 2sp \:2212 3p\:00b2 = 0, teda (s \:2212 3p)(s + p) = 0.",
      "3. Ke\[DHacek]\[ZHacek]e s, p > 0, je s = 3p. Strany trojuholn\[IAcute]ka s\[UAcute] 3p, 4p, 5p.",
      "`1`",
      "5. Strany: s = 3p = `2` cm, s + p = 4p = `3` cm, s + 2p = 5p = `4` cm.",
      "Odpove\[DHacek]: prepona meria `4` cm."},
     "\n"],
    krok, txt[3 p], txt[4 p], txt[5 p]];

  (* prav\[YAcute] uhol v po\[CHacek]iatku, dlh\[SHacek]ia odvesna vodorovne *)
  pA = {0, 0};
  pB = {4 p, 0};
  pC = {0, 3 p};
  m = p/2;

  ob = Graphics[{
    LightBlue, EdgeForm[{Thick, Black}], Polygon[{pA, pB, pC}],
    {Black, Line[{{m, 0}, {m, m}, {0, m}}]},
    Black, PointSize[Large], Point[{pA, pB, pC}],
    Text[Style["s + p = " <> txt[4 p] <> " cm", 12, Background -> White], (pA + pB)/2, {0, 1.6}],
    Text[Style["s = " <> txt[3 p] <> " cm", 12, Background -> White], (pA + pC)/2, {1.6, 0}],
    Text[Style["s + 2p = " <> txt[5 p] <> " cm", 12, Background -> White], (pB + pC)/2, {-1.2, -1.2}]},
   Frame -> True,
   PlotRange -> All,
   AspectRatio -> Automatic,
   PlotRangePadding -> Scaled[0.2],
   ImageSize -> 320];

  {zad, ries, ob}
]

End[]
EndPackage[]

PytagorasMedium`Private`pytagorasMedium[]
