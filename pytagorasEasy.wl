(* ::Package:: *)

BeginPackage["PytagorasEasy`"]

Begin["`Private`"]

(* pomocn\[EAcute] funkcie *)
(* text \[CHacek]\[IAcute]sla: 3 Sqrt[2] -> 3\[Sqrt]2 *)
txt[x_] := StringReplace[ToString[x, InputForm],
  {RegularExpression["Sqrt\\[(\\d+)\\]"] -> "\[Sqrt]$1", "*" -> ""}]

popisVrcholov[p_, mena_] := MapThread[
  Text[Style[#1, Bold, 14], Offset[14 Normalize[N[#2 - Mean[p]]], #2]] &,
  {mena, p}]

(* gener\[AAcute]tor troj\[IAcute]c: po\[CHacek]\[IAcute]ta sa raz pri na\[CHacek]\[IAcute]tan\[IAcute] *)
strany = Range[1, 50];

(* a < b < c, preto s\[UAcute] odvesny v\[ZHacek]dy r\[OHat]zne *)
trojice = Select[
   Subsets[strany, {3}],
   #[[1]]^2 + #[[2]]^2 == #[[3]]^2 &];

(* odvesna -> zoznam prep\[OHat]n, ktor\[EAcute] k nej patria *)
prepony = Association[
   Table[
    h -> Union[Cases[trojice, {x_, y_, z_} /; x == h || y == h :> z]],
    {h, Union[Flatten[trojice[[All, {1, 2}]]]]}]];

(* polovica tetivy AB: odvesna patriaca aspo\[NHacek] dvom trojiciam, nie pr\[IAcute]li\[SHacek] mal\[AAcute] *)
polovice = Select[Keys[prepony], Length[prepony[#]] >= 2 && # >= 5 &];


pytagorasEasy[] := Module[
  {h, c1, c2, r1, r2, d1, d2, pA, pB, pM, pS1, pS2, m, zad, ries, ob},

  h = RandomChoice[polovice];                (* |AB| / 2 *)
  {c1, c2} = RandomSample[prepony[h], 2];    (* dva r\[OHat]zne polomery *)
  {r1, r2} = RandomSample[{c1, c2}];         (* ktor\[AAcute] je k a ktor\[AAcute] m *)
  d1 = Sqrt[r1^2 - h^2];
  d2 = Sqrt[r2^2 - h^2];

  zad = StringForm[
    "S\[UAcute] dan\[EAcute] kru\[ZHacek]nice k a m, ktor\[EAcute] sa pret\[IAcute]naj\[UAcute] v bodoch A a B. Kru\[ZHacek]nica k m\[AAcute] polomer `1` cm a kru\[ZHacek]nica m m\[AAcute] polomer `2` cm. Ur\[CHacek]te v cm vzdialenos\[THacek] stredov t\[YAcute]chto kru\[ZHacek]n\[IAcute]c, ak |AB| = `3` cm.",
    txt[r1], txt[r2], txt[2 h]];

  ries = StringForm[
    StringRiffle[{
      "Rie\[SHacek]enie",
      "1. Spolo\[CHacek]n\[AAcute] tetiva AB je kolm\[AAcute] na spojnicu stredov S1S2 a jej stred M le\[ZHacek]\[IAcute] na tejto spojnici. Preto |AM| = |AB| / 2 = `1` cm.",
      "2. Pravouhl\[YAcute] trojuholn\[IAcute]k AMS1 m\[AAcute] preponu |AS1| = `2` cm a odvesnu |AM| = `1` cm, tak\[ZHacek]e |MS1| = \[Sqrt](`2`\:00b2 \:2212 `1`\:00b2) = \[Sqrt]`3` = `4` cm.",
      "3. Pravouhl\[YAcute] trojuholn\[IAcute]k AMS2 m\[AAcute] preponu |AS2| = `5` cm a odvesnu |AM| = `1` cm, tak\[ZHacek]e |MS2| = \[Sqrt](`5`\:00b2 \:2212 `1`\:00b2) = \[Sqrt]`6` = `7` cm.",
      "4. Ak stredy le\[ZHacek]ia na opa\[CHacek]n\[YAcute]ch stran\[AAcute]ch priamky AB, je |S1S2| = |MS1| + |MS2| = `4` + `7` = `8` cm.",
      "5. Ak stredy le\[ZHacek]ia na tej istej strane priamky AB, je |S1S2| = ||MS1| \:2212 |MS2|| = `9` cm.",
      "Odpove\[DHacek]: vzdialenos\[THacek] stredov je `8` cm (stredy na opa\[CHacek]n\[YAcute]ch stran\[AAcute]ch AB), resp. `9` cm (stredy na tej istej strane AB)."},
     "\n"],
    txt[h], txt[r1], txt[r1^2 - h^2], txt[d1], txt[r2], txt[r2^2 - h^2],
    txt[d2], txt[d1 + d2], txt[Abs[d1 - d2]]];

  (* spolo\[CHacek]n\[AAcute] tetiva AB zvislo, stredy na osi x *)
  pA = {0, h};
  pB = {0, -h};
  pM = {0, 0};
  pS1 = {-d1, 0};
  pS2 = {d2, 0};
  m = Min[h, d1, d2]/4;

  ob = Graphics[{
    {Opacity[0.12, Blue], Disk[pS1, r1]},
    {Opacity[0.12, Red], Disk[pS2, r2]},
    {Blue, Thick, Circle[pS1, r1]},
    {Red, Thick, Circle[pS2, r2]},
    {Black, Thick, Line[{pA, pB}]},
    {Black, Dashed, Line[{pS1, pS2}]},
    {Black, Dashed, Line[{pS1, pA}]},
    {Black, Dashed, Line[{pS2, pA}]},
    {Black, Line[{{m, 0}, {m, m}, {0, m}}]},
    Black, PointSize[Large], Point[{pA, pB, pM, pS1, pS2}],
    popisVrcholov[{pA, pB, pS1, pS2},
     {"A", "B", Subscript["S", 1], Subscript["S", 2]}],
    Text[Style["M", Bold, 14], pM, {1.6, 1.2}],
    Text[Style[txt[2 h] <> " cm", 12, Background -> White], {0, h/2}, {-1.2, 0}],
    Text[Style[txt[r1] <> " cm", 12, Background -> White], (pS1 + pA)/2, {1.2, -1.2}],
    Text[Style[txt[r2] <> " cm", 12, Background -> White], (pS2 + pA)/2, {-1.2, -1.2}]},
   Frame -> True,
   PlotRange -> All,
   AspectRatio -> Automatic,
   PlotRangePadding -> Scaled[0.15],
   ImageSize -> 400];

  {zad, ries, ob}
]

End[]
EndPackage[]

PytagorasEasy`Private`pytagorasEasy[]
