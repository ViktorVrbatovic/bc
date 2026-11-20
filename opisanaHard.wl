(* ::Package:: *)


BeginPackage["OpisanaHard`"]


Begin["`Private`"]

(* pomocn\[EAcute] funkcie *)
zlomok[q_] := If[IntegerQ[q], ToString[q],
  ToString[Numerator[q]] <> "/" <> ToString[Denominator[q]]]

(* \[CHacek]\[IAcute]slo s desatinnou \[CHacek]iarkou, d = po\[CHacek]et desatinn\[YAcute]ch miest *)
cislo[x_, d_: 2] := If[IntegerQ[x], ToString[x],
  StringReplace[
   StringReplace[
    StringReplace[ToString[NumberForm[N[x], {15, d}]],
     (("0" ..) ~~ EndOfString) -> ""],
    ("." ~~ EndOfString) -> ""],
   "." -> ","]]

(* text \[CHacek]\[IAcute]sla: 3 Sqrt[2] -> 3\[Sqrt]2 *)
txt[x_] := StringReplace[ToString[x, InputForm],
  {RegularExpression["Sqrt\\[(\\d+)\\]"] -> "\[Sqrt]$1", "*" -> ""}]

hodnota[q_, d_: 2] := Which[
  IntegerQ[q], ToString[q],
  Head[q] === Rational && Divisible[10^d, Denominator[q]],
   zlomok[q] <> " = " <> cislo[q, d],
  Head[q] === Rational, zlomok[q] <> " \[TildeTilde] " <> cislo[q, d],
  True, txt[q] <> " \[TildeTilde] " <> cislo[q, d]]

popisVrcholov[p_, mena_] := MapThread[
  Text[Style[#1, Bold, 14], Offset[14 Normalize[N[#2 - Mean[p]]], #2]] &,
  {mena, p}]

(* rovnoramenn\[YAcute] trojuholn\[IAcute]k so z\[AAcute]klad\[NHacek]ou AB d\:013a\[ZHacek]ky c a v\[YAcute]\[SHacek]kou v;
   extra = \[DHacek]al\[SHacek]ie grafick\[EAcute] prvky *)
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
   PlotRangePadding -> Scaled[0.15],
   ImageSize -> 300]
]

(* gener\[AAcute]tor troj\[IAcute]c: po\[CHacek]\[IAcute]ta sa raz pri na\[CHacek]\[IAcute]tan\[IAcute]*)
cele = Range[1, 15];
odmocniny = Flatten[Table[x Sqrt[y], {x, 1, 4}, {y, 2, 9}]];
zlomky = {1/2, 1/3, 2/3, 1/4, 3/4, 2/5, 3/5, 4/5, 3/2, 4/3, 5/3, 5/2};

strany = DeleteDuplicates[Join[cele, odmocniny, zlomky]];
strany = SortBy[Select[strany, N[#] <= 15 &], N];

(* a < b < c, preto s\[UAcute] odvesny v\[ZHacek]dy r\[OHat]zne *)
trojice = Select[
   Subsets[strany, {3}],
   PossibleZeroQ[#[[1]]^2 + #[[2]]^2 - #[[3]]^2] &];
(* filter na pekn\[EAcute] trojuholn\[IAcute]ky *)
trojice = Select[trojice,
   N[#[[1]]/#[[2]]] >= 0.4 &&      (* nie pr\[IAcute]li\[SHacek] ploch\[YAcute]/\[UAcute]zky *)
   N[#[[1]]] >= 2 &&               (* nie pr\[IAcute]li\[SHacek] mal\[YAcute] *)
   (IntegerQ[#[[1]]] || IntegerQ[#[[2]]]) &   (* aspo\[NHacek] jedna odvesna cel\[AAcute] *)
];

opisanaHard[] := Module[
  {a, b, x, h, l, c, obs, rr, cen, extra, zad, ries, ob},

  (* (x, h, l) je pytagorejsk\[AAcute] trojica; x = c/2, h = v\[YAcute]\[SHacek]ka, l = rameno *)
  {a, b, l} = RandomChoice[trojice];
  {x, h} = RandomSample[{a, b}];   (* n\[AAcute]hodne vysok\[YAcute] alebo \[SHacek]irok\[YAcute] trojuholn\[IAcute]k *)
  c = 2 x;
  obs = x h;
  rr = l^2/(2 h);                  (* R = abc / (4S) *)

  zad = StringForm[
    "Rovnoramenn\[YAcute] trojuholn\[IAcute]k ABC m\[AAcute] ramen\[AAcute] dlh\[EAcute] `1` cm a z\[AAcute]klad\[NHacek]u `2` cm. Vypo\[CHacek]\[IAcute]tajte polomer op\[IAcute]sanej kru\[ZHacek]nice.",
    txt[l], txt[c]];

  ries = StringForm[
    StringRiffle[{
      "Rie\[SHacek]enie",
      "1. V\[YAcute]\[SHacek]ka na z\[AAcute]klad\[NHacek]u ju rozpol\[IAcute]: v = \[Sqrt](`1`\.b2 - (`2`/2)\.b2) = \[Sqrt](`3` - `4`) = `5` cm.",
      "2. Obsah: S = c \[CenterDot] v / 2 = `2` \[CenterDot] `5` / 2 = `6` cm\.b2.",
      "3. Polomer op\[IAcute]sanej kru\[ZHacek]nice: R = a \[CenterDot] b \[CenterDot] c / (4S) = `1` \[CenterDot] `1` \[CenterDot] `2` / (4 \[CenterDot] `6`) = `7` / `8` = `9` cm.",
      "Odpove\[DHacek]: polomer op\[IAcute]sanej kru\[ZHacek]nice je `9` cm."},
     "\n"],
    txt[l], txt[c], txt[l^2], txt[x^2], txt[h], txt[obs],
    txt[l^2 c], txt[4 obs], hodnota[rr, 3]];

  cen = N[{0, h - rr}];
  extra = {
    {Opacity[0.1, Yellow], Disk[cen, N[rr]]},
    {Gray, Circle[cen, N[rr]]},
    {Red, Dashed, Line[{cen, N[{-x, 0}]}]},
    {Black, PointSize[Large], Point[cen]},
    Text[Style["O", Bold, 14], cen, {-1.6, 0}],
    Text[Style["R", 12, Red, Background -> White], (cen + N[{-x, 0}])/2],
    Text[Style["c = " <> txt[c], 12, Background -> White], {0, 0}, {0, 1.6}],
    Text[Style["a = " <> txt[l], 12, Background -> White], N[{-x/2, h/2}], {1.2, -1.2}],
    Text[Style["b = " <> txt[l], 12, Background -> White], N[{x/2, h/2}], {-1.2, -1.2}]};

  ob = vykresliTrojuholnik[c, h, extra];

  {zad, ries, ob}
]

End[]
EndPackage[]

OpisanaHard`Private`opisanaHard[]





