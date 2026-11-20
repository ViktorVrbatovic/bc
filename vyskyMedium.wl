(* ::Package:: *)

BeginPackage["VyskyMedium`"]
Begin["`Private`"]

(* ---------- pomocn\[EAcute] funkcie ---------- *)
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

(* text \[CHacek]\[IAcute]sla: 3 Sqrt[2] -> 3\[Sqrt]2, (13 Sqrt[2])/2 -> 13\[Sqrt]2/2 *)
txt[x_] := StringReplace[
  StringReplace[ToString[x, InputForm],
   {RegularExpression["Sqrt\\[(\\d+)\\]"] -> "\[Sqrt]$1", "*" -> ""}],
  RegularExpression["\\((\\d*\[Sqrt]\\d+)\\)/(\\d+)"] -> "$1/$2"]

(* z\[AAcute]tvorky pre ka\[ZHacek]d\[EAcute] necelo\[CHacek]\[IAcute]seln\[EAcute] \[CHacek]\[IAcute]slo (mocniny, delenie) *)
zat[x_] := If[IntegerQ[x], txt[x], "(" <> txt[x] <> ")"]

(* z\[AAcute]tvorky len ak je v texte zlomok (s\[UAcute]\[CHacek]iny) *)
zr[x_] := With[{s = txt[x]},
  If[IntegerQ[x] || ! StringContainsQ[s, "/"], s, "(" <> s <> ")"]]

(* \[CHacek]\[IAcute]slo v zadan\[IAcute]: racion\[AAcute]lne ako desatinn\[EAcute] s \[CHacek]iarkou, odmocniny ako text *)
dat[x_] := If[Head[x] === Rational || IntegerQ[x], cislo[x, 2], txt[x]]

hodnota[q_, d_: 2] := Which[
  IntegerQ[q], ToString[q],
  Head[q] === Rational && Divisible[10^d, Denominator[q]],
   zlomok[q] <> " = " <> cislo[q, d],
  Head[q] === Rational, zlomok[q] <> " \[TildeTilde] " <> cislo[q, d],
  True, txt[q] <> " \[TildeTilde] " <> cislo[q, d]]

popisVrcholov[p_, mena_] := MapThread[
  Text[Style[#1, Bold, 14], Offset[14 Normalize[N[#2 - Mean[p]]], #2]] &,
  {mena, p}]

(* ---------- kreslenie ---------- *)
(* p\[ADoubleDot]tka kolmice z bodu p na priamku uv *)
patka[p_, u_, v_] := u + ((p - u).(v - u))/((v - u).(v - u)) (v - u)

(* v\[YAcute]\[SHacek]ka z vrcholu p na priamku uv; ak p\[ADoubleDot]tka le\[ZHacek]\[IAcute] mimo strany, dop\[IAcute]\[SHacek]e bodkovan\[YAcute] pred\:013a\[ZHacek]ok *)
vyskaGraf[p_, u_, v_, farba_, popis_, s_] := Module[
  {t = ((p - u).(v - u))/((v - u).(v - u)), f, d1, d2},
  f = u + t (v - u);
  d1 = Normalize[p - f]; d2 = Normalize[v - u];
  {farba, Dashed, Line[{p, f}],
   If[t < 0, {Gray, Dotted, Line[{u, f}]},
    If[t > 1, {Gray, Dotted, Line[{v, f}]}, {}]],
   {Black, Dashing[{}], Thin, Line[{f + s d1, f + s d1 + s d2, f + s d2}]},
   {Black, PointSize[Medium], Point[f]},
   Text[Style[popis, 12, farba, Background -> White], (p + f)/2]}
]

(* vyska na stranu cislo i; body = {A, B, C}, strana a = BC, b = CA, c = AB *)
vyskaZoI[body_, i_, farba_, popis_, s_] := Module[
  {bok = {{2, 3}, {3, 1}, {1, 2}}},
  vyskaGraf[body[[i]], body[[bok[[i, 1]]]], body[[bok[[i, 2]]]], farba, popis, s]]

(* strany = {a, b, c}; popisy = texty pri stranach (vsetky tri);
   dane, hladane = zoznamy {index strany, popis vysky};
   vsetky tri vysky sa vzdy nakreslia: dane cervene, hladane zelene, ostatne modre (v_a, v_b, v_c) *)
kresliVysky[strany_, popisy_List, dane_List, hladane_List] := Module[
  {na, nb, nc, xA, yA, body, cen, stredy, s, ostatne, abc = {"a", "b", "c"}},
  {na, nb, nc} = N[strany];
  xA = (na^2 + nc^2 - nb^2)/(2 na);
  yA = Sqrt[nc^2 - xA^2];
  body = {{xA, yA}, {0., 0.}, {na, 0.}};          (* A, B, C *)
  cen = Mean[body];
  s = 0.04 Max[na, nb, nc];
  stredy = {(body[[2]] + body[[3]])/2, (body[[3]] + body[[1]])/2,
    (body[[1]] + body[[2]])/2};
  ostatne = Complement[{1, 2, 3}, Join[First /@ dane, First /@ hladane]];
  Graphics[{
    LightBlue, EdgeForm[{Thick, Black}], Polygon[body],
    (vyskaZoI[body, #[[1]], Red, #[[2]], s] &) /@ dane,
    (vyskaZoI[body, #[[1]], Darker[Green], #[[2]], s] &) /@ hladane,
    (vyskaZoI[body, #, Blue, "v_" <> abc[[#]], s] &) /@ ostatne,
    Black, PointSize[Large], Point[body],
    popisVrcholov[body, {"A", "B", "C"}],
    MapThread[
     Text[Style[#1, 12, Background -> White],
       Offset[18 Normalize[#2 - cen], #2]] &, {popisy, stredy}]},
   Frame -> False,
   PlotRange -> All,
   AspectRatio -> Automatic,
   PlotRangePadding -> Scaled[0.2],
   ImageSize -> 340]
]

(* ---------- nastavenia ---------- *)
maxStrana = 80;              (* najdlh\[SHacek]ia strana v cm *)
povolitOdmocniny = False;    (* True: prid\[AAcute] aj \[UAcute]lohy so stranami typu 13 Sqrt[2] (len e a h) *)
maxStranaOdm = 40;           (* pri odmocnin\[AAcute]ch: najv\[ADoubleDot]\[CHacek]\[SHacek]\[IAcute] koeficient pri odmocnine *)

(* ---------- gener\[AAcute]tor trojuholn\[IAcute]kov: po\[CHacek]\[IAcute]ta sa raz pri na\[CHacek]\[IAcute]tan\[IAcute] ---------- *)
(* primit\[IAcute]vne ostrouhl\[EAcute] heronovsk\[EAcute] trojuholn\[IAcute]ky {a0, b0, c0, S0}, a0 < b0 < c0
   (cel\[EAcute] strany aj obsah, nie pr\[IAcute]li\[SHacek] ploch\[EAcute]); z nich sa \[SHacek]k\[AAcute]lovan\[IAcute]m robia \[UAcute]lohy *)
heronske = {{13, 14, 15, 84}, {13, 20, 21, 126}, {15, 34, 35, 252},
   {17, 25, 28, 210}, {25, 29, 36, 360}, {25, 39, 40, 468},
   {39, 41, 50, 780}, {56, 61, 75, 1680}, {68, 75, 77, 2310}};

(* \[CHacek]\[IAcute]slo sa d\[AAcute] zap\[IAcute]sa\[THacek] presne s najviac jedn\[YAcute]m desatinn\[YAcute]m miestom *)
den10[q_] := MemberQ[{1, 2, 5, 10}, Denominator[q]]

(* mierka s = r Sqrt[m] (r cel\[EAcute]); strany = s {a0, b0, c0}, v\[YAcute]\[SHacek]ky = s 2 S0/strana, obsah = s^2 S0 *)
skaly = Join[
   Table[{r, 1}, {r, 1, maxStrana}],
   If[povolitOdmocniny,
    Flatten[Table[{r, m}, {m, {2, 3, 5}}, {r, 1, maxStranaOdm}], 1], {}]];

(* {r, m, a0, b0, c0, S0} *)
vsetky = Flatten[Table[Join[sk, h0], {h0, heronske}, {sk, skaly}], 1];

instancie = Select[vsetky, Function[t,
    t[[1]] t[[5]] <= If[t[[2]] == 1, maxStrana, maxStranaOdm] && t[[1]] t[[3]] >= 3]];

(* koeficient v\[YAcute]\[SHacek]ky na i-tu (zoraden\[UAcute]) stranu: v\[YAcute]\[SHacek]ka = koeficient Sqrt[m] *)
vyska0[t_, i_] := t[[1]] 2 t[[6]]/t[[2 + i]]

(* zadan\[AAcute] hodnota: bez odmocniny sta\[CHacek]\[IAcute] jedno desatinn\[EAcute] miesto, s odmocninou cel\[EAcute] \[CHacek]\[IAcute]slo *)
dana[t_, x_] := If[t[[2]] == 1, den10[x], Denominator[x] == 1]

(* {strany, v\[YAcute]\[SHacek]ky, obsah} pre zoraden\[EAcute] strany a < b < c *)
rozbal[t_] := Module[{s = t[[1]] Sqrt[t[[2]]], z = t[[3 ;; 5]], obsah},
  obsah = t[[1]]^2 t[[2]] t[[6]];
  {s z, s 2 t[[6]]/z, obsah}]

(* {in\[SHacek]tancia, permut\[AAcute]cia}: (a, b, c) = strany v danom porad\[IAcute];
   dan\[EAcute] v\[YAcute]\[SHacek]ky v_a a v_c musia by\[THacek] pekn\[EAcute] *)
ulohyM = Select[
   Flatten[Table[{t, p}, {t, instancie}, {p, Permutations[{1, 2, 3}]}], 1],
   Function[u, With[{t = u[[1]], p = u[[2]]},
     dana[t, vyska0[t, p[[1]]]] && dana[t, vyska0[t, p[[3]]]]]]];

(* ---------- m: dan\[EAcute] a, v_a, b, v_c; h\:013ead\[AAcute] sa obvod a obsah ---------- *)
vyskyMedium[] := Module[
  {t, p, st, vy, obsah, strany, vysky, a, b, c, va, vc, o, zad, ries, ob},

  {t, p} = RandomChoice[ulohyM];
  {st, vy, obsah} = rozbal[t];
  strany = st[[p]];                (* a, b, c = n\[AAcute]hodn\[AAcute] permut\[AAcute]cia str\[AAcute]n *)
  vysky = vy[[p]];
  {a, b, c} = strany;
  {va, vc} = vysky[[{1, 3}]];
  o = a + b + c;

  zad = StringForm[
    "V trojuholn\[IAcute]ku ABC s\[UAcute] dan\[EAcute] a = `1` cm, v_a = `2` cm, b = `3` cm a v_c = `4` cm. Vypo\[CHacek]\[IAcute]tajte obvod a obsah.",
    dat[a], dat[va], dat[b], dat[vc]];

  ries = StringForm[
    StringRiffle[{
      "Rie\[SHacek]enie",
      "1. Obsah z v\[YAcute]\[SHacek]ky na stranu a: S = a \[CenterDot] v_a / 2 = `1` \[CenterDot] `2` / 2 = `5` cm\.b2.",
      "2. Strana c z v\[YAcute]\[SHacek]ky na \[NHacek]u: S = c \[CenterDot] v_c / 2, teda c = 2S / v_c = 2 \[CenterDot] `6` / `4` = `7` cm.",
      "3. Obvod: o = a + b + c = `1` + `3` + `9` = `8` cm.",
      "Odpove\[DHacek]: obvod je `8` cm a obsah je `5` cm\.b2."},
     "\n"],
    zr[a], zr[va], zr[b], zat[vc], hodnota[obsah], zr[obsah],
    hodnota[c], hodnota[o], zr[c]];

  ob = kresliVysky[strany,
    {"a = " <> dat[a], "b = " <> dat[b], "c = " <> dat[c]},
    {{1, "v_a = " <> dat[va]}, {3, "v_c = " <> dat[vc]}},
    {}];

  {zad, ries, ob}
]

End[]
EndPackage[]

VyskyMedium`Private`vyskyMedium[]
