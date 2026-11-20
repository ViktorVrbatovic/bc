(* ::Package:: *)




ClearAll["Global`*"];


(* ::Package:: *)
(**)


adresar = "C:\\Users\\Viktor\\OneDrive\\Desktop\\banger\\generatory\\";

okruhy = <|
  "Nerovnos\[THacek] trojuholn\[IAcute]ka" -> <|
    "Easy"   -> "nerovnostEasy",
    "Medium" -> "nerovnostMedium",
    "Hard"   -> "nerovnostHard"|>,

  "Pomer" -> <|
    "Easy" -> "pomerEasy",
    "Medium"->"pomerMedium",
    "Hard" -> "pomerHard"|>,

  "Rovnoramenn\[YAcute] trojuholn\[IAcute]k" -> <|
    "Easy"   -> "rovnoRamennyEasy",
    "Medium" -> "rovnoRamennyNormal",
    "Hard"   -> "rovnoRamennyHard"|>,

  "Rovnostrann\[YAcute] trojuholn\[IAcute]k" -> <|
    "Easy"   -> "rovnostrannyEasy",
    "Medium" -> "rovnostrannyNormal",
    "Hard"   -> "rovnostrannyHard"|>,

  "Pytagoras" -> <|
    "Easy"   -> "pytagorasEasy",
    "Medium" -> "pytagorasMedium",
    "Hard"   -> "pytagorasHard"|>,

  "Euklidove vety" -> <|
    "Easy"   -> "euklidEasy",
    "Medium" -> "euklidMedium",
    "Hard"   -> "euklidHard"|>,

  "\[CapitalTHacek]a\[ZHacek]nica a \[THacek]a\[ZHacek]isko" -> <|
    "Easy"   -> "taznicaEasy",
    "Medium" -> "taznicaMedium",
    "Hard"   -> "taznicaHard"|>,

  "V\[YAcute]\[SHacek]ky" -> <|
    "Easy"   -> "vyskyEasy",
    "Medium" -> "vyskyMedium",
    "Hard"   -> "vyskyHard"|>,

  "Vp\[IAcute]san\[AAcute] kru\[ZHacek]nica" -> <|
    "Easy"   -> "vpisanaEasy",
    "Medium" -> "vpisanaMedium",
    "Hard"   -> "vpisanaHard"|>,

  "Op\[IAcute]san\[AAcute] kru\[ZHacek]nica" -> <|
    "Easy"   -> "opisanaEasy",
    "Medium" -> "opisanaMedium",
    "Hard"   -> "opisanaHard"|>
|>;

(* euklidEasy -> kontext EuklidEasy` *)
kontext[meno_] := ToUpperCase[StringTake[meno, 1]] <> StringDrop[meno, 1] <> "`";

nacitaj[meno_] := Module[{subor = adresar <> meno <> ".wl"},
  If[FileExistsQ[subor],
   Get[subor],
   Print["Ch\[YAcute]ba s\[UAcute]bor: ", subor]; $Failed]
  ];

(* funkcia je definovan\[AAcute] v kontexte Meno`Private`, preto "Private`" *)
spusti[meno_] := Module[{symbol = kontext[meno] <> "Private`" <> meno},
  If[Names[symbol] === {},
   Print["Nena\[CHacek]\[IAcute]tan\[AAcute] funkcia: ", symbol]; $Failed,
   ToExpression[symbol][]]
  ];

(* na\[CHacek]\[IAcute]tanie s\[UAcute]borov *)
nacitaj /@ Flatten[Values /@ Values[okruhy]];

(* vysledky["Euklidove vety", "Easy"] -> {zadanie, rie\[SHacek]enie, obr\[AAcute]zok} *)
vysledky = Map[spusti, okruhy, {2}];

zobrazVysledok[{zad_, ries_, ob_}] := Column[{zad, ries, ob}, Spacings -> 1];
zobrazVysledok[_] := Style["Chyba: gener\[AAcute]tor sa nespustil", Red];

zobraz[] := Column[
  KeyValueMap[
   Function[{okruh, obt},
    Panel[
     Column[{
       Style[okruh, Bold, 16],
       Grid[
        KeyValueMap[{Style[#1, Bold], zobrazVysledok[#2]} &, vysledky[okruh]],
        Alignment -> {Left, Top}, Dividers -> All, Spacings -> {2, 1}]
       }, Spacings -> 1],
     ImageSize -> 700]],
   okruhy],
  Spacings -> 2];

zobraz[]




