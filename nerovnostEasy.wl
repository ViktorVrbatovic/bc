(* ::Package:: *)

BeginPackage["NerovnostEasy`"]
Begin["`Private`"]

vykresliTrojuholnik[a_, b_, c_, existuje_] :=
 If[existuje,
  With[{p = First[SSSTriangle[a, b, c]]},
   Graphics[{
     LightBlue, EdgeForm[{Thick, Black}], Triangle[p],
     Black, PointSize[Large], Point[p],
     MapThread[
      Text[Style[#1, Bold, 14], Offset[14 Normalize[N[#2 - Mean[p]]], #2]] &,
      {{"A", "B", "C"}, p}]},
     Frame -> True,
     PlotRange -> All,
     AspectRatio -> 1,
     PlotRangePadding -> Scaled[0.15],
     ImageSize -> 300]
   ],
  Style["Trojuholn\[IAcute]k sa ned\[AAcute] zostroji\[THacek].", Red, 14]
  ]

nerovnostEasy[] := Module[
    {a, b, c, strany, najdlhsia, zvysok, existuje, zad, ries, ob},

    a = RandomInteger[{3, 12}];
    b = RandomInteger[{3, 12}];
    c = RandomInteger[{3, 20}];

    strany = {a, b, c};
    najdlhsia = Max[strany];
    zvysok = Total[strany] - najdlhsia;

    existuje = najdlhsia < zvysok;

    zad = StringForm["Rozhodnite, \[CHacek]i d\:013a\[ZHacek]ky a = `1` cm, b = `2` cm, c = `3` cm m\[OHat]\[ZHacek]u tvori\[THacek] trojuholn\[IAcute]k.",
       a, b, c
    ];

    ries = StringForm[
       "Najdlh\[SHacek]ia strana: `1` cm\nS\[UAcute]\[CHacek]et zvy\[SHacek]n\[YAcute]ch str\[AAcute]n: `2` cm\nKe\[DHacek]\[ZHacek]e `1` `3` `2`, odpove\[DHacek] je `4`.",
       najdlhsia,
       zvysok,
       If[existuje, "<", ">="],
       If[existuje, "\[CapitalAAcute]NO", "NIE"]
    ];
    
	ob = vykresliTrojuholnik[a, b, c, existuje];

    Return[{zad, ries, ob}]
]
End[]
EndPackage[]




