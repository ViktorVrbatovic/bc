(* ::Package:: *)

BeginPackage["NerovnostMedium`"]
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


nerovnostMedium[]:= Module[
	{a, b, pocet, dolna, horna, zad, ries, ob, existuje, c},
	
	a = RandomInteger[{3, 10}];
	b = RandomInteger[{3, 10}];
	
	dolna = Abs[a - b];
	horna = a + b;
	
	pocet = horna - dolna - 1;
	

	
	zad = StringForm[
		"Ko\:013ek\[YAcute]mi cel\[YAcute]mi \[CHacek]\[IAcute]slami m\[OHat]\[ZHacek]e by\[THacek] vyjadren\[AAcute]
		strana c trojuholn\[IAcute]ka ABC, ak a = `1` cm a b = `2` cm?", a, b];
		
	ries = StringForm[
		"Rie\[SHacek]enie\n
		1. Pou\[ZHacek]ijeme trojuholn\[IAcute]kov\[UAcute] nerovnos\[THacek]:\n
		\t|a - b| < c < a + b\n
		2. Dosad\[IAcute]me zadan\[EAcute] d\:013a\[ZHacek]ky str\[AAcute]n:\n
		\t|`1` - `2`| < c < `1` + `2`\n
		\t`3` < c < `4`\n
		3. Ke\[DHacek]\[ZHacek]e c je cel\[EAcute] \[CHacek]\[IAcute]slo, m\[OHat]\[ZHacek]e nadob\[UAcute]da\[THacek] hodnoty od `5` po `6`.\n
		4. Po\[CHacek]et cel\[YAcute]ch \[CHacek]\[IAcute]sel vypo\[CHacek]\[IAcute]tame:\n
		\t`4` - `3` - 1 = `7`.\n
		Odpove\[DHacek]: Strana c m\[OHat]\[ZHacek]e by\[THacek] vyjadren\[AAcute] `7` cel\[YAcute]mi \[CHacek]\[IAcute]slami.",
		a, b, dolna, horna,dolna + 1, horna - 1, pocet
    ];
	
	If[pocet > 0,
	   c = RandomInteger[{dolna + 1, horna - 1}];
	   existuje = True,
	   c = 0;
	   existuje = False
   ];
	ob = vykresliTrojuholnik[a, b, c, existuje];
	Return[{zad, ries, ob}];
	
]
End[]
EndPackage[]



