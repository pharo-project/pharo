specific inquiries
easterDateFor: year

 "  compute the easter date.
    source: Physikalisch-Technische Bundesanstalt Braunschweig.
    Lichtenberg, H.: Zur Interpretation der Gaussschen Osterformel
                     und ihrer Ausnahmeregeln,
                     Historia Mathematica 24 (1997), pp. 441-444
  
    http://www.ptb.de/de/org/4/44/441/oste.htm
  "

  | k m s a d r og sz oe day |

  k := year // 100.
  m := 15 + (3*k + 3//4) - (8*k + 13//25).
   s := 2 - (3*k + 3// 4).
  a := year \\ 19.
  d := 19*a + m \\ 30.
  r := d//29 + ((d//28) - (d//29)* (a// 11)).

  og := 21 + d - r.
  sz := 7 - (year//4 + year + s\\7).
  oe := 7 - (og - sz\\7).
  day := og + oe.
  ^day <= 31
    ifTrue: [ Date year: year month: 3 day: day ]
    ifFalse: [ Date year: year month: 4 day: day - 31 ].