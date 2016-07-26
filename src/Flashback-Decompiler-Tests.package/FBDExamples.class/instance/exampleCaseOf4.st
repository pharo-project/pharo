examples - branches
exampleCaseOf4
	| t1 |
	5
		caseOf:
			{([ 1 ] -> [ t1 := {#CAIRO_FONT_SLANT_NORMAL} first ]).
			([ 3 ] -> [ t1 := #CAIRO_FONT_SLANT_ITALIC ]).
			([ 5 ] -> [ t1 := #CAIRO_FONT_SLANT_OBLIQUE ])}
		otherwise: [ t1 := #NIL ].
	^ t1