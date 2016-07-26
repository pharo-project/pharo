examples - branches
exampleCaseOf5
	| t1 |
	5
		caseOf:
			{([ 1 ] -> [ t1 := #CAIRO_FONT_SLANT_NORMAL ]).
			([ 3 ] -> [ t1 := #CAIRO_FONT_SLANT_ITALIC ]).
			([ 5 ] -> [ t1 := #CAIRO_FONT_SLANT_OBLIQUE ])}
		otherwise: [ ].
	^ t1