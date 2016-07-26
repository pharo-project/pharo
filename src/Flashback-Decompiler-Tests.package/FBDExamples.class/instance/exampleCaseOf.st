examples - branches
exampleCaseOf
	^ 5
		caseOf:
			{([ 1 ] -> [ #CAIRO_FONT_SLANT_NORMAL ]).
			([ 3 ] -> [ #CAIRO_FONT_SLANT_ITALIC ]).
			([ 5 ] -> [ #CAIRO_FONT_SLANT_OBLIQUE ])}