examples - branches
exampleCaseOf7
	| t1 |
	5
		caseOf:
			{([ 1 ]
				-> [ 
					t1 := 2.
					t1 := t1 + 1 ]).
			([ 3 ] -> [ t1 := {#CAIRO_FONT_SLANT_ITALIC} first ]).
			([ 5 ] -> [ t1 := #CAIRO_FONT_SLANT_OBLIQUE ])}
		otherwise: [ t1 := #NIL ].
	^ t1