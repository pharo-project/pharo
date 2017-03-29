opening
open

	| window area |
	
	window := (self buildMorph openInWindowLabeled: 'Fuzzy Matching Example').
	area := window world clearArea.
	
	window 
		extent: (area extent * (0.6 @ 0.7)) asIntegerPoint;
		center: area center.
	
	^ window