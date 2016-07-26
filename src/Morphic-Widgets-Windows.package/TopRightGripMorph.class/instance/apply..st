target resize
apply: delta 
	| oldBounds |
	oldBounds := target bounds.
	target
		bounds: (oldBounds origin + (0@delta y) corner: oldBounds corner + (delta x @ 0))