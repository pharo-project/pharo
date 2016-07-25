target resize
apply: delta 
	| oldBounds |
	oldBounds := target bounds.
	target
		bounds: (oldBounds origin + (delta x @ 0) corner: oldBounds corner + (0 @ delta y))