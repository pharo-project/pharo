target resize
apply: delta 
	| oldBounds |
	oldBounds := target bounds.
	target
		bounds: (oldBounds origin corner: oldBounds corner + delta)