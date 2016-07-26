target resize
apply: delta 
	| oldBounds |
	oldBounds := target bounds.
	target
		bounds: (oldBounds origin + delta corner: oldBounds corner)