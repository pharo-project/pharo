accessing
extents
	| extents |
	extents := CairoFontExtents new.

	self getExtents: extents.
	
	^ extents