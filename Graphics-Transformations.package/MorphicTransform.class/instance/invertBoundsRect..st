transformations
invertBoundsRect: aRectangle 
	"Return a rectangle whose coordinates have been transformed
	from local back to global coordinates.  NOTE: if the transformation
	is not just a translation, then it will compute the bounding box
	in global coordinates."
	| outerRect |
	self isPureTranslation 
		ifTrue: 
			[ ^ (self invert: aRectangle topLeft) corner: (self invert: aRectangle bottomRight) ]
		ifFalse: 
			[ outerRect := Rectangle encompassing: (aRectangle innerCorners collect: [ :p | self invert: p ]).
			"Following asymmetry due to likely subsequent truncation"
			^ outerRect topLeft - (1 @ 1) corner: outerRect bottomRight + (2 @ 2) ]