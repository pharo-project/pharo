transformations
transformBoundsRect: aRectangle 
	"Return a rectangle whose coordinates have been transformed
	from global to local coordinates.  NOTE: if the transformation
	is not just a translation, then it will compute the bounding box
	in global coordinates."
	| outerRect |
	self isPureTranslation 
		ifTrue: 
			[ ^ (self transform: aRectangle topLeft) corner: (self transform: aRectangle bottomRight) ]
		ifFalse: 
			[ outerRect := Rectangle encompassing: (aRectangle innerCorners collect: [ :p | self transform: p ]).
			"Following asymmetry due to likely subsequent truncation"
			^ outerRect topLeft - (1 @ 1) corner: outerRect bottomRight + (2 @ 2) ]