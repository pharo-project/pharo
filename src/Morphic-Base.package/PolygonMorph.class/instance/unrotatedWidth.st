menu
unrotatedWidth
	"If the receiver bears rotation without a transformation morph, answer what its length in the direction it is headed is"
	
	vertices size = 2 ifTrue: [^ self borderWidth].
	^ ((PolygonMorph new setVertices: vertices) rotationDegrees: self rotationDegrees negated) width