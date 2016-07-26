menu
unrotatedLength
	"If the receiver bears rotation without a transformation morph, answer what its length in the direction it is headed is"

	vertices size = 2 ifTrue:
		[^ (vertices second - vertices first) r].

	^ ((PolygonMorph new setVertices: vertices) rotationDegrees: self rotationDegrees negated) height