menu
unrotatedLength: aLength
	"If the receiver bears rotation without a transformation morph, answer what its length in the direction it is headed is"

	vertices size = 2 ifTrue: [^ self arrowLength: aLength].

	self setVertices: ((((PolygonMorph new setVertices: vertices) rotationDegrees: self rotationDegrees negated) height: aLength) rotationDegrees: 0) vertices