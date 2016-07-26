point functions
insideTriangle: p1 with: p2 with: p3 
	"Return true if the receiver is within the triangle defined by the three coordinates.
	Note: This method computes the barycentric coordinates for the receiver and tests those coordinates."

	| p0 b0 b1 b2 b3 |
	p0 := self.
	b0 := (p2 x - p1 x) * (p3 y - p1 y) - ((p3 x - p1 x) * (p2 y - p1 y)).
	b0 isZero ifTrue: [ ^ false ].	"degenerate"
	b0 := 1.0 / b0.
	b1 := ((p2 x - p0 x) * (p3 y - p0 y) - ((p3 x - p0 x) * (p2 y - p0 y))) * b0.
	b2 := ((p3 x - p0 x) * (p1 y - p0 y) - ((p1 x - p0 x) * (p3 y - p0 y))) * b0.
	b3 := ((p1 x - p0 x) * (p2 y - p0 y) - ((p2 x - p0 x) * (p1 y - p0 y))) * b0.
	b1 < 0.0 ifTrue: [ ^ false ].
	b2 < 0.0 ifTrue: [ ^ false ].
	b3 < 0.0 ifTrue: [ ^ false ].
	^ true