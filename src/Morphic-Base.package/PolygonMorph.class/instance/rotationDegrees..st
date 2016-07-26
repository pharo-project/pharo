geometry
rotationDegrees: degrees 
	| flex center |
	(center := self valueOfProperty: #referencePosition) ifNil:
		[self setProperty: #referencePosition toValue: (center := self bounds center)].
	flex := (MorphicTransform offset: center negated)
			withAngle: (degrees - self forwardDirection) degreesToRadians.
	self setVertices: (vertices collect: [:v | (flex transform: v) - flex offset]).
	self forwardDirection: degrees.

