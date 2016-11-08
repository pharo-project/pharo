rotate scale and flex
rotationDegrees: degrees
	| ref newPos flex origAngle origCenter |
	ref := self referencePosition.
	origAngle := originalAngle ifNil: [ self heading ].
	origCenter := originalCenter ifNil: [ self center ].
	flex := (MorphicTransform offset: ref negated)
		withAngle: (degrees - origAngle) degreesToRadians.
	newPos := (flex transform: origCenter) - flex offset.
	self position: (self position + newPos - self center) asIntegerPoint.
	referencePosition := ref.
	originalAngle := origAngle.
	originalCenter := origCenter.
	self forwardDirection: degrees.
	self changed