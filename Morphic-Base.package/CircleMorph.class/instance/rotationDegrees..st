rotate scale and flex
rotationDegrees: degrees
	| ref newPos flex origAngle origCenter |
	ref := self referencePosition.
	origAngle := self valueOfProperty: #originalAngle ifAbsentPut: [ self heading ].
	origCenter := self valueOfProperty: #originalCenter ifAbsentPut: [ self center ].
	flex := (MorphicTransform offset: ref negated)
			withAngle: (degrees - origAngle) degreesToRadians.
	newPos := (flex transform: origCenter) - flex offset.
	self position: (self position + newPos - self center) asIntegerPoint.
	self setProperty: #referencePosition toValue: ref.
	self setProperty: #originalAngle toValue: origAngle.
	self setProperty: #originalCenter toValue: origCenter.
	self forwardDirection: degrees.
	self changed.
