testing
isIdentity
	^ globalTransform isIdentity and: [localTransform isIdentity]