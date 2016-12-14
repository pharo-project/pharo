api
possibleEntities
	possibleEntities value ifNil: [ ^#() ]
		ifNotNil: [ ^possibleEntities value value ]