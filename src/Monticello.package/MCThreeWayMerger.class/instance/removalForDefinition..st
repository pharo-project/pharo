operations
removalForDefinition: aDefinition
	^ operations ifNotNil:
		[operations
			detect: [:ea | (ea definition isRevisionOf: aDefinition) and: [ea isRemoval]]
			ifNone: []]