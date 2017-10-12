api-events
whenClosedDo: aBlock

	isClosedHolder whenChangedDo: [:value |
		value ifTrue: [ aBlock value ] ]