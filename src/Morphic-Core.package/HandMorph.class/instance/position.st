geometry
position

	^temporaryCursor
		ifNil: [bounds topLeft]
		ifNotNil: [bounds topLeft - temporaryCursorOffset]