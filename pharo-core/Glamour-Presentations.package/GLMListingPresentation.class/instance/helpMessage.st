accessing
helpMessage

	^ helpMessage ifNil: [
		helpMessage := 'Search Input'
		]

	