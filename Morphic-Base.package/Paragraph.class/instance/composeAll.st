composition
composeAll
	^ self
		multiComposeLinesFrom: firstCharacterIndex
		to: text size
		delta: 0
		into: OrderedCollection new
		priorLines: Array new
		atY: container top 