querying
defaultCharacterBlock
	^ (RubCharacterBlock new
		stringIndex: 1
		topLeft: self lines first topLeft
		extent: 0 @ 0) textLine: self lines first