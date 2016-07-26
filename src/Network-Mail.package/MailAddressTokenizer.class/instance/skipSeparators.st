tokenizing
skipSeparators
	pos := text indexOfAnyOf: CSNonSeparators  startingAt: pos  ifAbsent: [ text size + 1 ].