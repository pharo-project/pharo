character classification
canBeNonGlobalVarInitial: char

	| leadingChar |
	leadingChar := char leadingChar.

	leadingChar = 0 ifTrue: [^ self isLowercase: char].
	^ self isLetter: char.
