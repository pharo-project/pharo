character classification
canBeGlobalVarInitial: char

	| leadingChar |
	leadingChar := char leadingChar.

	leadingChar = 0 ifTrue: [^ self isUppercase: char].
	^ self isLetter: char.
