private
previousWord: position
	| string index |
	string := self string.
	index := position.
	[(index between: 1 and: string size) and: [(string at: index) isAlphaNumeric not]]
		whileTrue: [index := index - 1].
	[(index between: 1 and: string size) and: [(string at: index) isAlphaNumeric]]
		whileTrue: [index := index - 1].
	^ index + 1