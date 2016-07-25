private
nextWord: position
	| string index  initialIsAlphaNumeric |
	string := self string.
	index := position.
	position >= string size ifTrue: [ ^ position ].
	initialIsAlphaNumeric := (string at: index - 1) isAlphaNumeric.

	[(index <= string size) 
		and: [(string at: index) isAlphaNumeric = initialIsAlphaNumeric]]
			whileTrue: [index := index + 1].
	^ index