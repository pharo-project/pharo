private
previousWord: position
	| string index  initialIsAlphaNumeric |
	string := self string.
	index := position.
	position <= 1 ifTrue: [ ^ position ].
	initialIsAlphaNumeric := (string at: index) isAlphaNumeric.
	[index := index - 1] doWhileTrue:
		[(index > 0) and: [(string at: index) isAlphaNumeric = initialIsAlphaNumeric]].
	^ index + 1