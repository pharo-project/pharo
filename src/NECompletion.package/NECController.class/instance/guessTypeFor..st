type guessing
guessTypeFor: aString
	^ model isNil ifFalse: [ model guessTypeForName: aString ]