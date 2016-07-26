private
isWordChar: aCharacterOrNil
	"Answer whether the argument is a word constituent character:
	alphanumeric or _."

	^aCharacterOrNil ~~ nil
		and: [aCharacterOrNil isAlphaNumeric]