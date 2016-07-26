printing
asTwoCharacterString
	"Answer a two-character string representing the receiver, with leading zero if required.  Intended for use with integers in the range 0 to 99, but plausible replies given for other values too
	Examples:
		2 asTwoCharacterString
		11 asTwoCharacterString
		1943 asTwoCharacterString
		0 asTwoCharacterString
		-2 asTwoCharacterString
		-234 asTwoCharacterString "

	^ (self >= 0 and: [self < 10])
		ifTrue:	['0', self printString]
		ifFalse:	[self printString copyFrom: 1 to: 2]