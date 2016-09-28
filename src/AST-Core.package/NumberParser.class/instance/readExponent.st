parsing-private
readExponent
	"read the exponent if any (stored in instVar).
	Answer true if found, answer false if none.
	If exponent letter is not followed by a digit,
	this is not considered as an error.
	Exponent are always read in base 10."

	| eneg epos |
	exponent := 0.
	sourceStream atEnd
		ifTrue: [ ^ false ].
	(self exponentLetters includes: sourceStream peek)
		ifFalse: [ ^ false ].
	sourceStream next.
	eneg := sourceStream peekFor: $-.
	epos := eneg not
		and: [ self allowPlusSignInExponent and: [ sourceStream peekFor: $+ ] ].
	exponent := self nextUnsignedIntegerOrNilBase: 10.
	exponent
		ifNil:
			[ "Oops, there was no digit after the exponent letter.Ungobble the letter"
			exponent := 0.
			sourceStream
				skip:
					((eneg or: [ epos ])
						ifTrue: [ -2 ]
						ifFalse: [ -1 ]).
			^ false ].
	eneg
		ifTrue: [ exponent := exponent negated ].
	exponent abs > 1023
		ifTrue:
			[ "Number literals should have a maximun of 1023 and a minumum of -1022 as exponent, in exponential notation"
			self fail ].
	^ true