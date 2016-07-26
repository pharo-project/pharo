platform conventions
withSqueakLineEndings
	"Assume the string is textual, and that CR, LF, and CRLF are all valid line endings.
	Replace each occurence with a single CR."

	(self includes: Character lf) ifFalse: [ ^self ].
	(self includes: Character cr) ifFalse: [
		^self translateWith: String crLfExchangeTable ].
	^self withLineEndings: String cr