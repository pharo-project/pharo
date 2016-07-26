platform conventions
withUnixLineEndings
	"Assume the string is textual, and that CR, LF, and CRLF are all valid line endings.
	Replace each occurence with a single LF."

	(self includes: Character cr) ifFalse: [ ^self ].
	(self includes: Character lf) ifFalse: [
		^self translateWith: String crLfExchangeTable ].
	^self withLineEndings: String lf