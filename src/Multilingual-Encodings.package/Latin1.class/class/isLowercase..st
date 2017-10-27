character classification
isLowercase: char
	"Answer whether the receiver is a lowercase letter.
	The inherited implementation gives the wrong answer for µ and ß,
	and for accented characters."

	| code |
	code := char codePoint.
	(code between: 8r141 and: 8r172) ifTrue: [ ^ true ].
	(char = $÷) ifTrue: [ ^ false ].
	(char = $µ) ifTrue: [ ^ true  ].
	^ code >= 8r337