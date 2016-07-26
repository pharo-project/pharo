input
readOptionalSeparatorFrom: stream
	"Read an optional separator (non decimal digit) from stream and return it.
	Return nil if nothing was read"

	| isDigit |
	stream atEnd
		ifTrue: [ ^ nil ].
	isDigit := '0123456789' includes: stream peek.
	(isDigit or: [ ':' includes: stream peek ])
		ifFalse: [ ^ nil ].
	isDigit
		ifFalse: [ stream next ]