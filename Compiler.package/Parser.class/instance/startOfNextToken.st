scanning
startOfNextToken
	"Return starting position in source of next token."

	hereType == #doIt ifTrue: [^source position + 1].
	^hereMark