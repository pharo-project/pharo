accessing
scanToken
	"fast-n-ugly. Don't write stuff like this. Has been found to cause cancer in laboratory rats. Basically a 
	case statement. Didn't use Dictionary because lookup is pretty slow."

	characterType = #alphabetic ifTrue: [^self scanIdentifierOrKeyword].
	(characterType = #digit 
		or: [currentCharacter = $- and: [(self classify: stream peek) = #digit]]) 
			ifTrue: [^self scanNumber].
	characterType = #binary ifTrue: [^self scanBinary: RBBinarySelectorToken].
	characterType = #special ifTrue: [^self scanSpecialCharacter].
	currentCharacter = $' ifTrue: [^self scanLiteralString].
	currentCharacter = $# ifTrue: [^self scanLiteral].
	currentCharacter = $$ ifTrue: [^self scanLiteralCharacter].
	^self scanUnknownCharacter