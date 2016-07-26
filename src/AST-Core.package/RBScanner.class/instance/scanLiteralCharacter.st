private-scanning
scanLiteralCharacter
	| token |
	self step.	"$"
	currentCharacter ifNil:[ ^ self scanError:'A Character was expected'].
	token := RBLiteralToken 
				value: currentCharacter
				start: tokenStart
				stop: stream position.
	self step.	"char"
	^token