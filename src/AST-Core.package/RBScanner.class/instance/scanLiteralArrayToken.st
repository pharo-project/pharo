private-scanning
scanLiteralArrayToken
	| token |
	token := RBLiteralArrayToken 
				value: (String with: $# with: currentCharacter)
				start: tokenStart.
	self step.
	^token