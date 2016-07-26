private-scanning
scanBinary: aClass 
	| val |
	buffer nextPut: currentCharacter.
	self step.
	[ characterType = #binary ] whileTrue: 
		[ buffer nextPut: currentCharacter.
		self step ].
	val := buffer contents.
	val := val asSymbol.
	^aClass value: val start: tokenStart