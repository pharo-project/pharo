private-scanning
scanPatternVariable
	buffer nextPut: currentCharacter.
	self step.
	currentCharacter = ${ 
		ifTrue: 
			[self step.
			^RBPatternBlockToken value: '`{' start: tokenStart].
	[characterType = #alphabetic] whileFalse: 
			[characterType = #eof 
				ifTrue: [ ^ self scanError: 'Meta variable expected'].
			buffer nextPut: currentCharacter.
			self step].
	^self scanIdentifierOrKeyword