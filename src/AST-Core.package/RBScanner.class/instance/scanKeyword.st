private-scanning
scanKeyword
	| outputPosition inputPosition name |
	[currentCharacter = $:] whileTrue: 
			[buffer nextPut: currentCharacter.
			outputPosition := buffer position.
			inputPosition := stream position.
			self step.	":"
			[characterType = #alphabetic] whileTrue: [self scanName]].
	buffer position: outputPosition.
	stream position: inputPosition.
	self step.
	name := buffer contents.
	^(name occurrencesOf: $:) == 1 
		ifTrue: [RBKeywordToken value: name start: tokenStart]
		ifFalse: 
			[RBMultiKeywordLiteralToken 
				value: name asSymbol
				start: tokenStart
				stop: tokenStart + name size - 1]