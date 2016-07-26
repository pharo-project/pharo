private-scanning
stripSeparators
	
	[[characterType = #separator] whileTrue: [self step].
	currentCharacter = $"] 
			whileTrue: [self stripComment]