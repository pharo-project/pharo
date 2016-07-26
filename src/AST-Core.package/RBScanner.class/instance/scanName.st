private-scanning
scanName
	[characterType = #alphabetic or: [characterType = #digit]] whileTrue: 
			[buffer nextPut: currentCharacter.
			self step]