running
randomCharacterBetween: lower and: upper
	^ Character codePoint: (self randomCodePointBetween: lower and: upper) 