accessing
scanToken
	currentCharacter = PatternVariableCharacter 
		ifTrue: [^self scanPatternVariable].
	currentCharacter = $} ifTrue: [^self scanSpecialCharacter].
	^super scanToken