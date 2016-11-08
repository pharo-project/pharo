private
score: aString at: stringIndex patternAt: patternIndex

	| score prev |
	
	prev := (aString at: stringIndex - 1).
	
	score := (self isSeperator: prev) 
		ifTrue: [ self separatorBonus ]
		ifFalse: [ (prev asLowercase = (lowercasePattern at: patternIndex - 1))
			ifTrue: [ 
				self adjacencyBonus + 
				((prev = (pattern at: patternIndex - 1)) ifTrue: [ self adjacentCaseEqualBonus ] ifFalse: [ 0 ]) 
			] 
			ifFalse: [ 0 ] 
		].
	
	(aString at: stringIndex) = (pattern at: patternIndex) ifTrue: [ 
		score := score + self caseEqualBonus.
	].

	^ score 