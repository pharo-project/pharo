private
firstScore: aString at: anIndex

	| score |

	score := (aString at: anIndex) = pattern first 
		ifTrue: [ self caseEqualBonus ]
		ifFalse: [ 0 ].
	
	anIndex = 1 	ifTrue: [ ^ score + self firstLetterBonus ].
		
	score := score + (((anIndex - 1) * self leadingLetterPenalty) max: self maxLeadingLetterPenalty).
				
	^ score 