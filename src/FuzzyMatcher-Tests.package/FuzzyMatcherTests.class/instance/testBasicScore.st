tests
testBasicScore

	| fuzzy |
	
	fuzzy := self newFuzzer.
	
	fuzzy setPattern: 'a'.
	
	self 
		assert: (fuzzy scoreFor: 'a') 
		equals: (fuzzy firstLetterBonus + fuzzy caseEqualBonus);
		
		assert: (fuzzy scoreFor: 'A')
		equals: (fuzzy firstLetterBonus);
		
		assert: (fuzzy scoreFor: 'ab') 
		equals: (fuzzy firstLetterBonus + fuzzy caseEqualBonus + fuzzy unmatchedLetterPenalty);
				
		assert: (fuzzy scoreFor: '1a')
		equals: (fuzzy leadingLetterPenalty + fuzzy unmatchedLetterPenalty + fuzzy caseEqualBonus);
		
		assert: (fuzzy scoreFor: '12345a')
		equals: (fuzzy maxLeadingLetterPenalty + (5 * fuzzy unmatchedLetterPenalty ) + (fuzzy caseEqualBonus) ).
		
		
	fuzzy setPattern: 'ab'.
	
	self 
		assert: (fuzzy scoreFor: 'ab') 
		equals: (fuzzy firstLetterBonus + fuzzy adjacencyBonus + (fuzzy caseEqualBonus * 2) + (fuzzy adjacentCaseEqualBonus) + fuzzy indexScore);
		
		assert: (fuzzy scoreFor: 'A:B')
		equals: (fuzzy firstLetterBonus + fuzzy separatorBonus + fuzzy unmatchedLetterPenalty);
		
		assert: (fuzzy scoreFor: 'able') 
		equals: (
			fuzzy firstLetterBonus + 
			fuzzy adjacencyBonus + 
			fuzzy adjacentCaseEqualBonus +
			(fuzzy caseEqualBonus * 2) +
			(fuzzy unmatchedLetterPenalty * 2)
			+ fuzzy indexScore
		).
