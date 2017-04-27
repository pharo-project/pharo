tests
testScoreValues

	| matcher |
	
	matcher := self newMatcher.
	
	matcher pattern: 'a'.
	
	self 
	
		assert: matcher 
		scoreFor: 'a' 
		equals: (matcher firstLetterBonus + matcher caseEqualBonus);
		
		assert: matcher 
		scoreFor: 'A' 
		equals: (matcher firstLetterBonus);
		
		assert: matcher 
		scoreFor: 'ab' 
		equals: (matcher firstLetterBonus + matcher caseEqualBonus + matcher unmatchedLetterPenalty);
		
		assert: matcher 
		scoreFor: '1a' 
		equals: (matcher leadingLetterPenalty + matcher unmatchedLetterPenalty + matcher caseEqualBonus);
		
		assert: matcher 
		scoreFor: '12345a' 
		equals: (matcher maxLeadingLetterPenalty + (5 * matcher unmatchedLetterPenalty ) + (matcher caseEqualBonus)).
		
	matcher pattern: 'ab'.
	
	self 
	
		assert: matcher 
		scoreFor: 'ab' 
		equals: [ "delayed bonus calculation since #indexscore is based on state after matching" 
			matcher firstLetterBonus 
			+  matcher adjacencyBonus 
			+ (matcher caseEqualBonus * 2) 
			+ (matcher adjacentCaseEqualBonus) 
			+  matcher indexScore
		];
		
		assert: matcher 
		scoreFor: 'A:B' 
		equals: (
			matcher firstLetterBonus 
			+ matcher separatorBonus 
			+ matcher unmatchedLetterPenalty
		);
		
		assert: matcher 
		scoreFor: 'able' 
		equals: [ 
			matcher firstLetterBonus 
			+  matcher adjacencyBonus 
			+  matcher adjacentCaseEqualBonus 
			+ (matcher caseEqualBonus * 2) 
			+ (matcher unmatchedLetterPenalty * 2)
			+  matcher indexScore
		].