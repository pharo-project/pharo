testing
isKeywordPattern
	^ self isPatternVariable	and: [ value second = RBScanner keywordPatternCharacter
				and: [ value third ~= RBScanner cascadePatternCharacter ] ]