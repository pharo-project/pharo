tests
testRobustMatcher

	| fuzzy |
	
	fuzzy := self newFuzzer.
	
	self 
	assert: (fuzzy scoredMatch: 'abcdefghijkl');
	assert: (fuzzy scoredMatch: '').
	
	fuzzy setPattern: ''.
	
	self 
	assert: (fuzzy scoredMatch: 'abcdefghijkl');
	assert: (fuzzy scoredMatch: '').
	
	fuzzy setPattern: 'afg'.
	
	self 
	assert: (fuzzy scoredMatch: 'abcdefghijkl');
	deny: (fuzzy scoredMatch: '').