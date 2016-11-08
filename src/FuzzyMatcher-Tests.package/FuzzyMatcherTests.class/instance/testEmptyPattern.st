tests
testEmptyPattern

	| fuzzy score |
	
	
	fuzzy := self newFuzzer.
	
	self
	assert: fuzzy pattern isEmpty; 
	assert: (fuzzy matches: '');
	assert: (fuzzy matches: 'abc');
	assert: (fuzzy matches: '   ').
	
	score := nil.
	fuzzy match: '' ifScored: [ :s | score := s ].	
	self assert: score == 0.
	
	score := nil.
	fuzzy match: 'abc' ifScored: [ :s | score := s ].	
	self assert: score == 0.
	
	score := nil.
	fuzzy match: '   ' ifScored: [ :s | score := s ].
	self assert: score == 0.
	