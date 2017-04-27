tests
testMatching

	| matcher |
	
	matcher := self newMatcher.
	matcher pattern: 'a'.
	
	self 
		assert: (matcher matches: 'a');
		assert: (matcher matches: String cr, String tab, String lf, String space, 'a');
		assert: (matcher matches: ',;:_-!"#¤%&/?+\()a').
		