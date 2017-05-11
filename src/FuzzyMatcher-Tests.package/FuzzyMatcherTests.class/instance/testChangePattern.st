tests
testChangePattern

	| matcher |
	
	matcher := self newMatcher.
	
	self 
	assert: matcher pattern equals: '';
	assert: (matcher matches: 'abcdefghijkl');
	assert: (matcher matches: '').
	
	matcher pattern: 'afg'.
	
	self 
	assert: (matcher matches: 'abcdefghijkl');
	deny: (matcher matches: '').
	