tests
testEmptyPattern

	| matcher |
	
	matcher := self newMatcher.
	
	self
	
		assert: matcher pattern isEmpty; 
		
		assert: matcher scoreFor: '' 		equals: 0;
		assert: matcher scoreFor: 'abc' 	equals: 0;
		assert: matcher scoreFor: '   ' 	equals: 0.
			
			
	