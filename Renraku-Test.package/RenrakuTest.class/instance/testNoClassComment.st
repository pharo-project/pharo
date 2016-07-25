tests
testNoClassComment
		
	self assert: (testClass critiques anySatisfy: [ :critic |
		critic rule class = RBNoClassCommentRule ]).
	
	testClass comment: 'Now I have a comment'.
	
	self assert: (testClass critiques noneSatisfy: [ :critic |
		critic rule class = RBNoClassCommentRule ]).