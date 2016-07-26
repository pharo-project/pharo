tests
testBan
	| criticToSkip |
		
	criticToSkip := testClass critiques detect: [ :critic |
		critic rule class = RBNoClassCommentRule ].
	
	testClass ban: criticToSkip.
	
	self assert: (testClass critiques noneSatisfy: [ :critic |
		critic rule class = RBNoClassCommentRule ]).