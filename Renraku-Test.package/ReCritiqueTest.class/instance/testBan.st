tests
testBan
	| criticToSkip |
		
	criticToSkip := testClass critiques detect: [ :critic |
		critic rule class = RBNoClassCommentRule ].
	
	criticToSkip ban.
	
	self assert: (testClass critiques noneSatisfy: [ :critic |
		critic rule class = RBNoClassCommentRule ]).