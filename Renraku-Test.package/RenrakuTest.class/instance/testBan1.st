tests
testBan1
	| criticToSkip |
	
		
	criticToSkip := testMethod critiques detect: [ :critic |
		critic rule class = RBUnclassifiedMethodsRule ].
	
	testMethod ban: criticToSkip.
	
	self assert: (testMethod critiques noneSatisfy: [ :critic |
		critic rule class = RBUnclassifiedMethodsRule ]).