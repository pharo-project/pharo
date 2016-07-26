tests
testBan2
	| criticToSkip |
	
		
	criticToSkip := testMethod critiques detect: [ :critic |
		critic rule class = RBUnclassifiedMethodsRule ].
	
	testClass ban: criticToSkip.
	
	self assert: (testMethod critiques noneSatisfy: [ :critic |
		critic rule class = RBUnclassifiedMethodsRule ]).