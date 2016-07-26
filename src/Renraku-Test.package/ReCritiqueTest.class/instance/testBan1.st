tests
testBan1
	| criticToSkip |
	
		
	criticToSkip := testMethod critiques detect: [ :critic |
		critic rule class = RBUnclassifiedMethodsRule ].
	
	criticToSkip ban.
	
	self assert: (testMethod critiques noneSatisfy: [ :critic |
		critic rule class = RBUnclassifiedMethodsRule ]).