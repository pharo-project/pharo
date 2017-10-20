tests
testBan1
	| criticToSkip |
	
		
	criticToSkip := testMethod critiques detect: [ :critic |
		critic rule class = ScreamerRule ].
	
	testMethod ban: criticToSkip.
	
	self assert: (testMethod critiques noneSatisfy: [ :critic |
		critic rule class = ScreamerRule ]).