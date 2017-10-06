tests
testBan
	| criticToSkip |
		
	criticToSkip := testClass critiques detect: [ :critic |
		critic rule class = ScreamerRule ].
	
	testClass ban: criticToSkip.
	
	self assert: (testClass critiques noneSatisfy: [ :critic |
		critic rule class = ScreamerRule ]).