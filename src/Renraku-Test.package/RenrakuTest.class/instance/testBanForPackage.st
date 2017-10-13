tests
testBanForPackage
	| criticToSkip |
		
	criticToSkip := testClass critiques detect: [ :critic |
		critic rule class = ScreamerRule ].
	
	testPackage ban: criticToSkip.
	
	self assert: (testClass2 critiques noneSatisfy: [ :critic |
		critic rule class = ScreamerRule ]).