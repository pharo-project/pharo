tests
testAsDays
	self assert: (Duration days: 2) asDays = 2.
	self assert: (Duration weeks: 1) asDays = 7.	
	self assert: (aDuration asDays closeTo: 1.08546).