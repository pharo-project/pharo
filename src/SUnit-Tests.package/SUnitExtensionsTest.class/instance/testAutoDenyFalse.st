test
testAutoDenyFalse
	| booleanCondition |
	self assert: self isLogging.
	self should: [ self deny: 1 = 1 description: 'self deny: 1 = 1'.] raise: self defaultTestFailure.
	booleanCondition := (self stream contents substrings:  {Character cr}) last = 'self deny: 1 = 1'.
	self assert: booleanCondition