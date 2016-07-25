test
testAutoAssertFalse
	| booleanCondition |
	self assert: self isLogging.
	self should: [ self assert: 1 = 2 description: 'self assert: 1 = 2' ] raise: self defaultTestFailure.
	booleanCondition := (self stream contents substrings: {Character cr}) last = 'self assert: 1 = 2'.
	self assert: booleanCondition