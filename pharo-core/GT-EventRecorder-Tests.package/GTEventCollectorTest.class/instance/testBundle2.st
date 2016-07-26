tests
testBundle2
	collector add: GTDummyEvent new.
	collector add: GTDummyEvent new.
	self assert: collector bundle size equals: 2.
	self assert: collector bundle isEmpty.