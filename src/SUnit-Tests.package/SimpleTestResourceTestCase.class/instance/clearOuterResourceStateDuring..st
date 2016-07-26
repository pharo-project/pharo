utility
clearOuterResourceStateDuring: aBlock
	"This self-testing test must clear the outer state of its resources before starting and after finishing, so that it can construct test cases and suites of itself and test them."
	
	self
		assert: SimpleTestResource isAlreadyAvailable
		description: 'The resource was not set up for the test'.
	SimpleTestResource reset.
	self
		deny: SimpleTestResource isAlreadyAvailable
		description: 'The resource was still set up before we began the run'.
	^aBlock
		ensure:
			[self
				deny: SimpleTestResource isAlreadyAvailable
				description: 'The resource was still set up after we finished the run'.
			SimpleTestResource isAvailable.
			self
				assert: SimpleTestResource isAlreadyAvailable
				description: 'The resource was not set up again after the test']