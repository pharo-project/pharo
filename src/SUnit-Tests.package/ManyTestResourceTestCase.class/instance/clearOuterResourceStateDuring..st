utility
clearOuterResourceStateDuring: aBlock
	"This self-testing test must clear the outer state of its resources before starting and after finishing, so that it can construct test cases and suites of itself and test them."
	
	self
		assert: SimpleTestResourceA1 isAlreadyAvailable
		description: 'The resource was not set up for the test'.
	SimpleTestResourceA reset.
	SimpleTestResourceB reset.
	SimpleTestResourceA1 reset.
	self
		deny: SimpleTestResourceA1 isAlreadyAvailable
		description: 'The resource was still set up before we began the run'.
	^[super clearOuterResourceStateDuring: aBlock]
		ensure:
			[self
				deny: SimpleTestResourceA1 isAlreadyAvailable
				description: 'The resource was still set up after we finished the run'.
			self
				deny: SimpleTestResourceB1 isAlreadyAvailable
				description: 'The resource was still set up after we finished the run'.
			SimpleTestResourceA isAvailable.
			self
				assert: SimpleTestResourceA1 isAlreadyAvailable
				description: 'The resource was not set up again after the test'.
			SimpleTestResourceB isAvailable.
			self
				assert: SimpleTestResourceB1 isAlreadyAvailable
				description: 'The resource was not set up again after the test']