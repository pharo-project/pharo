asserting
assert: actual equals: expected
	^ self
		assert: actual = expected
		description: [self comparingStringBetween: actual and: expected]