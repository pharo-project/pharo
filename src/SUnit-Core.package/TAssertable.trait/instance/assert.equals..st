asserting
assert: actual equals: expected
	^ self
		assert: expected = actual
		description: [self comparingStringBetween: actual and: expected]