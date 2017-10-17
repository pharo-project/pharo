asserting
assert: actual identicalTo: expected
	^ self
		assert: expected == actual
		description: [self comparingIdentityStringBetween: actual and: expected]