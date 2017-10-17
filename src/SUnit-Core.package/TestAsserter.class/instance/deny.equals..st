asserting
deny: actual equals: expected
	^ self
		deny: expected = actual
		description: [self comparingStringBetween: actual and: expected]