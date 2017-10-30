asserting
deny: actual equals: expected
	^ self
		deny: expected = actual
		description: [self unexpectedEqualityStringBetween: actual and: expected]