asserting
deny: actual identicalTo: expected
	^ self
		deny: expected == actual
		description: [self unexpectedIdentityEqualityStringBetween: actual and: expected]