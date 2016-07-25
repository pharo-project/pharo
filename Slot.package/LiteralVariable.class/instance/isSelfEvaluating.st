testing
isSelfEvaluating
	^ self key isSelfEvaluating and: [self value isSelfEvaluating]