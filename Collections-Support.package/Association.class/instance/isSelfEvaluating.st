self evaluating
isSelfEvaluating
	^ self class == Association and: [self key isSelfEvaluating and: [self value isSelfEvaluating]]