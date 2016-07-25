self evaluating
isSelfEvaluating
	^ self class == RunArray and: [values isSelfEvaluating]