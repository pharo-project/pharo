tests
testEvaluation
	self
		assert: EpPluggableFilter evaluationFilter
		accepts: self expressionEvaluation
		rejects: self classAddition