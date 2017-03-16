tests
testNoTrigger
	| expressionEvaluationWithTrigger |
	expressionEvaluationWithTrigger := self expressionEvaluation.
	expressionEvaluationWithTrigger tags 
		at: EpLog triggererReferenceKey 
		put: (OmReference globalName: 'test' localName: '1').
				
	self
		assert: EpPluggableFilter noTriggerFilter
		accepts: self expressionEvaluation
		rejects: expressionEvaluationWithTrigger