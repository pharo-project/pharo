tests
testNoTrigger
	| expressionEvaluationWithTrigger |
	expressionEvaluationWithTrigger := self classAddition.
	expressionEvaluationWithTrigger tags 
		at: EpLog triggererReferenceKey 
		put: (OmReference globalName: 'test' localName: '1').
				
	self
		assert: EpPluggableFilter noTriggerFilter
		accepts: self classAddition
		rejects: expressionEvaluationWithTrigger