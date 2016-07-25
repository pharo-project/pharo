well-known filters
evaluationFilter
	^ self
		title: 'evaluation'
		condition: [ :e | e content isKindOf: EpExpressionEvaluation ]