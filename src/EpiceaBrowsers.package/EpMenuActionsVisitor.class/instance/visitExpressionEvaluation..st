visitor
visitExpressionEvaluation: anExpressionEvaluation
	^ (self visitEvent: anExpressionEvaluation)
		add: self redoMenuAction;
		yourself