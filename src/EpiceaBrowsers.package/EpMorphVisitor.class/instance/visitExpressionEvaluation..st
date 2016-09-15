visitor
visitExpressionEvaluation: anExpressionEvaluation

	^ (self displayOperation: 'DoIt:'), (self displayString:
		(anExpressionEvaluation expression printStringLimitedTo: 50) 
			withSeparatorsCompacted)