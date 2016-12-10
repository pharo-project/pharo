accessing
expressionCode
	^ (RBParser parseFaultyExpression: self expression) doSemanticAnalysis; yourself
	