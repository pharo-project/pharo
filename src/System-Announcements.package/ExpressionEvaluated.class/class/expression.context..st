instance creation
expression: theEvaluatedExpression context: anExecutionContext
	^self new
			expressionEvaluated: theEvaluatedExpression;
			context: anExecutionContext;
			yourself