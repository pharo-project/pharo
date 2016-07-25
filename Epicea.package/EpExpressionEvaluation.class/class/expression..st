instance creation
expression: anExpression 
	^ self basicNew 
		initializeExpression: anExpression;
		yourself