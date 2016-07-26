preconditions
myConditions
	^RBCondition withBlock: [oldSelector numArgs = newSelector numArgs]
		errorString: newSelector printString 
				, ' doesn''t have the correct number of arguments.'