preconditions
preconditions
	| newCondition |
	newCondition := (RBCondition 
							withBlock: [newSelector = oldSelector]
							errorString: 'The selectors are <1?:not >equivalent') 
						& (RBCondition 
							withBlock: [permutation asArray ~= (1 to: oldSelector numArgs) asArray]
							errorString: 'The arguments are <1?:not >permuted').
	^newCondition | super preconditions