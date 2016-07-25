preconditions
preconditions
	| newCondition |
	newCondition := (RBCondition 
							withBlock: [oldSelector numArgs = newSelector numArgs or: [ newSelector numArgs = 0 ]]
							errorString: 'The new selectors should have the same number of arguments as the old one, or not have them at all').
	^newCondition