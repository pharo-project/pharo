validation
validateSharedVariables: allVariables
	| resultVariables |
	resultVariables  := result classPool keys.

	self assertCollection: resultVariables equals: allVariables 