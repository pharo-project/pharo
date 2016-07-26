private-source
privateMoveVariableDefinitionBetween: anInterval from: aMethod
	^ RBMoveVariableDefinitionRefactoring
		model: environment
		bindTight: anInterval
		in: aMethod methodClass
		selector: aMethod selector