private-method
privateMoveMethodFor: aMethod
	^ RBMoveMethodRefactoring
		model: environment
		selector: aMethod selector
		class: aMethod methodClass
		variable: (self chooseFrom: aMethod methodClass instVarNames title: 'Choose:' lines:  aMethod methodClass instVarNames)