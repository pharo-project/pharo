private-method
privateRemoveParameterMethodFor: aMethod
	^ RBRemoveParameterRefactoring
		model: environment
		removeParameter: (self chooseFrom: aMethod argumentNames title: 'Choose a parameter to remove:' lines:  aMethod argumentNames)
		in: aMethod origin
		selector: aMethod selector